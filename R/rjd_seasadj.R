# =============================================================================
# RJDemetra Seasonal Adjustment - X13 Equivalent
# =============================================================================
#
# Performs seasonal adjustment using RJDemetra with X13-style spec file support.
# If no spec file is provided, uses defaults matching:
#   - spec_files/seasadj_quarterly.spc (freq=4)
#   - spec_files/seasadj_monthly.spc (freq=12)
#
# If a spec file is provided, parses it and creates an equivalent RJDemetra
# specification. Results will NOT match X13 command line exactly due to
# differences in ARIMA model selection and numerical algorithms.
#
# Dependencies:
#   Required: RJDemetra (>= 0.2.0)
#   Optional: tempdisagg (for Denton benchmarking, with fallback if missing)
#
# =============================================================================


# =============================================================================
# MATRIX WRAPPER — SEASONAL ADJUSTMENT
# =============================================================================
#
# Accepts a matrix (rows = series, cols = date columns) and replaces each
# series' values with their seasonally adjusted counterparts in place.
# Seasonal adjustment only supports quarterly (freq=4) and monthly (freq=12).
#
# @param tsMat              Matrix with rownames = series names, colnames = date labels
# @param periodicity        Character: "Q" or "M" (annual not supported for seasadj)
# @param spec_names         Character vector of spec file names (same length as
#                           custom_spec_series). NULL if no custom specs.
# @param spec_folder        Path to directory containing spec files
# @param custom_spec_series Character vector of series names (matching rownames)
#                           that use the corresponding spec file from spec_names
# @param apply_denton       Apply Denton benchmarking if configured (default TRUE)
# @return Modified matrix with SA values replacing the original data
# =============================================================================

rjd_seasadj_matrix <- function(tsMat, periodicity,
                                spec_names = NULL, spec_folder = NULL,
                                custom_spec_series = NULL,
                                apply_denton = TRUE) {

  freq <- periodicity_to_freq(periodicity)

  # Seasonal adjustment only supports quarterly and monthly
  if (!freq %in% c(4L, 12L)) {
    stop(method_error(SEASADJ_FREQ_MISMATCH))
  }

  col_dates <- colnames(tsMat)
  series_names <- rownames(tsMat) %||% as.character(seq_len(nrow(tsMat)))

  for (i in seq_len(nrow(tsMat))) {
    sname <- series_names[i]

    # Resolve per-series spec file
    spec_file <- resolve_series_spec(sname, spec_names, spec_folder,
                                      custom_spec_series)

    row_info <- matrix_row_to_ts(tsMat[i, ], col_dates, freq)
    if (is.null(row_info)) next

    result <- tryCatch(
      rjd_seasadj(row_info$ts_obj, spec_file = spec_file,
                   apply_denton = apply_denton),
      error = function(e) list(error = TRUE, messages = e$message)
    )

    if (isTRUE(result$error)) {
      record_ts_error(SEASADJ_RJD_FAIL, sname)
      next
    }

    # Write SA values back over the original positions
    tsMat[i, row_info$start_col:row_info$end_col] <- result$sa
  }

  tsMat
}


# =============================================================================
# DEFAULT CONFIGURATION
# =============================================================================
#
# Returns settings matching the standard spec files:
#   - seasadj_quarterly.spc: pickmdl, Easter, AO+LS, Denton, X11 mult
#   - seasadj_monthly.spc:   pickmdl, Easter+TD, AO+LS, Denton, X11 mult
# =============================================================================

get_default_config <- function(freq) {

  if (freq == 4) {
    trading_days <- FALSE
    forecast_maxlead <- 4
  } else {
    trading_days <- TRUE
    forecast_maxlead <- 12
  }

  list(
    periodicity = freq,
    transform = "auto",
    arima_auto = TRUE,
    arima_order = NULL,
    seasonal_order = NULL,
    outlier_enabled = TRUE,
    outlier_ao = TRUE,
    outlier_ls = TRUE,
    outlier_tc = FALSE,
    outlier_cv = NULL,
    user_outliers = list(),
    trading_days = trading_days,
    easter = TRUE,
    easter_duration = 8,
    x11_mode = "mult",
    x11_seasonalma = "msr",
    x11_trendma = NULL,
    x11_type = "sa",
    forecast_maxlead = forecast_maxlead,
    force_enabled = TRUE,
    force_rho = 1,
    force_lambda = 1,
    force_mode = "ratio",
    span_start = NULL,
    span_end = NULL
  )
}


# =============================================================================
# MAIN ENTRY POINT
# =============================================================================
#
# @param ts_data     Single ts object or named list of ts objects
# @param spec_file   Path to X13 .spc file (NULL = use defaults)
# @param apply_denton Apply Denton benchmarking if configured (default TRUE)
# @param verbose     Print progress messages for batch processing
#
# @return For single ts: result list with SA values and diagnostics
#         For list of ts: batch result object
# =============================================================================

rjd_seasadj <- function(ts_data, spec_file = NULL, apply_denton = TRUE, verbose = FALSE) {

  if (!requireNamespace("RJDemetra", quietly = TRUE)) {
    stop("RJDemetra package is required. Install with: install.packages('RJDemetra')")
  }

  dispatch_ts(
    ts_data,
    single_fn = function(ts) rjd_seasadj_single(ts, spec_file, apply_denton),
    batch_fn = function(ts_list) {
      run_batch(ts_list,
        single_fn = function(ts_obj, parsed_config) {
          rjd_seasadj_single(ts_obj, spec_file, apply_denton, parsed_config)
        },
        batch_class = "rjd_seasadj_batch",
        method = "RJDemetra (JDemetra+ implementation)",
        spec_file = spec_file, verbose = verbose)
    }
  )
}


# =============================================================================
# SINGLE SERIES - WARNING WRAPPER
# =============================================================================

rjd_seasadj_single <- function(ts_data, spec_file = NULL, apply_denton = TRUE,
                                parsed_config = NULL) {
  captured <- run_with_warnings(
    rjd_seasadj_impl(ts_data, spec_file, apply_denton, parsed_config)
  )
  append_warnings(captured$result, captured$warnings)
}


# =============================================================================
# SINGLE SERIES - IMPLEMENTATION
# =============================================================================

rjd_seasadj_impl <- function(ts_data, spec_file, apply_denton, parsed_config) {

  warnings <- character(0)

  # --- Validation ---
  validation <- validate_input(ts_data, "seasadj")
  if (!validation$valid) return(list(error = TRUE, messages = validation$message))

  freq <- frequency(ts_data)

  # --- Config resolution ---
  config <- resolve_config(freq, parsed_config, spec_file, get_default_config)

  # --- Span filter ---
  span_result <- apply_span_filter(ts_data, config)
  ts_filtered <- span_result$ts
  warnings <- c(warnings, span_result$warnings)

  # --- Handle type=summary (no decomposition) ---
  if ((config$x11_type %||% "sa") == "summary") {
    sa_values <- as.numeric(ts_filtered)
    denton_applied <- FALSE

    if (apply_denton && config$force_enabled) {
      denton_result <- apply_denton_benchmarking(sa_values, ts_filtered, config)
      sa_values <- denton_result$values
      denton_applied <- denton_result$applied
    }

    return(list(
      error = FALSE,
      messages = build_result_messages(warnings, list()),
      sa = sa_values,
      ts = ts(sa_values, start = start(ts_filtered), frequency = freq),
      original = ts_filtered,
      trend = NULL, seasonal = NULL, irregular = NULL,
      arima = list(order = NULL, seasonal = NULL),
      outliers = list(types = character(0), dates = character(0)),
      transform = "None",
      x11_mode = "summary",
      denton_applied = denton_applied,
      config = config,
      method = "RJDemetra (no decomposition - type=summary)"
    ))
  }

  # --- Resolve transform (once, for both spec and result) ---
  transform_applied <- resolve_transform(config$transform, ts_filtered)

  # --- Create RJDemetra X13 specification ---
  spec <- tryCatch(
    create_rjdemetra_spec(config, ts_filtered, transform_fun = transform_applied),
    error = function(e) e
  )
  if (inherits(spec, "error")) {
    return(list(error = TRUE, messages = sprintf("ERROR: Failed to create RJDemetra spec: %s", spec$message)))
  }

  # --- Run X13 decomposition ---
  result <- tryCatch(
    RJDemetra::x13(ts_filtered, spec = spec),
    error = function(e) e
  )
  if (inherits(result, "error")) {
    return(list(error = TRUE, messages = sprintf("ERROR: RJDemetra X13 failed: %s", result$message)))
  }

  # --- Extract components ---
  sa_raw   <- as.numeric(result$final$series[, "sa"])
  trend    <- as.numeric(result$final$series[, "t"])
  seasonal <- as.numeric(result$final$series[, "s"])
  irregular <- as.numeric(result$final$series[, "i"])

  # --- Extract model info ---
  arima_info  <- extract_arima_info(result, is_x13 = TRUE)
  outlier_info <- extract_outlier_info(result, is_x13 = TRUE)
  diagnostics <- extract_x13_diagnostics(result)

  # --- Denton benchmarking ---
  if (apply_denton && config$force_enabled && config$force_mode == "ratio") {
    denton_result <- apply_denton_benchmarking(sa_raw, ts_filtered, config)
    sa_final <- denton_result$values
    denton_applied <- denton_result$applied
  } else {
    sa_final <- sa_raw
    denton_applied <- FALSE
  }

  # --- Build result ---
  list(
    error = FALSE,
    messages = build_result_messages(warnings, diagnostics),
    sa = sa_final,
    sa_raw = sa_raw,
    ts = ts(sa_final, start = start(ts_filtered), frequency = freq),
    original = ts_filtered,
    trend = trend,
    seasonal = seasonal,
    irregular = irregular,
    arima = arima_info,
    outliers = outlier_info,
    transform = transform_applied,
    x11_mode = config$x11_mode,
    denton_applied = denton_applied,
    diagnostics = diagnostics,
    config = config,
    rjdemetra_result = result,
    method = "RJDemetra (JDemetra+ implementation)"
  )
}


# =============================================================================
# DENTON BENCHMARKING
# =============================================================================
#
# Adjusts SA series so annual totals match original unadjusted series.
# Primary: tempdisagg package (Denton-Cholette method)
# Fallback: Simple proportional distribution
# =============================================================================

apply_denton_benchmarking <- function(sa_values, original_ts, config) {

  freq <- frequency(original_ts)
  start_info <- start(original_ts)
  original <- as.numeric(original_ts)

  sa_ts <- ts(sa_values, start = start_info, frequency = freq)
  annual_totals <- aggregate(original_ts, nfrequency = 1, FUN = sum)

  if (length(annual_totals) < 1) {
    return(list(values = sa_values, applied = FALSE))
  }

  # Try tempdisagg (preferred)
  if (requireNamespace("tempdisagg", quietly = TRUE)) {
    benchmarked <- tryCatch({
      td_result <- tempdisagg::td(
        annual_totals ~ 0 + sa_ts,
        method = "denton-cholette",
        conversion = "sum")
      as.numeric(predict(td_result))
    }, error = function(e) {
      warning(sprintf("tempdisagg Denton failed: %s. Using fallback.", e$message))
      NULL
    })

    if (!is.null(benchmarked) && length(benchmarked) == length(sa_values)) {
      return(list(values = benchmarked, applied = !identical(benchmarked, sa_values)))
    }
  } else {
    warning("tempdisagg package not installed. Using fallback Denton implementation.")
  }

  # Fallback: proportional distribution
  benchmarked <- tryCatch(
    denton_pfd_fallback(sa_values, original, freq, start_info),
    error = function(e) {
      warning(sprintf("Fallback Denton failed: %s", e$message))
      sa_values
    }
  )

  list(values = benchmarked, applied = !identical(benchmarked, sa_values))
}


denton_pfd_fallback <- function(sa_values, original, freq, start_info) {
  n <- length(sa_values)
  start_year <- start_info[1]
  start_period <- start_info[2]

  years <- start_year + ((seq_len(n) - 1 + start_period - 1) %/% freq)
  annual_orig <- tapply(original, years, sum)
  annual_sa <- tapply(sa_values, years, sum)

  adj_ratios <- annual_orig / annual_sa
  adj_ratios[!is.finite(adj_ratios)] <- 1

  result <- sa_values
  unique_years <- unique(years)
  for (yr in unique_years) {
    mask <- years == yr
    yr_idx <- which(names(adj_ratios) == as.character(yr))
    if (length(yr_idx) > 0) {
      result[mask] <- sa_values[mask] * adj_ratios[yr_idx]
    }
  }
  result
}


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Extract SA series from single or batch results
extract_sa <- function(results, as_ts = FALSE, na_for_errors = TRUE) {
  if (!inherits(results, "rjd_seasadj_batch")) {
    if (results$error) return(if (na_for_errors) NA else NULL)
    return(if (as_ts) results$ts else results$sa)
  }

  sa_list <- list()
  for (name in names(results)) {
    r <- results[[name]]
    if (r$error) {
      if (na_for_errors) sa_list[[name]] <- NA
    } else {
      sa_list[[name]] <- if (as_ts) r$ts else r$sa
    }
  }
  sa_list
}


#' Batch summary data frame
batch_summary <- function(results) {
  if (!inherits(results, "rjd_seasadj_batch")) {
    stop("Input must be batch results from rjd_seasadj()")
  }

  rows <- lapply(names(results), function(name) {
    r <- results[[name]]
    if (r$error) {
      data.frame(
        series = name, success = FALSE, n = NA_integer_,
        transform = NA_character_, arima = NA_character_,
        n_outliers = NA_integer_, denton = NA,
        has_messages = nchar(r$messages %||% "") > 0,
        stringsAsFactors = FALSE)
    } else {
      arima_str <- sprintf("(%d,%d,%d)(%d,%d,%d)",
        r$arima$order[1], r$arima$order[2], r$arima$order[3],
        r$arima$seasonal[1], r$arima$seasonal[2], r$arima$seasonal[3])
      data.frame(
        series = name, success = TRUE, n = length(r$sa),
        transform = r$transform, arima = arima_str,
        n_outliers = length(r$outliers$types), denton = r$denton_applied,
        has_messages = nchar(r$messages %||% "") > 0,
        stringsAsFactors = FALSE)
    }
  })
  do.call(rbind, rows)
}


#' Get messages from results
get_messages <- function(results) {
  if (inherits(results, "rjd_seasadj_batch")) {
    return(attr(results, "messages") %||% "")
  }
  results$messages %||% ""
}


#' Check for messages
has_messages <- function(results) {
  nchar(get_messages(results)) > 0
}


#' Print method for batch results
print.rjd_seasadj_batch <- function(x, ...) {
  summary <- attr(x, "summary")
  cat(sprintf("RJDemetra Seasonal Adjustment Batch Results\n"))
  cat(sprintf("  Series: %d total, %d success, %d failed\n",
              summary$n_series, summary$n_success, summary$n_failed))
  if (!is.null(summary$spec_file)) {
    cat(sprintf("  Spec: %s\n", summary$spec_file))
  } else {
    cat("  Spec: defaults\n")
  }
  invisible(x)
}
