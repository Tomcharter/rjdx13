# =============================================================================
# RJDemetra Backcasting - X13 Equivalent
# =============================================================================
#
# Provides time series backcasting using RJDemetra's RegARIMA model.
# Uses time-reversal approach: reverse series, forecast, reverse results.
#
# If no spec file is provided, uses defaults matching:
#   - spec_files/backcast-annual.spc    (freq=1)  ARIMA(0,2,2), no calendar
#   - spec_files/backcast-quarterly.spc (freq=4)  automdl, Easter
#   - spec_files/backcast-monthly.spc   (freq=12) automdl, Easter+TD
#
# If a spec file is provided, parses it and creates an equivalent RJDemetra
# RegARIMA specification. Calendar effects are disabled for the reversed series.
#
# =============================================================================


# =============================================================================
# MATRIX WRAPPER — BACKCAST
# =============================================================================
#
# Accepts a matrix (rows = series, cols = date columns) with a target date
# string. The target date is the start point to backcast to. Calculates
# per-series backcast horizons, calls rjd_backcast() for each series, and
# writes backcast values into the matrix.
#
# @param tsMat              Matrix with rownames = series names, colnames = date labels
# @param date               Target date string ("2020", "2020Q1", "2020Jan", etc.),
#                           or NULL to use maxback from spec file
# @param periodicity        Character: "A", "Q", or "M"
# @param spec_names         Character vector of spec file names (same length as
#                           custom_spec_series). NULL if no custom specs.
# @param spec_folder        Path to directory containing spec files
# @param custom_spec_series Character vector of series names (matching rownames)
#                           that use the corresponding spec file from spec_names
# @return Modified matrix with backcast values filled in
# =============================================================================

rjd_backcast_matrix <- function(tsMat, date = NULL, periodicity,
                                 spec_names = NULL, spec_folder = NULL,
                                 custom_spec_series = NULL) {

  freq <- periodicity_to_freq(periodicity)

  # --- Validate target date ---------------------------------------------------
  if (!is.null(date) && nzchar(trimws(date))) {
    target <- parse_target_date(date)
    if (target$frequency != freq) {
      stop(method_error(BACKCAST_FREQ_MISMATCH))
    }
  } else {
    # No date supplied — need maxback from at least one spec file
    has_maxback <- FALSE
    if (!is.null(spec_names) && length(spec_names) > 0 && !is.null(spec_folder)) {
      first_spec <- file.path(spec_folder, spec_names[1])
      if (file.exists(first_spec)) {
        cfg <- parse_x13_spec(first_spec)
        has_maxback <- !is.null(cfg$forecast_maxback)
      }
    }
    if (!has_maxback) {
      stop(method_error(BACKCAST_NO_HORIZON))
    }
    target <- NULL
  }

  # --- Column dates ----------------------------------------------------------
  col_dates <- colnames(tsMat)
  series_names <- rownames(tsMat) %||% as.character(seq_len(nrow(tsMat)))

  # --- Prepend matrix columns if target date is before current range ---------
  if (!is.null(target)) {
    first_col <- parse_target_date(col_dates[1])
    first_col_period <- align_period(first_col, freq)
    prepend_n <- calculate_periods_to_target(
      target$year, target$period, first_col$year, first_col_period, freq
    )
    if (prepend_n > 0) {
      new_labels <- generate_date_labels(target$year, target$period, prepend_n, freq)
      extension <- matrix(NA_real_, nrow = nrow(tsMat), ncol = prepend_n,
                          dimnames = list(series_names, new_labels))
      tsMat <- cbind(extension, tsMat)
      col_dates <- colnames(tsMat)
    }
  }

  # --- Process each series (row) ---------------------------------------------
  for (i in seq_len(nrow(tsMat))) {
    sname <- series_names[i]

    # Resolve per-series spec file
    spec_file <- resolve_series_spec(sname, spec_names, spec_folder,
                                      custom_spec_series)

    # Extract ts from non-NA range
    row_info <- matrix_row_to_ts(tsMat[i, ], col_dates, freq)
    if (is.null(row_info)) next

    ts_obj <- row_info$ts_obj
    ts_start <- start(ts_obj)

    # Calculate horizon (periods from target to series start)
    if (!is.null(target)) {
      h <- calculate_periods_to_target(
        target$year, target$period, ts_start[1], ts_start[2], freq
      )
    } else {
      # No target date — use maxback from this series' spec file
      if (!is.null(spec_file) && file.exists(spec_file)) {
        cfg <- parse_x13_spec(spec_file)
        h <- cfg$forecast_maxback %||% 0L
      } else {
        h <- 0L
      }
    }

    if (h <= 0) next

    # Call rjd_backcast
    result <- tryCatch(
      rjd_backcast(ts_obj, horizon = h, spec_file = spec_file),
      error = function(e) list(error = TRUE, messages = e$message)
    )

    if (isTRUE(result$error)) {
      record_ts_error(BACKCAST_RJD_FAIL, sname)
      next
    }

    # Write backcast values into the matrix (before the series start)
    write_end   <- row_info$start_col - 1L
    write_start <- write_end - h + 1L
    if (write_start >= 1) {
      tsMat[i, write_start:write_end] <- result$backcast[seq_len(h)]
    }
  }

  tsMat
}


# =============================================================================
# MAIN ENTRY POINT
# =============================================================================
#
# @param ts_data   Single ts object or named list of ts objects
# @param horizon   Number of periods to backcast (NULL = use default)
# @param spec_file Path to X13 .spc file (NULL = use defaults)
# @param verbose   Print progress for batch processing
# =============================================================================

rjd_backcast <- function(ts_data, horizon = NULL, spec_file = NULL, verbose = FALSE) {

  if (!requireNamespace("RJDemetra", quietly = TRUE)) {
    stop("RJDemetra package is required. Install with: install.packages('RJDemetra')")
  }

  dispatch_ts(
    ts_data,
    single_fn = function(ts) rjd_backcast_single(ts, horizon, spec_file),
    batch_fn = function(ts_list) {
      run_batch(ts_list,
        single_fn = function(ts_obj, parsed_config) {
          rjd_backcast_single(ts_obj, horizon, spec_file, parsed_config)
        },
        batch_class = "rjd_backcast_batch",
        method = "RJDemetra RegARIMA Backcast (time-reversal)",
        spec_file = spec_file, verbose = verbose)
    }
  )
}


# =============================================================================
# DEFAULT CONFIGURATION
# =============================================================================

get_backcast_config <- function(freq) {

  if (freq == 1) {
    # Annual: matches backcast-annual.spc - fixed ARIMA(0,2,2), no calendar
    list(
      periodicity = freq,
      transform = "auto",
      arima_auto = FALSE,
      arima_order = c(0, 2, 2),
      seasonal_order = c(0, 0, 0),
      arima_mean = FALSE,
      outlier_enabled = TRUE,
      outlier_ao = TRUE, outlier_ls = TRUE, outlier_tc = FALSE,
      outlier_cv = NULL, user_outliers = list(),
      trading_days = FALSE, trading_days_type = "TradingDays",
      td_pretest = TRUE, easter_pretest = TRUE,
      easter = FALSE, easter_duration = 8,
      backcast_horizon = 3,
      force_enabled = FALSE,
      span_start = NULL, span_end = NULL
    )
  } else if (freq == 4) {
    # Quarterly: matches backcast-quarterly.spc - automdl, Easter
    list(
      periodicity = freq,
      transform = "auto",
      arima_auto = TRUE,
      arima_order = NULL, seasonal_order = NULL,
      arima_mean = FALSE,
      outlier_enabled = TRUE,
      outlier_ao = TRUE, outlier_ls = TRUE, outlier_tc = FALSE,
      outlier_cv = NULL, user_outliers = list(),
      trading_days = FALSE, trading_days_type = "TradingDays",
      td_pretest = TRUE, easter_pretest = TRUE,
      easter = TRUE, easter_duration = 8,
      backcast_horizon = 4,
      force_enabled = FALSE,
      span_start = NULL, span_end = NULL
    )
  } else {
    # Monthly: matches backcast-monthly.spc - automdl, Easter+TD
    list(
      periodicity = freq,
      transform = "auto",
      arima_auto = TRUE,
      arima_order = NULL, seasonal_order = NULL,
      arima_mean = FALSE,
      outlier_enabled = TRUE,
      outlier_ao = TRUE, outlier_ls = TRUE, outlier_tc = FALSE,
      outlier_cv = NULL, user_outliers = list(),
      trading_days = TRUE, trading_days_type = "TradingDays",
      td_pretest = TRUE, easter_pretest = TRUE,
      easter = TRUE, easter_duration = 8,
      backcast_horizon = 12,
      force_enabled = FALSE,
      span_start = NULL, span_end = NULL
    )
  }
}


# =============================================================================
# SINGLE SERIES - WARNING WRAPPER
# =============================================================================

rjd_backcast_single <- function(ts_data, horizon = NULL, spec_file = NULL,
                                parsed_config = NULL) {
  captured <- run_with_warnings(
    rjd_backcast_impl(ts_data, horizon, spec_file, parsed_config)
  )
  append_warnings(captured$result, captured$warnings)
}


# =============================================================================
# SINGLE SERIES - IMPLEMENTATION
# =============================================================================

rjd_backcast_impl <- function(ts_data, horizon, spec_file, parsed_config) {

  warnings <- character(0)

  # --- Validation ---
  validation <- validate_input(ts_data, "backcast")
  if (!validation$valid) return(list(error = TRUE, messages = validation$message))

  freq <- frequency(ts_data)

  # --- Config resolution ---
  config <- resolve_config(freq, parsed_config, spec_file, get_backcast_config)

  # --- Resolve horizon ---
  if (!is.null(horizon)) {
    config$backcast_horizon <- horizon
  }
  if (is.null(config$backcast_horizon)) {
    config$backcast_horizon <- config$forecast_maxback %||%
      (if (freq == 1) 3 else if (freq == 4) 4 else 12)
  }
  # Validate horizon
  if (config$backcast_horizon < 1) {
    return(list(error = TRUE,
                messages = sprintf("ERROR: Backcast horizon must be >= 1, got %d",
                                   config$backcast_horizon)))
  }
  # RegARIMA spec uses forecast_horizon for the number of forecasts on the
  # reversed series
  config$forecast_horizon <- config$backcast_horizon

  # --- Span filter ---
  span_result <- apply_span_filter(ts_data, config)
  ts_filtered <- span_result$ts
  warnings <- c(warnings, span_result$warnings)

  # --- Log transform check ---
  log_check <- check_log_transform(ts_filtered, config)
  if (log_check$error) return(list(error = TRUE, messages = log_check$message))
  config <- log_check$config
  warnings <- c(warnings, log_check$warnings)

  # --- Resolve transform ---
  transform_applied <- resolve_transform(config$transform, ts_filtered)

  # --- Time reversal ---
  ts_reversed <- ts(rev(as.numeric(ts_filtered)),
                    start = start(ts_filtered), frequency = freq)

  # --- Create RegARIMA spec (calendar disabled for reversed series) ---
  spec <- tryCatch(
    create_regarima_spec(config, ts_reversed,
                         transform_fun = transform_applied,
                         disable_calendar = TRUE),
    error = function(e) e
  )
  if (inherits(spec, "error")) {
    return(list(error = TRUE, messages = sprintf("ERROR: Failed to create RJDemetra spec: %s", spec$message)))
  }

  # --- Run RegARIMA on reversed series ---
  regarima_result <- tryCatch(
    RJDemetra::regarima(ts_reversed, spec = spec),
    error = function(e) e
  )
  if (inherits(regarima_result, "error")) {
    return(list(error = TRUE, messages = sprintf("ERROR: RJDemetra RegARIMA failed: %s", regarima_result$message)))
  }

  # --- Extract and reverse forecasts to get backcasts ---
  h <- config$backcast_horizon
  backcast_values <- tryCatch(
    rev(extract_forecast_values(regarima_result, h)),
    error = function(e) {
      warnings <<- c(warnings, sprintf("Backcast extraction failed: %s", e$message))
      rep(NA_real_, h)
    }
  )

  # Ensure correct length
  if (length(backcast_values) < h) {
    backcast_values <- c(rep(NA_real_, h - length(backcast_values)), backcast_values)
  } else if (length(backcast_values) > h) {
    backcast_values <- backcast_values[(length(backcast_values) - h + 1):length(backcast_values)]
  }

  # --- Extract model info and diagnostics ---
  arima_info   <- extract_arima_info(regarima_result)
  outlier_info <- extract_outlier_info(regarima_result)
  diagnostics  <- extract_regarima_diagnostics(regarima_result)

  # --- Calculate backcast dates ---
  start_date <- start(ts_filtered)
  backcast_end <- if (start_date[2] == 1) {
    c(start_date[1] - 1, freq)
  } else {
    c(start_date[1], start_date[2] - 1)
  }

  backcast_start_year <- backcast_end[1]
  backcast_start_period <- backcast_end[2] - h + 1
  while (backcast_start_period < 1) {
    backcast_start_year <- backcast_start_year - 1
    backcast_start_period <- backcast_start_period + freq
  }
  backcast_start <- c(backcast_start_year, backcast_start_period)

  # --- Build result ---
  list(
    error = FALSE,
    messages = build_result_messages(warnings, diagnostics),
    backcast = backcast_values,
    backcast_ts = ts(backcast_values, start = backcast_start, frequency = freq),
    horizon = h,
    original = ts_filtered,
    arima = arima_info,
    outliers = outlier_info,
    transform = transform_applied,
    diagnostics = diagnostics,
    config = config,
    regarima_result = regarima_result,
    method = "RJDemetra RegARIMA Backcast (time-reversal)"
  )
}


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Extract backcasts from batch results
extract_backcasts <- function(results, as_ts = FALSE) {
  if (!inherits(results, "rjd_backcast_batch")) {
    if (results$error) return(NA)
    return(if (as_ts) results$backcast_ts else results$backcast)
  }

  bcast_list <- list()
  for (name in names(results)) {
    r <- results[[name]]
    if (!r$error) {
      bcast_list[[name]] <- if (as_ts) r$backcast_ts else r$backcast
    }
  }
  bcast_list
}


#' Print method for backcast batch results
print.rjd_backcast_batch <- function(x, ...) {
  summary <- attr(x, "summary")
  cat(sprintf("RJDemetra Backcast Batch Results\n"))
  cat(sprintf("  Series: %d total, %d success, %d failed\n",
              summary$n_series, summary$n_success, summary$n_failed))
  invisible(x)
}
