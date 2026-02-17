# =============================================================================
# RJDemetra Forecasting - X13 Equivalent
# =============================================================================
#
# Provides time series forecasting using RJDemetra's RegARIMA model.
# If no spec file is provided, uses defaults matching:
#   - spec_files/forecast-annual.spc    (freq=1)  ARIMA(0,2,2), no calendar
#   - spec_files/forecast-quarterly.spc (freq=4)  automdl, Easter
#   - spec_files/forecast-monthly.spc   (freq=12) automdl, Easter+TD
#
# If a spec file is provided, parses it and creates an equivalent RJDemetra
# RegARIMA specification. No X11 decomposition or Denton benchmarking.
#
# =============================================================================


# =============================================================================
# MATRIX WRAPPER — FORECAST
# =============================================================================
#
# Accepts a matrix (rows = series, cols = date columns) with a target date
# string. Calculates per-series forecast horizons, calls rjd_forecast() for
# each series, and writes forecast values back into the matrix.
#
# @param tsMat              Matrix with rownames = series names, colnames = date labels
# @param date               Target date string ("2030", "2030Q3", "2030May", etc.),
#                           or NULL to use maxlead from spec file
# @param periodicity        Character: "A", "Q", or "M"
# @param spec_names         Character vector of spec file names (same length as
#                           custom_spec_series). NULL if no custom specs.
# @param spec_folder        Path to directory containing spec files
# @param custom_spec_series Character vector of series names (matching rownames)
#                           that use the corresponding spec file from spec_names
# @return Modified matrix with forecast values filled in
# =============================================================================

rjd_forecast_matrix <- function(tsMat, date = NULL, periodicity,
                                 spec_names = NULL, spec_folder = NULL,
                                 custom_spec_series = NULL) {

  freq <- periodicity_to_freq(periodicity)

  # --- Validate target date ---------------------------------------------------
  if (!is.null(date) && nzchar(trimws(date))) {
    target <- parse_target_date(date)
    if (target$frequency != freq) {
      stop(method_error(FORECAST_FREQ_MISMATCH))
    }
  } else {
    # No date supplied — need maxlead from at least one spec file
    # Try the first custom spec to check for maxlead
    has_maxlead <- FALSE
    if (!is.null(spec_names) && length(spec_names) > 0 && !is.null(spec_folder)) {
      first_spec <- file.path(spec_folder, spec_names[1])
      if (file.exists(first_spec)) {
        cfg <- parse_x13_spec(first_spec)
        has_maxlead <- !is.null(cfg$forecast_maxlead)
      }
    }
    if (!has_maxlead) {
      stop(method_error(FORECAST_NO_HORIZON))
    }
    target <- NULL
  }

  # --- Column dates ----------------------------------------------------------
  col_dates <- colnames(tsMat)
  series_names <- rownames(tsMat) %||% as.character(seq_len(nrow(tsMat)))

  # --- Extend matrix columns if target date is beyond current range ----------
  if (!is.null(target)) {
    last_col <- parse_target_date(col_dates[length(col_dates)])
    extend_n <- calculate_periods_to_target(
      last_col$year, last_col$period, target$year, target$period, freq
    )
    if (extend_n > 0) {
      next_year <- last_col$year
      next_period <- last_col$period + 1L
      if (next_period > freq) { next_period <- 1L; next_year <- next_year + 1L }
      new_labels <- generate_date_labels(next_year, next_period, extend_n, freq)
      extension <- matrix(NA_real_, nrow = nrow(tsMat), ncol = extend_n,
                          dimnames = list(series_names, new_labels))
      tsMat <- cbind(tsMat, extension)
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
    ts_end <- end(ts_obj)

    # Calculate horizon
    if (!is.null(target)) {
      h <- calculate_periods_to_target(
        ts_end[1], ts_end[2], target$year, target$period, freq
      )
    } else {
      # No target date — use maxlead from this series' spec file
      if (!is.null(spec_file) && file.exists(spec_file)) {
        cfg <- parse_x13_spec(spec_file)
        h <- cfg$forecast_maxlead %||% 0L
      } else {
        h <- 0L
      }
    }

    if (h <= 0) next

    # Call rjd_forecast
    result <- tryCatch(
      rjd_forecast(ts_obj, horizon = h, spec_file = spec_file),
      error = function(e) list(error = TRUE, messages = e$message)
    )

    if (isTRUE(result$error)) {
      record_ts_error(FORECAST_RJD_FAIL, sname)
      next
    }

    # Write forecast values into the matrix
    write_start <- row_info$end_col + 1L
    write_end   <- write_start + h - 1L
    if (write_end <= ncol(tsMat)) {
      tsMat[i, write_start:write_end] <- result$forecast[seq_len(h)]
    }
  }

  tsMat
}


# =============================================================================
# MAIN ENTRY POINT
# =============================================================================
#
# @param ts_data   Single ts object or named list of ts objects
# @param horizon   Number of periods to forecast (NULL = use default)
# @param spec_file Path to X13 .spc file (NULL = use defaults)
# @param verbose   Print progress for batch processing
# =============================================================================

rjd_forecast <- function(ts_data, horizon = NULL, spec_file = NULL, verbose = FALSE) {

  if (!requireNamespace("RJDemetra", quietly = TRUE)) {
    stop("RJDemetra package is required. Install with: install.packages('RJDemetra')")
  }

  dispatch_ts(
    ts_data,
    single_fn = function(ts) rjd_forecast_single(ts, horizon, spec_file),
    batch_fn = function(ts_list) {
      run_batch(ts_list,
        single_fn = function(ts_obj, parsed_config) {
          rjd_forecast_single(ts_obj, horizon, spec_file, parsed_config)
        },
        batch_class = "rjd_forecast_batch",
        method = "RJDemetra RegARIMA Forecast",
        spec_file = spec_file, verbose = verbose)
    }
  )
}


# =============================================================================
# DEFAULT CONFIGURATION
# =============================================================================

get_forecast_config <- function(freq) {

  if (freq == 1) {
    # Annual: matches forecast-annual.spc - fixed ARIMA(0,2,2), no calendar
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
      forecast_horizon = 3,
      force_enabled = FALSE,
      span_start = NULL, span_end = NULL
    )
  } else if (freq == 4) {
    # Quarterly: matches forecast-quarterly.spc - automdl, Easter
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
      forecast_horizon = 4,
      force_enabled = FALSE,
      span_start = NULL, span_end = NULL
    )
  } else {
    # Monthly: matches forecast-monthly.spc - automdl, Easter+TD
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
      forecast_horizon = 12,
      force_enabled = FALSE,
      span_start = NULL, span_end = NULL
    )
  }
}


# =============================================================================
# SINGLE SERIES - WARNING WRAPPER
# =============================================================================

rjd_forecast_single <- function(ts_data, horizon = NULL, spec_file = NULL,
                                parsed_config = NULL) {
  captured <- run_with_warnings(
    rjd_forecast_impl(ts_data, horizon, spec_file, parsed_config)
  )
  append_warnings(captured$result, captured$warnings)
}


# =============================================================================
# SINGLE SERIES - IMPLEMENTATION
# =============================================================================

rjd_forecast_impl <- function(ts_data, horizon, spec_file, parsed_config) {

  warnings <- character(0)

  # --- Validation ---
  validation <- validate_input(ts_data, "forecast")
  if (!validation$valid) return(list(error = TRUE, messages = validation$message))

  freq <- frequency(ts_data)

  # --- Config resolution ---
  config <- resolve_config(freq, parsed_config, spec_file, get_forecast_config)

  # --- Resolve horizon ---
  if (!is.null(horizon)) {
    config$forecast_horizon <- horizon
  }
  if (is.null(config$forecast_horizon)) {
    config$forecast_horizon <- config$forecast_maxlead %||%
      (if (freq == 1) 3 else if (freq == 4) 4 else 12)
  }
  if (config$forecast_horizon < 1) {
    return(list(error = TRUE,
                messages = sprintf("ERROR: Forecast horizon must be >= 1, got %d",
                                   config$forecast_horizon)))
  }

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

  # --- Create RegARIMA spec ---
  spec <- tryCatch(
    create_regarima_spec(config, ts_filtered, transform_fun = transform_applied),
    error = function(e) e
  )
  if (inherits(spec, "error")) {
    return(list(error = TRUE, messages = sprintf("ERROR: Failed to create RJDemetra spec: %s", spec$message)))
  }

  # --- Run RegARIMA ---
  regarima_result <- tryCatch(
    RJDemetra::regarima(ts_filtered, spec = spec),
    error = function(e) e
  )
  if (inherits(regarima_result, "error")) {
    return(list(error = TRUE, messages = sprintf("ERROR: RJDemetra RegARIMA failed: %s", regarima_result$message)))
  }

  # --- Extract forecasts ---
  h <- config$forecast_horizon
  forecast_values <- tryCatch(
    extract_forecast_values(regarima_result, h),
    error = function(e) {
      warnings <<- c(warnings, sprintf("Forecast extraction failed: %s", e$message))
      rep(NA_real_, h)
    }
  )

  # Ensure correct length
  if (length(forecast_values) < h) {
    forecast_values <- c(forecast_values, rep(NA_real_, h - length(forecast_values)))
  } else if (length(forecast_values) > h) {
    forecast_values <- forecast_values[seq_len(h)]
  }

  # --- Extract model info and diagnostics ---
  arima_info   <- extract_arima_info(regarima_result)
  outlier_info <- extract_outlier_info(regarima_result)
  diagnostics  <- extract_regarima_diagnostics(regarima_result)

  # --- Calculate forecast start date ---
  end_date <- end(ts_filtered)
  forecast_start <- if (end_date[2] == freq) {
    c(end_date[1] + 1, 1)
  } else {
    c(end_date[1], end_date[2] + 1)
  }

  # --- Build result ---
  list(
    error = FALSE,
    messages = build_result_messages(warnings, diagnostics),
    forecast = forecast_values,
    forecast_ts = ts(forecast_values, start = forecast_start, frequency = freq),
    horizon = h,
    original = ts_filtered,
    arima = arima_info,
    outliers = outlier_info,
    transform = transform_applied,
    diagnostics = diagnostics,
    config = config,
    regarima_result = regarima_result,
    method = "RJDemetra RegARIMA Forecast"
  )
}


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Extract forecasts from batch results
extract_forecasts <- function(results, as_ts = FALSE) {
  if (!inherits(results, "rjd_forecast_batch")) {
    if (results$error) return(NA)
    return(if (as_ts) results$forecast_ts else results$forecast)
  }

  fcst_list <- list()
  for (name in names(results)) {
    r <- results[[name]]
    if (!r$error) {
      fcst_list[[name]] <- if (as_ts) r$forecast_ts else r$forecast
    }
  }
  fcst_list
}


#' Print method for forecast batch results
print.rjd_forecast_batch <- function(x, ...) {
  summary <- attr(x, "summary")
  cat(sprintf("RJDemetra Forecast Batch Results\n"))
  cat(sprintf("  Series: %d total, %d success, %d failed\n",
              summary$n_series, summary$n_success, summary$n_failed))
  invisible(x)
}
