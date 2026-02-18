# =============================================================================
# RJDemetra Moving Average Preprocessing - X13 Equivalent
# =============================================================================
#
# Extends time series at both ends using forecast and backcast to prepare
# data for moving average calculations. This prevents data loss at series
# boundaries when applying moving average filters.
#
# MAVE does NOT accept custom spec files. It always uses built-in defaults
# matching:
#   - spec_files/mave_annual.spc     (freq=1)  ARIMA(0,2,2), no calendar
#   - spec_files/mave_quarterly.spc  (freq=4)  automdl, Easter
#   - spec_files/mave_monthly.spc    (freq=12) automdl, Easter+TD
#
# The function:
#   1. Forecasts the series by 'extend' periods
#   2. Backcasts the series by 'extend' periods (via time-reversal)
#   3. Returns the extended series: [backcast | original | forecast]
#
# =============================================================================


# =============================================================================
# MATRIX WRAPPER — MAVE (Moving Average Extension)
# =============================================================================
#
# Accepts a matrix (rows = series, cols = date columns) and extends each
# series at both ends using forecast and backcast values. This prepares
# data for moving average calculations without data loss at boundaries.
#
# Horizon determination (per series):
#   - If start_date / end_date are provided: compute per-series horizons
#     from those target dates to each series' actual start/end.
#   - If extend is provided: use that fixed amount for both ends.
#   - At least one of (start_date, end_date, extend) must be given.
#
# MAVE does NOT accept spec files — it always uses built-in defaults.
#
# @param tsMat       Matrix with rownames = series names, colnames = date labels
# @param start_date  Target date for backcast direction (prepend), or NULL
# @param end_date    Target date for forecast direction (append), or NULL
# @param periodicity Character: "A", "Q", or "M"
# @param extend      Fixed number of periods to extend each end (fallback)
# @return Modified matrix with extended values filled in
# =============================================================================

rjd_mave_matrix <- function(tsMat, start_date = NULL, end_date = NULL,
                             periodicity, extend = NULL) {

  freq <- periodicity_to_freq(periodicity)

  # --- Validate target dates if provided -------------------------------------
  start_target <- NULL
  end_target   <- NULL

  if (!is.null(end_date) && nzchar(trimws(end_date))) {
    end_target <- parse_target_date(end_date)
    if (end_target$frequency != freq) {
      stop(method_error(MAVE_FREQ_MISMATCH))
    }
  }
  if (!is.null(start_date) && nzchar(trimws(start_date))) {
    start_target <- parse_target_date(start_date)
    if (start_target$frequency != freq) {
      stop(method_error(MAVE_FREQ_MISMATCH))
    }
  }

  # Need at least one way to determine extension
  if (is.null(start_target) && is.null(end_target) && is.null(extend)) {
    stop(method_error(MAVE_NO_HORIZON))
  }

  # --- Column dates ----------------------------------------------------------
  col_dates <- colnames(tsMat)
  series_names <- rownames(tsMat) %||% as.character(seq_len(nrow(tsMat)))

  # --- Extend matrix columns to accommodate new values ------------------------

  # Append columns (forward direction)
  if (!is.null(end_target)) {
    last_col <- parse_target_date(col_dates[length(col_dates)])
    last_col_period <- align_period(last_col, freq)
    extend_fwd <- calculate_periods_to_target(
      last_col$year, last_col_period, end_target$year, end_target$period, freq
    )
  } else if (!is.null(extend)) {
    extend_fwd <- extend
  } else {
    extend_fwd <- 0L
  }
  if (extend_fwd > 0) {
    last_col_parsed <- parse_target_date(col_dates[length(col_dates)])
    next_year <- last_col_parsed$year
    next_period <- align_period(last_col_parsed, freq) + 1L
    if (next_period > freq) { next_period <- 1L; next_year <- next_year + 1L }
    new_labels <- generate_date_labels(next_year, next_period, extend_fwd, freq)
    extension <- matrix(NA_real_, nrow = nrow(tsMat), ncol = extend_fwd,
                        dimnames = list(series_names, new_labels))
    tsMat <- cbind(tsMat, extension)
    col_dates <- colnames(tsMat)
  }

  # Prepend columns (backward direction)
  if (!is.null(start_target)) {
    first_col <- parse_target_date(col_dates[1])
    first_col_period <- align_period(first_col, freq)
    prepend_n <- calculate_periods_to_target(
      start_target$year, start_target$period,
      first_col$year, first_col_period, freq
    )
  } else if (!is.null(extend)) {
    prepend_n <- extend
  } else {
    prepend_n <- 0L
  }
  if (prepend_n > 0) {
    first_col_parsed <- parse_target_date(col_dates[1])
    # Compute the start date for prepended columns
    pre_year <- first_col_parsed$year
    pre_period <- align_period(first_col_parsed, freq) - prepend_n
    while (pre_period < 1) {
      pre_year <- pre_year - 1L
      pre_period <- pre_period + freq
    }
    new_labels <- generate_date_labels(pre_year, pre_period, prepend_n, freq)
    extension <- matrix(NA_real_, nrow = nrow(tsMat), ncol = prepend_n,
                        dimnames = list(series_names, new_labels))
    tsMat <- cbind(extension, tsMat)
    col_dates <- colnames(tsMat)
  }

  # --- Process each series (row) ---------------------------------------------
  for (i in seq_len(nrow(tsMat))) {
    sname <- series_names[i]

    row_info <- matrix_row_to_ts(tsMat[i, ], col_dates, freq)
    if (is.null(row_info)) next

    ts_obj   <- row_info$ts_obj
    ts_start <- start(ts_obj)
    ts_end   <- end(ts_obj)

    # Calculate per-series horizons for each direction
    forward_h <- 0L
    if (!is.null(end_target)) {
      forward_h <- calculate_periods_to_target(
        ts_end[1], ts_end[2], end_target$year, end_target$period, freq
      )
    } else if (!is.null(extend)) {
      forward_h <- extend
    }

    backward_h <- 0L
    if (!is.null(start_target)) {
      backward_h <- calculate_periods_to_target(
        start_target$year, start_target$period, ts_start[1], ts_start[2], freq
      )
    } else if (!is.null(extend)) {
      backward_h <- extend
    }

    # Need at least one direction to extend
    effective_extend <- max(forward_h, backward_h, 0L)
    if (effective_extend <= 0) next

    # Call rjd_mave with the larger of the two horizons
    result <- tryCatch(
      rjd_mave(ts_obj, extend = effective_extend),
      error = function(e) list(error = TRUE, messages = e$message)
    )

    if (isTRUE(result$error)) {
      record_ts_error(MAVE_RJD_FAIL, sname)
      next
    }

    # Write forecast values (forward extension)
    if (forward_h > 0) {
      write_start <- row_info$end_col + 1L
      write_end   <- write_start + forward_h - 1L
      if (write_end <= ncol(tsMat)) {
        # Take the first forward_h values from the forecast portion
        tsMat[i, write_start:write_end] <- result$forecast[seq_len(forward_h)]
      }
    }

    # Write backcast values (backward extension)
    if (backward_h > 0) {
      write_end   <- row_info$start_col - 1L
      write_start <- write_end - backward_h + 1L
      if (write_start >= 1) {
        # Take the last backward_h values from the backcast portion
        bc <- result$backcast
        bc_len <- length(bc)
        tsMat[i, write_start:write_end] <- bc[(bc_len - backward_h + 1L):bc_len]
      }
    }
  }

  tsMat
}


# =============================================================================
# DEFAULT CONFIGURATION
# =============================================================================
#
# Returns settings matching the mave spec files.
# Annual uses fixed ARIMA(0,2,2), no calendar effects.
# Quarterly uses automdl with Easter.
# Monthly uses automdl with Easter and trading days.
# =============================================================================

get_mave_config <- function(freq) {

  if (freq == 1) {
    # Annual: matches mave_annual.spc / mave_financial.spc
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
      default_extend = 3,
      force_enabled = FALSE,
      span_start = NULL, span_end = NULL
    )
  } else if (freq == 4) {
    # Quarterly: matches mave_quarterly.spc
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
      default_extend = 4,
      force_enabled = FALSE,
      span_start = NULL, span_end = NULL
    )
  } else {
    # Monthly: matches mave_monthly.spc
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
      default_extend = 12,
      force_enabled = FALSE,
      span_start = NULL, span_end = NULL
    )
  }
}


# =============================================================================
# MAIN ENTRY POINT
# =============================================================================
#
# @param ts_data  Single ts object or named list of ts objects
# @param extend   Number of periods to extend at each end (NULL = use default:
#                 3 for annual, 4 for quarterly, 12 for monthly)
# @param verbose  Print progress for batch processing
#
# @return For single ts: result list with extended series
#         For list of ts: batch result object
# =============================================================================

rjd_mave <- function(ts_data, extend = NULL, verbose = FALSE) {

  if (!requireNamespace("RJDemetra", quietly = TRUE)) {
    stop("RJDemetra package is required. Install with: install.packages('RJDemetra')")
  }

  dispatch_ts(
    ts_data,
    single_fn = function(ts) rjd_mave_single(ts, extend),
    batch_fn = function(ts_list) {
      run_batch(ts_list,
        single_fn = function(ts_obj, parsed_config) {
          rjd_mave_single(ts_obj, extend)
        },
        batch_class = "rjd_mave_batch",
        method = "RJDemetra MAVE Preprocessing",
        verbose = verbose)
    }
  )
}


# =============================================================================
# SINGLE SERIES - WARNING WRAPPER
# =============================================================================

rjd_mave_single <- function(ts_data, extend = NULL) {
  captured <- run_with_warnings(
    rjd_mave_impl(ts_data, extend)
  )
  append_warnings(captured$result, captured$warnings)
}


# =============================================================================
# SINGLE SERIES - IMPLEMENTATION
# =============================================================================

rjd_mave_impl <- function(ts_data, extend) {

  warnings <- character(0)

  # --- Validation ---
  validation <- validate_input(ts_data, "mave")
  if (!validation$valid) return(list(error = TRUE, messages = validation$message))

  freq <- frequency(ts_data)

  # --- Config (always defaults, no spec file) ---
  config <- get_mave_config(freq)

  # --- Determine extend periods ---
  extend_periods <- extend %||% config$default_extend
  if (extend_periods < 1) {
    return(list(error = TRUE,
                messages = sprintf("ERROR: Extend periods must be >= 1, got %d",
                                   extend_periods)))
  }
  config$forecast_horizon <- extend_periods

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

  # =========================================================================
  # FORECASTING PHASE
  # =========================================================================
  forecast_spec <- tryCatch(
    create_regarima_spec(config, ts_filtered, transform_fun = transform_applied),
    error = function(e) e
  )
  if (inherits(forecast_spec, "error")) {
    return(list(error = TRUE, messages = sprintf("ERROR: Failed to create forecast spec: %s", forecast_spec$message)))
  }

  forecast_result <- tryCatch(
    RJDemetra::regarima(ts_filtered, spec = forecast_spec),
    error = function(e) e
  )
  if (inherits(forecast_result, "error")) {
    return(list(error = TRUE, messages = sprintf("ERROR: RJDemetra forecast failed: %s", forecast_result$message)))
  }

  forecast_values <- tryCatch(
    extract_forecast_values(forecast_result, extend_periods),
    error = function(e) {
      warnings <<- c(warnings, sprintf("Forecast extraction failed: %s", e$message))
      rep(NA_real_, extend_periods)
    }
  )

  # Ensure correct length
  if (length(forecast_values) < extend_periods) {
    forecast_values <- c(forecast_values, rep(NA_real_, extend_periods - length(forecast_values)))
  } else if (length(forecast_values) > extend_periods) {
    forecast_values <- forecast_values[seq_len(extend_periods)]
  }

  # =========================================================================
  # BACKCASTING PHASE (time-reversal)
  # =========================================================================
  ts_reversed <- ts(rev(as.numeric(ts_filtered)),
                    start = start(ts_filtered), frequency = freq)

  backcast_spec <- tryCatch(
    create_regarima_spec(config, ts_reversed,
                         transform_fun = transform_applied,
                         disable_calendar = TRUE),
    error = function(e) e
  )
  if (inherits(backcast_spec, "error")) {
    return(list(error = TRUE, messages = sprintf("ERROR: Failed to create backcast spec: %s", backcast_spec$message)))
  }

  backcast_result <- tryCatch(
    RJDemetra::regarima(ts_reversed, spec = backcast_spec),
    error = function(e) e
  )
  if (inherits(backcast_result, "error")) {
    return(list(error = TRUE, messages = sprintf("ERROR: RJDemetra backcast failed: %s", backcast_result$message)))
  }

  backcast_values <- tryCatch(
    rev(extract_forecast_values(backcast_result, extend_periods)),
    error = function(e) {
      warnings <<- c(warnings, sprintf("Backcast extraction failed: %s", e$message))
      rep(NA_real_, extend_periods)
    }
  )

  # Ensure correct length
  if (length(backcast_values) < extend_periods) {
    backcast_values <- c(rep(NA_real_, extend_periods - length(backcast_values)), backcast_values)
  } else if (length(backcast_values) > extend_periods) {
    backcast_values <- backcast_values[(length(backcast_values) - extend_periods + 1):length(backcast_values)]
  }

  # =========================================================================
  # COMBINE: [backcast | original | forecast]
  # =========================================================================
  original_values <- as.numeric(ts_filtered)
  extended_values <- c(backcast_values, original_values, forecast_values)

  # Calculate start date for extended series
  orig_start <- start(ts_filtered)
  ext_start_year <- orig_start[1]
  ext_start_period <- orig_start[2] - extend_periods
  while (ext_start_period < 1) {
    ext_start_year <- ext_start_year - 1
    ext_start_period <- ext_start_period + freq
  }
  extended_ts <- ts(extended_values,
                    start = c(ext_start_year, ext_start_period),
                    frequency = freq)

  # --- Extract model info and diagnostics (from forecast model) ---
  arima_info  <- extract_arima_info(forecast_result)
  diagnostics <- extract_regarima_diagnostics(forecast_result)

  # --- Build result ---
  list(
    error = FALSE,
    messages = build_result_messages(warnings, diagnostics),
    extended = extended_values,
    extended_ts = extended_ts,
    original = ts_filtered,
    forecast = forecast_values,
    backcast = backcast_values,
    extend = extend_periods,
    arima = arima_info,
    transform = transform_applied,
    diagnostics = diagnostics,
    config = config,
    method = "RJDemetra MAVE Preprocessing"
  )
}


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Extract extended series from batch results
extract_extended <- function(results, as_ts = FALSE) {
  if (!inherits(results, "rjd_mave_batch")) {
    if (results$error) return(NA)
    return(if (as_ts) results$extended_ts else results$extended)
  }

  ext_list <- list()
  for (name in names(results)) {
    r <- results[[name]]
    if (!r$error) {
      ext_list[[name]] <- if (as_ts) r$extended_ts else r$extended
    }
  }
  ext_list
}


#' Print method for mave batch results
print.rjd_mave_batch <- function(x, ...) {
  summary <- attr(x, "summary")
  cat(sprintf("RJDemetra MAVE Preprocessing Batch Results\n"))
  cat(sprintf("  Series: %d total, %d success, %d failed\n",
              summary$n_series, summary$n_success, summary$n_failed))
  invisible(x)
}
