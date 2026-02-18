# =============================================================================
# RJDemetra Utilities - Shared Functions
# =============================================================================
#
# Common utility functions shared across seasonal adjustment, forecasting,
# backcasting, and MAVE modules:
#   - Null coalescing operator
#   - Input validation
#   - Transform resolution
#   - ARIMA/outlier extraction from RJDemetra results
#   - Span filtering and log transform checks
#   - Warning capture and message building
#   - Batch processing and input dispatch
#   - Diagnostics extraction and formatting
#
# =============================================================================


# -----------------------------------------------------------------------------
# Null-coalescing operator
# Returns 'b' if 'a' is NULL, otherwise returns 'a'
# -----------------------------------------------------------------------------
`%||%` <- function(a, b) if (is.null(a)) b else a


# =============================================================================
# INPUT VALIDATION
# =============================================================================
#
# Validates time series input for all operations.
# Seasonal adjustment requires freq 4 or 12.
# Forecast, backcast, and mave also support freq 1 (annual).
#
# @param ts_data   Time series object to validate
# @param operation "seasadj", "forecast", "backcast", or "mave"
# @return List with: valid (logical), message (error message if invalid)
# =============================================================================

validate_input <- function(ts_data, operation = "seasadj") {

  if (!is.ts(ts_data)) {
    return(list(valid = FALSE,
                message = "ERROR: Input must be a ts (time series) object"))
  }

  freq <- frequency(ts_data)
  supported <- if (operation == "seasadj") c(4, 12) else c(1, 4, 12)

  if (!freq %in% supported) {
    return(list(valid = FALSE,
                message = sprintf(
                  "ERROR: Unsupported frequency %d for %s. Supported: %s",
                  freq, operation, paste(supported, collapse = ", "))))
  }

  n <- length(ts_data)

  # Minimum observations by operation and frequency
  min_obs <- switch(operation,
    "seasadj" = if (freq == 12) 36 else 16,
    # forecast, backcast, mave
    if (freq == 1) 10 else if (freq == 4) 12 else 24
  )

  if (n < min_obs) {
    return(list(valid = FALSE,
                message = sprintf(
                  "ERROR: Insufficient data for %s: %d observations, need at least %d",
                  operation, n, min_obs)))
  }

  # Check for Inf/-Inf values
  n_inf <- sum(is.infinite(ts_data))
  if (n_inf > 0) {
    return(list(valid = FALSE,
                message = sprintf(
                  "ERROR: Series contains %d non-finite (Inf/-Inf) values", n_inf)))
  }

  # Check for excessive NAs (>20%)
  n_na <- sum(is.na(ts_data))
  if (n_na > 0) {
    na_pct <- round(100 * n_na / n, 1)
    if (na_pct > 20) {
      return(list(valid = FALSE,
                  message = sprintf(
                    "ERROR: Too many missing values: %d NAs (%.1f%% of series)",
                    n_na, na_pct)))
    }
  }

  # Check for constant series
  values_clean <- as.numeric(ts_data)[!is.na(as.numeric(ts_data))]
  if (length(unique(values_clean)) == 1) {
    return(list(valid = FALSE,
                message = "ERROR: Series is constant (no variation). Cannot fit model."))
  }

  # Check for near-constant series
  cv <- sd(values_clean, na.rm = TRUE) / abs(mean(values_clean, na.rm = TRUE))
  if (!is.na(cv) && cv < 0.001) {
    return(list(valid = FALSE,
                message = sprintf(
                  "ERROR: Series has extremely low variation (CV=%.4f)", cv)))
  }

  list(valid = TRUE, message = "")
}


# =============================================================================
# TRANSFORM RESOLUTION
# =============================================================================
#
# Resolves transform setting ("auto", "log", "none") to the RJDemetra value
# ("Log" or "None"). Call once and reuse for both spec creation and result
# reporting to ensure consistency.
#
# @param transform_setting "auto", "log", or "none"
# @param ts_obj            Time series (used to check positivity for "auto")
# @return "Log" or "None"
# =============================================================================

resolve_transform <- function(transform_setting, ts_obj) {
  setting <- tolower(transform_setting %||% "auto")
  if (setting == "auto") {
    if (!is.null(ts_obj) && all(ts_obj > 0, na.rm = TRUE)) "Log" else "None"
  } else if (setting == "log") {
    "Log"
  } else {
    "None"
  }
}


# =============================================================================
# FORECAST VALUES EXTRACTION
# =============================================================================
#
# Safely extracts point forecast values from RJDemetra results.
# Handles both vector/ts results and matrix results (where column 1
# contains point forecasts and other columns may contain SEs or CIs).
#
# @param result RJDemetra regarima result object
# @param h      Number of forecast periods requested
# @return Numeric vector of point forecast values
# =============================================================================

extract_forecast_values <- function(result, h) {
  fcst <- result$forecast
  if (is.null(fcst) || length(fcst) == 0) {
    fcst <- tryCatch(predict(result, n.ahead = h), error = function(e) NULL)
  }
  if (is.null(fcst)) return(rep(NA_real_, h))

  # If result is a matrix/data.frame, take first column (point forecasts)
  if (is.matrix(fcst) || is.data.frame(fcst)) {
    fcst <- fcst[, 1]
  }
  as.numeric(fcst)
}


# =============================================================================
# ARIMA INFO EXTRACTION
# =============================================================================
#
# Extracts the fitted ARIMA order from RJDemetra results.
# Tries to get differencing orders (d, D) from the specification object,
# and AR/MA orders (p, q, P, Q) from coefficient names.
#
# @param result RJDemetra result (from x13() or regarima())
# @param is_x13 TRUE if result is from x13() (model nested under $regarima)
# @return List with: order = c(p, d, q), seasonal = c(P, D, Q)
# =============================================================================

extract_arima_info <- function(result, is_x13 = FALSE) {
  ra <- if (is_x13 && !is.null(result$regarima)) result$regarima else result

  # Default: airline model
  order <- c(0, 1, 1)
  seasonal <- c(0, 1, 1)

  tryCatch({
    # Try to get d and D from specification (multiple possible paths)
    for (spec_path in list(ra$specification$arima, ra$spec$arima,
                           ra$specification, ra$spec)) {
      if (!is.null(spec_path)) {
        if (!is.null(spec_path$d))  order[2] <- spec_path$d
        if (!is.null(spec_path$bd)) seasonal[2] <- spec_path$bd
        if (!is.null(spec_path$D))  seasonal[2] <- spec_path$D
        break
      }
    }

    # Get p, q, P, Q from coefficient names
    coefs <- ra$arima.coefficients
    if (!is.null(coefs)) {
      rn <- rownames(coefs)
      order[1]    <- sum(grepl("^Phi\\d+$", rn))
      order[3]    <- sum(grepl("^Theta\\d+$", rn))
      seasonal[1] <- sum(grepl("^BPhi\\d+$", rn))
      seasonal[3] <- sum(grepl("^BTheta\\d+$", rn))
    }
  }, error = function(e) {
    warning(sprintf("ARIMA extraction failed, using defaults: %s", e$message))
  })

  list(order = order, seasonal = seasonal)
}


# =============================================================================
# OUTLIER INFO EXTRACTION
# =============================================================================
#
# Extracts detected outlier types and dates from RJDemetra regression output.
#
# @param result RJDemetra result (from x13() or regarima())
# @param is_x13 TRUE if result is from x13()
# @return List with: types (character vector), dates (character vector)
# =============================================================================

extract_outlier_info <- function(result, is_x13 = FALSE) {
  ra <- if (is_x13 && !is.null(result$regarima)) result$regarima else result

  types <- character(0)
  dates <- character(0)

  tryCatch({
    coefs <- ra$regression.coefficients
    if (!is.null(coefs)) {
      outlier_rows <- grepl("^(AO|LS|TC)", rownames(coefs))
      if (any(outlier_rows)) {
        types <- gsub("\\s*\\(.*", "", rownames(coefs)[outlier_rows])
        dates <- gsub(".*\\((.*)\\).*", "\\1", rownames(coefs)[outlier_rows])
      }
    }
  }, error = function(e) {
    warning(sprintf("Outlier extraction failed: %s", e$message))
  })

  list(types = types, dates = dates)
}


# =============================================================================
# SPAN FILTERING
# =============================================================================
#
# Applies date range restriction from config$span_start / config$span_end.
#
# @param ts_data Time series object
# @param config  Configuration list with span_start, span_end
# @return List with: ts (filtered series), warnings (character vector)
# =============================================================================

apply_span_filter <- function(ts_data, config) {
  if (is.null(config$span_start) && is.null(config$span_end)) {
    return(list(ts = ts_data, warnings = character(0)))
  }

  warnings <- character(0)
  new_start <- config$span_start %||% start(ts_data)
  new_end <- config$span_end %||% end(ts_data)

  ts_filtered <- tryCatch(
    window(ts_data, start = new_start, end = new_end),
    error = function(e) {
      warnings <<- c(warnings, sprintf("Span filter failed: %s", e$message))
      ts_data
    }
  )

  list(ts = ts_filtered, warnings = warnings)
}


# =============================================================================
# LOG TRANSFORM CHECK
# =============================================================================
#
# Validates that log transform is possible (all values positive).
# If transform is "auto" and values are non-positive, switches to "none".
# If transform is "log" and values are non-positive, returns error.
#
# @param ts_filtered Time series after span filtering
# @param config      Configuration list with transform setting
# @return List with: config (possibly modified), error (logical),
#         message (error string), warnings (character vector)
# =============================================================================

check_log_transform <- function(ts_filtered, config) {
  if (!tolower(config$transform) %in% c("log", "auto")) {
    return(list(config = config, error = FALSE, message = "", warnings = character(0)))
  }

  if (any(ts_filtered <= 0, na.rm = TRUE)) {
    if (tolower(config$transform) == "log") {
      return(list(
        config = config, error = TRUE,
        message = "ERROR: Log transform requested but series contains non-positive values",
        warnings = character(0)
      ))
    }
    # auto -> switch to none
    config$transform <- "none"
    return(list(
      config = config, error = FALSE, message = "",
      warnings = "Series contains non-positive values, using no transform instead of auto"
    ))
  }

  list(config = config, error = FALSE, message = "", warnings = character(0))
}


# =============================================================================
# MESSAGE BUILDING
# =============================================================================
#
# Combines warning messages and diagnostics into a single result string.
#
# @param warnings    Character vector of warning messages
# @param diagnostics Diagnostics list from extract_*_diagnostics()
# @return Formatted message string
# =============================================================================

build_result_messages <- function(warnings, diagnostics) {
  parts <- character(0)
  if (length(warnings) > 0) {
    parts <- c(parts, sprintf("WARNING: %s", warnings))
  }
  diag_str <- format_diagnostics(diagnostics)
  if (nchar(diag_str) > 0) {
    parts <- c(parts, sprintf("DIAGNOSTICS:\n%s", diag_str))
  }
  paste(parts, collapse = "\n")
}


# =============================================================================
# WARNING CAPTURE WRAPPER
# =============================================================================
#
# Executes an expression while capturing all R warnings into a character vector.
# Warnings are muffled so they don't print to console.
#
# @param expr Expression to evaluate
# @return List with: result (expression result), warnings (character vector)
# =============================================================================

run_with_warnings <- function(expr) {
  warnings <- character(0)
  result <- tryCatch(
    withCallingHandlers(expr, warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }),
    error = function(e) {
      list(error = TRUE, messages = conditionMessage(e))
    }
  )
  list(result = result, warnings = warnings)
}


# =============================================================================
# APPEND WARNINGS TO RESULT
# =============================================================================
#
# Appends captured warnings from run_with_warnings() to a result's messages.
#
# @param result   Result list with $messages field
# @param warnings Character vector of warnings
# @return Modified result with warnings appended to messages
# =============================================================================

append_warnings <- function(result, warnings) {
  if (length(warnings) == 0) return(result)
  w_str <- paste(sprintf("WARNING: %s", warnings), collapse = "\n")
  result$messages <- if (nchar(result$messages %||% "") > 0) {
    paste(result$messages, w_str, sep = "\n")
  } else {
    w_str
  }
  result
}


# =============================================================================
# INPUT DISPATCH
# =============================================================================
#
# Routes ts_data to single or batch processing based on input type.
# Handles: single ts, list of ts, single ts wrapped in list.
#
# @param ts_data   Single ts object or named list of ts objects
# @param single_fn Function to call for single ts: function(ts_obj) -> result
# @param batch_fn  Function to call for list of ts: function(ts_list) -> results
# @return Result from single_fn or batch_fn
# =============================================================================

dispatch_ts <- function(ts_data, single_fn, batch_fn) {
  if (is.ts(ts_data)) return(single_fn(ts_data))

  if (is.list(ts_data)) {
    if (!all(vapply(ts_data, is.ts, logical(1)))) {
      if (length(ts_data) == 1 && is.ts(ts_data[[1]])) {
        return(single_fn(ts_data[[1]]))
      }
      stop("ts_data must be a ts object or a list of ts objects")
    }
    return(batch_fn(ts_data))
  }

  stop("ts_data must be a ts object or a list of ts objects")
}


# =============================================================================
# BATCH PROCESSING
# =============================================================================
#
# Processes multiple time series with shared specification.
# Parses spec file once and passes parsed config to each single-series call.
# Each series is processed independently with error isolation.
#
# @param ts_list     Named list of ts objects
# @param single_fn   Function: (ts_obj, parsed_config) -> result list
# @param batch_class Class name for the batch result (e.g. "rjd_forecast_batch")
# @param method      Method description string for summary attribute
# @param spec_file   Path to spec file (NULL = no spec, use defaults)
# @param verbose     Print progress for each series
# @return Batch result object (list with class and summary attributes)
# =============================================================================

run_batch <- function(ts_list, single_fn, batch_class, method,
                      spec_file = NULL, verbose = FALSE) {

  n_series <- length(ts_list)
  series_names <- names(ts_list) %||% as.character(seq_len(n_series))

  # Parse spec file once for efficiency
  base_config <- NULL
  if (!is.null(spec_file) && nchar(trimws(spec_file)) > 0) {
    if (!file.exists(spec_file)) stop(sprintf("Spec file not found: %s", spec_file))
    base_config <- parse_x13_spec(spec_file)
  }

  if (verbose) {
    cat(sprintf("Processing %d series with %s specification\n",
                n_series,
                if (is.null(spec_file)) "default" else basename(spec_file)))
  }

  results <- vector("list", n_series)
  names(results) <- series_names
  all_messages <- character(0)

  for (i in seq_len(n_series)) {
    name <- series_names[i]
    if (verbose) cat(sprintf("  [%d/%d] %s... ", i, n_series, name))

    result <- tryCatch(
      single_fn(ts_list[[i]], base_config),
      error = function(e) {
        list(error = TRUE, messages = sprintf("[%s] ERROR: %s", name, e$message))
      }
    )

    result$series_name <- name

    if (!is.null(result$messages) && nchar(result$messages) > 0) {
      result$messages <- sprintf("[%s] %s", name, result$messages)
      all_messages <- c(all_messages, result$messages)
    }

    results[[name]] <- result
    if (verbose) cat(if (result$error) "ERROR\n" else "OK\n")
  }

  n_success <- sum(!vapply(results, function(r) r$error, logical(1)))

  attr(results, "summary") <- list(
    n_series = n_series, n_success = n_success,
    n_failed = n_series - n_success,
    spec_file = spec_file, method = method
  )
  attr(results, "messages") <- paste(all_messages, collapse = "\n")
  class(results) <- c(batch_class, "list")

  if (verbose) {
    cat(sprintf("Completed: %d success, %d failed\n", n_success, n_series - n_success))
  }

  results
}


# =============================================================================
# CONFIG RESOLUTION
# =============================================================================
#
# Resolves configuration from one of three sources (in priority order):
#   1. Pre-parsed config (from batch processing)
#   2. Spec file (parsed on demand)
#   3. Frequency-specific defaults
#
# @param freq          Time series frequency
# @param parsed_config Pre-parsed config (NULL if not available)
# @param spec_file     Path to spec file (NULL if not provided)
# @param default_fn    Function: (freq) -> default config list
# @return Configuration list
# =============================================================================

resolve_config <- function(freq, parsed_config, spec_file, default_fn) {
  if (!is.null(parsed_config)) {
    config <- parsed_config
    config$periodicity <- freq
    return(config)
  }

  if (!is.null(spec_file) && nchar(trimws(spec_file)) > 0) {
    if (!file.exists(spec_file)) {
      stop(sprintf("Spec file not found: %s", spec_file))
    }
    config <- parse_x13_spec(spec_file)
    config$periodicity <- freq
    return(config)
  }

  default_fn(freq)
}


# =============================================================================
# DIAGNOSTICS EXTRACTION - X13 DECOMPOSITION
# =============================================================================
#
# Extracts diagnostic information from RJDemetra x13 results.
# Includes M-statistics, residual tests, and model quality indicators.
#
# @param x13_result Result from RJDemetra::x13()
# @return Named list of diagnostics
# =============================================================================

extract_x13_diagnostics <- function(x13_result) {

  diagnostics <- list(
    m_statistics = NULL, m_overall = NULL,
    residual_tests = NULL,
    log_likelihood = NULL, aic = NULL, bic = NULL, aicc = NULL,
    seasonality = NULL,
    quality_summary = NULL
  )

  tryCatch({
    # M-statistics from decomposition (matrix with rownames "M(1)"..."Q-M2")
    if (!is.null(x13_result$decomposition)) {
      mstats <- tryCatch(x13_result$decomposition$mstats, error = function(e) NULL)
      if (!is.null(mstats) && is.matrix(mstats)) {
        get_m <- function(name) if (name %in% rownames(mstats)) as.numeric(mstats[name, 1]) else NULL
        diagnostics$m_statistics <- list(
          M1 = get_m("M(1)"), M2 = get_m("M(2)"), M3 = get_m("M(3)"),
          M4 = get_m("M(4)"), M5 = get_m("M(5)"), M6 = get_m("M(6)"),
          M7 = get_m("M(7)"), M8 = get_m("M(8)"), M9 = get_m("M(9)"),
          M10 = get_m("M(10)"), M11 = get_m("M(11)"),
          Q = get_m("Q"), Q_M2 = get_m("Q-M2")
        )
        diagnostics$m_overall <- get_m("Q")
      }
    }

    # Residual diagnostics from RegARIMA
    if (!is.null(x13_result$regarima)) {
      ra <- x13_result$regarima

      # loglik is a matrix with rows: logvalue, aic, aicc, bic, bicc, etc.
      if (!is.null(ra$loglik) && is.matrix(ra$loglik)) {
        ll <- ra$loglik
        get_ll <- function(name) if (name %in% rownames(ll)) as.numeric(ll[name, 1]) else NULL
        diagnostics$log_likelihood <- get_ll("logvalue")
        diagnostics$aic <- get_ll("aic")
        diagnostics$bic <- get_ll("bic")
        diagnostics$aicc <- get_ll("aicc")
      }

      residual_tests <- list()

      # residuals.stat is a list with $st.error and $tests (data.frame)
      if (!is.null(ra$residuals.stat) && is.list(ra$residuals.stat)) {
        tests_df <- ra$residuals.stat$tests
        if (!is.null(tests_df) && is.data.frame(tests_df)) {
          get_test <- function(pattern) {
            idx <- grep(pattern, rownames(tests_df), ignore.case = TRUE)
            if (length(idx) > 0) {
              list(statistic = tests_df$Statistic[idx[1]],
                   p_value = tests_df$P.value[idx[1]])
            } else NULL
          }
          lb <- get_test("^ljung box$")
          if (!is.null(lb)) residual_tests$ljung_box <- lb
          norm_skew <- get_test("skewness")
          norm_kurt <- get_test("kurtosis")
          if (!is.null(norm_skew) || !is.null(norm_kurt)) {
            residual_tests$normality <- list(
              skewness = if (!is.null(norm_skew)) norm_skew$statistic else NULL,
              kurtosis = if (!is.null(norm_kurt)) norm_kurt$statistic else NULL,
              jb_p_value = if (!is.null(norm_skew)) norm_skew$p_value else NULL
            )
          }
        }
      }

      if (length(residual_tests) > 0) {
        diagnostics$residual_tests <- residual_tests
      }
    }

    # Seasonality tests
    if (!is.null(x13_result$diagnostics)) {
      diag <- x13_result$diagnostics
      seasonality <- list()
      if (!is.null(diag$combined_test))       seasonality$combined_test <- diag$combined_test
      if (!is.null(diag$residual_seasonality)) seasonality$residual_seasonality <- diag$residual_seasonality
      if (length(seasonality) > 0) diagnostics$seasonality <- seasonality
    }

    # Quality summary
    quality_messages <- c()
    if (!is.null(diagnostics$m_overall)) {
      q <- diagnostics$m_overall
      quality_messages <- c(quality_messages,
        if (q < 1)  sprintf("Good decomposition quality (Q=%.2f)", q)
        else if (q < 2) sprintf("Acceptable decomposition quality (Q=%.2f)", q)
        else sprintf("Poor decomposition quality (Q=%.2f) - review results", q))
    }
    if (!is.null(diagnostics$residual_tests$normality$jb_p_value) &&
        diagnostics$residual_tests$normality$jb_p_value < 0.01) {
      quality_messages <- c(quality_messages, "Residuals deviate significantly from normality")
    }
    if (!is.null(diagnostics$residual_tests$ljung_box$p_value) &&
        diagnostics$residual_tests$ljung_box$p_value < 0.01) {
      quality_messages <- c(quality_messages, "Significant residual autocorrelation detected")
    }
    if (length(quality_messages) > 0) {
      diagnostics$quality_summary <- paste(quality_messages, collapse = "; ")
    }
  }, error = function(e) {
    warning(sprintf("X13 diagnostics extraction failed: %s", e$message))
  })

  diagnostics
}


# =============================================================================
# DIAGNOSTICS EXTRACTION - REGARIMA
# =============================================================================
#
# Extracts diagnostic information from RJDemetra regarima results.
# Used for forecasting, backcasting, and MAVE.
#
# @param regarima_result Result from RJDemetra::regarima()
# @return Named list of diagnostics
# =============================================================================

extract_regarima_diagnostics <- function(regarima_result) {

  diagnostics <- list(
    log_likelihood = NULL, aic = NULL, bic = NULL, aicc = NULL,
    residual_se = NULL, residual_tests = NULL, quality_summary = NULL
  )

  tryCatch({
    # loglik is a matrix with rows: logvalue, aic, aicc, bic, bicc, etc.
    if (!is.null(regarima_result$loglik) && is.matrix(regarima_result$loglik)) {
      ll <- regarima_result$loglik
      get_ll <- function(name) if (name %in% rownames(ll)) as.numeric(ll[name, 1]) else NULL
      diagnostics$log_likelihood <- get_ll("logvalue")
      diagnostics$aic <- get_ll("aic")
      diagnostics$bic <- get_ll("bic")
      diagnostics$aicc <- get_ll("aicc")
    }
    if (!is.null(regarima_result$residuals.stat) && is.list(regarima_result$residuals.stat)) {
      diagnostics$residual_se <- regarima_result$residuals.stat$st.error
    }

    residual_tests <- list()
    # residuals.stat$tests is a data.frame with Statistic, P.value, Description
    if (!is.null(regarima_result$residuals.stat) && is.list(regarima_result$residuals.stat)) {
      tests_df <- regarima_result$residuals.stat$tests
      if (!is.null(tests_df) && is.data.frame(tests_df)) {
        get_test <- function(pattern) {
          idx <- grep(pattern, rownames(tests_df), ignore.case = TRUE)
          if (length(idx) > 0) {
            list(statistic = tests_df$Statistic[idx[1]],
                 p_value = tests_df$P.value[idx[1]])
          } else NULL
        }
        lb <- get_test("^ljung box$")
        if (!is.null(lb)) residual_tests$ljung_box <- lb
        norm_skew <- get_test("skewness")
        norm_kurt <- get_test("kurtosis")
        if (!is.null(norm_skew) || !is.null(norm_kurt)) {
          residual_tests$normality <- list(
            skewness = if (!is.null(norm_skew)) norm_skew$statistic else NULL,
            kurtosis = if (!is.null(norm_kurt)) norm_kurt$statistic else NULL,
            jb_p_value = if (!is.null(norm_skew)) norm_skew$p_value else NULL
          )
        }
      }
    }
    if (length(residual_tests) > 0) diagnostics$residual_tests <- residual_tests

    # Quality summary
    quality_messages <- c()
    if (!is.null(diagnostics$aic)) {
      quality_messages <- c(quality_messages, sprintf("AIC=%.2f", diagnostics$aic))
    }
    if (!is.null(residual_tests$normality$jb_p_value) &&
        residual_tests$normality$jb_p_value < 0.01) {
      quality_messages <- c(quality_messages, "Non-normal residuals")
    }
    if (!is.null(residual_tests$ljung_box$p_value) &&
        residual_tests$ljung_box$p_value < 0.01) {
      quality_messages <- c(quality_messages, "Residual autocorrelation")
    }
    if (length(quality_messages) > 0) {
      diagnostics$quality_summary <- paste(quality_messages, collapse = "; ")
    }
  }, error = function(e) {
    warning(sprintf("RegARIMA diagnostics extraction failed: %s", e$message))
  })

  diagnostics
}


# =============================================================================
# FORMAT DIAGNOSTICS FOR OUTPUT
# =============================================================================


# =============================================================================
# RESOLVE SPEC FILE FOR A SERIES
# =============================================================================
#
# Looks up whether a series has a custom spec file assigned. If so, returns
# the full path (spec_folder / spec_name). Otherwise returns NULL (defaults).
#
# @param series_name       Name of the current series (matched to custom_spec_series)
# @param spec_names        Character vector of spec file names
# @param spec_folder       Path to directory containing spec files
# @param custom_spec_series Character vector of series names with custom specs
#                           (same length and order as spec_names)
# @return Full path to the spec file, or NULL
# =============================================================================

resolve_series_spec <- function(series_name, spec_names, spec_folder,
                                 custom_spec_series) {
  if (is.null(custom_spec_series) || length(custom_spec_series) == 0) {
    return(NULL)
  }
  idx <- match(series_name, custom_spec_series)
  if (is.na(idx)) return(NULL)
  file.path(spec_folder, spec_names[idx])
}


# =============================================================================
# PERIODICITY STRING TO FREQUENCY
# =============================================================================
#
# Maps a periodicity string ("A", "Q", "M") to the corresponding ts frequency.
#
# @param periodicity Character: "A", "Q", or "M"
# @return Integer: 1, 4, or 12
# =============================================================================

periodicity_to_freq <- function(periodicity) {
  switch(toupper(periodicity),
    "A" = 1L, "Q" = 4L, "M" = 12L,
    stop(sprintf("Unknown periodicity '%s'. Use 'A', 'Q', or 'M'.", periodicity))
  )
}


# =============================================================================
# GENERATE DATE COLUMN LABELS
# =============================================================================
#
# Generates a sequence of date column labels from (year, period) for
# n_periods at the given frequency, in the same format the matrix uses.
#
# @param start_year   Integer year
# @param start_period Integer period (1-based)
# @param n_periods    Integer number of periods to generate
# @param freq         Integer frequency (1, 4, or 12)
# @return Character vector of date labels
# =============================================================================

generate_date_labels <- function(start_year, start_period, n_periods, freq) {
  month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  labels <- character(n_periods)
  yr <- start_year
  pd <- start_period
  for (i in seq_len(n_periods)) {
    if (freq == 1L) {
      labels[i] <- as.character(yr)
    } else if (freq == 4L) {
      labels[i] <- sprintf("%dQ%d", yr, pd)
    } else {
      labels[i] <- sprintf("%d%s", yr, month_names[pd])
    }
    pd <- pd + 1L
    if (pd > freq) {
      pd <- 1L
      yr <- yr + 1L
    }
  }
  labels
}


# =============================================================================
# TARGET DATE PARSING
# =============================================================================
#
# Parses a target date string into components.
#
# Supported formats:
#   "2030"       -> annual   (freq=1)  -> year=2030, period=1
#   "2030Q3"     -> quarterly(freq=4)  -> year=2030, period=3
#   "2030May"    -> monthly  (freq=12) -> year=2030, period=5
#   "2030-05-01" -> monthly  (freq=12) -> year=2030, period=5
#
# @param date_string Character string of target date
# @return List with: year, period, frequency
# =============================================================================

# =============================================================================
# ALIGN PARSED PERIOD TO TARGET FREQUENCY
# =============================================================================
#
# Converts a period from parse_target_date() to the correct sub-period for a
# given target frequency. Needed because ISO date strings ("2010-04-01") are
# always parsed as monthly (period = month number), but when the matrix uses
# quarterly data the period must be a quarter number (1-4).
#
# @param parsed List from parse_target_date() with $year, $period, $frequency
# @param freq   Target frequency (1, 4, or 12)
# @return Integer period aligned to freq
# =============================================================================

align_period <- function(parsed, freq) {
  if (parsed$frequency == freq) return(parsed$period)
  if (parsed$frequency == 12L && freq == 4L) {
    return((parsed$period - 1L) %/% 3L + 1L)
  }
  if (parsed$frequency == 12L && freq == 1L) {
    return(1L)
  }
  parsed$period
}


parse_target_date <- function(date_string) {
  if (is.null(date_string) || !nzchar(trimws(date_string))) {
    return(NULL)
  }
  ds <- trimws(date_string)

  month_map <- c(
    Jan = 1L, Feb = 2L, Mar = 3L, Apr = 4L, May = 5L, Jun = 6L,
    Jul = 7L, Aug = 8L, Sep = 9L, Oct = 10L, Nov = 11L, Dec = 12L
  )

  # ISO date: "2030-05-01"
  if (grepl("^\\d{4}-\\d{2}-\\d{2}$", ds)) {
    parts <- as.integer(strsplit(ds, "-")[[1]])
    return(list(year = parts[1], period = parts[2], frequency = 12L))
  }

  # Annual: "2030"
  if (grepl("^\\d{4}$", ds)) {
    return(list(year = as.integer(ds), period = 1L, frequency = 1L))
  }

  # Quarterly: "2030Q3"
  m <- regmatches(ds, regexec("^(\\d{4})[Qq]([1-4])$", ds))[[1]]
  if (length(m) == 3) {
    return(list(year = as.integer(m[2]), period = as.integer(m[3]), frequency = 4L))
  }

  # Monthly: "2030May"
  m <- regmatches(ds, regexec("^(\\d{4})([A-Za-z]{3})$", ds))[[1]]
  if (length(m) == 3) {
    mon <- unname(month_map[m[3]])
    if (!is.na(mon)) {
      return(list(year = as.integer(m[2]), period = mon, frequency = 12L))
    }
  }

  stop(sprintf("Cannot parse target date: '%s'", date_string))
}


# =============================================================================
# MATRIX ROW TO TS CONVERSION
# =============================================================================
#
# Extracts a contiguous non-NA/non-NaN range from a matrix row and converts
# it to a ts object. Returns the column indices so results can be written back.
#
# @param row_values Numeric vector (one row of the matrix)
# @param col_dates  Character vector of column date strings (colnames)
# @param freq       Integer frequency (1, 4, or 12)
# @return List with: ts_obj, start_col, end_col (1-based column indices)
#         or NULL if no valid data
# =============================================================================

matrix_row_to_ts <- function(row_values, col_dates, freq) {
  valid <- which(!is.na(row_values) & !is.nan(row_values))
  if (length(valid) == 0) return(NULL)

  start_col <- min(valid)
  end_col   <- max(valid)
  values    <- as.numeric(row_values[start_col:end_col])

  # Parse the start date column to get ts start
  start_parsed <- parse_target_date(col_dates[start_col])
  start_period <- align_period(start_parsed, freq)
  ts_obj <- ts(values, start = c(start_parsed$year, start_period),
               frequency = freq)

  list(ts_obj = ts_obj, start_col = start_col, end_col = end_col)
}


# =============================================================================
# PERIODS TO TARGET CALCULATOR
# =============================================================================
#
# Calculates the number of periods between a series endpoint and a target date.
# Positive = target is ahead, zero = already there, negative = past target.
#
# @param end_year     Integer year of series endpoint
# @param end_period   Integer period of series endpoint
# @param target_year  Integer year of target
# @param target_period Integer period of target
# @param freq         Integer frequency (1, 4, or 12)
# @return Integer number of periods
# =============================================================================

calculate_periods_to_target <- function(end_year, end_period,
                                        target_year, target_period, freq) {
  (target_year - end_year) * freq + (target_period - end_period)
}


format_diagnostics <- function(diagnostics) {
  if (is.null(diagnostics) || length(diagnostics) == 0) return("")

  lines <- c()
  if (!is.null(diagnostics$m_overall)) {
    lines <- c(lines, sprintf("Decomposition quality (Q): %.3f", diagnostics$m_overall))
  }
  if (!is.null(diagnostics$aic)) {
    lines <- c(lines, sprintf("AIC: %.2f", diagnostics$aic))
  }
  if (!is.null(diagnostics$quality_summary)) {
    lines <- c(lines, sprintf("Notes: %s", diagnostics$quality_summary))
  }

  if (length(lines) > 0) paste(lines, collapse = "\n") else ""
}
