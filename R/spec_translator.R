# =============================================================================
# X13 SPEC FILE TO RJDEMETRA SPECIFICATION TRANSLATOR
# =============================================================================
#
# Translates X13-ARIMA-SEATS specification files (.spc) into equivalent
# RJDemetra specifications.
#
# Provides two spec creators:
#   - create_rjdemetra_spec()  : For X13 seasonal adjustment (X11 decomposition)
#   - create_regarima_spec()   : For RegARIMA-only (forecast, backcast, mave)
#
# IMPORTANT: RJDemetra produces methodologically equivalent but NOT numerically
# identical results to X13 command line. For exact X13 replication, use the
# 'seasonal' R package which calls the actual Census Bureau X13 binary.
#
# =============================================================================


# =============================================================================
# PARSE X13 SPEC FILE
# =============================================================================
#
# Reads an X13 .spc file and extracts parameters into a structured list.
# Handles placeholder tokens (e.g. @@@forecast_maxlead=4@@@) by extracting
# the default value after the = sign.
#
# @param spec_file Path to the X13 .spc file
# @return Named list containing all parsed configuration values
# =============================================================================

parse_x13_spec <- function(spec_file) {

  if (!file.exists(spec_file)) {
    stop(sprintf("Spec file not found: %s", spec_file))
  }

  # Read file and remove comments (preserving # inside quoted strings)
  lines <- readLines(spec_file, warn = FALSE)
  content <- paste(lines, collapse = "\n")
  content <- gsub('"[^"]*"(*SKIP)(*FAIL)|#[^\n]*', "", content, perl = TRUE)

  # Replace @@@key=value@@@ placeholder tokens with just the value
  content <- gsub("@@@[^=]+=([^@]+)@@@", "\\1", content)

  # Initialize configuration with sensible defaults
  config <- list(
    periodicity = 4,
    title = NULL,
    span_start = NULL,
    span_end = NULL,
    transform = "auto",
    arima_auto = TRUE,
    arima_order = NULL,
    seasonal_order = NULL,
    automdl_maxorder = NULL,
    automdl_maxdiff = NULL,
    outlier_enabled = TRUE,
    outlier_ao = TRUE,
    outlier_ls = TRUE,
    outlier_tc = FALSE,
    outlier_cv = NULL,
    user_outliers = list(),
    trading_days = FALSE,
    trading_days_type = "TradingDays",
    td_pretest = TRUE,
    easter = FALSE,
    easter_pretest = TRUE,
    easter_duration = 8,
    arima_mean = FALSE,
    transform_constant = NULL,
    user_regressors = NULL,
    prior_adjustment = NULL,
    x11_mode = "mult",
    x11_seasonalma = "msr",
    x11_trendma = NULL,
    x11_type = "sa",
    forecast_maxlead = NULL,
    forecast_maxback = NULL,
    force_enabled = FALSE,
    force_type = "denton",
    force_rho = 1,
    force_lambda = 1,
    force_mode = "ratio"
  )

  # ---------------------------------------------------------------------------
  # Parse series{} block
  # ---------------------------------------------------------------------------
  series_block <- regmatches(content, regexpr("series\\s*\\{[^}]*\\}", content, ignore.case = TRUE))
  if (length(series_block) > 0) {
    period_match <- regmatches(series_block, regexpr("period\\s*=\\s*(\\d+)", series_block))
    if (length(period_match) > 0) {
      config$periodicity <- as.integer(gsub("\\D", "", period_match))
    }

    title_match <- regmatches(series_block, regexpr("title\\s*=\\s*\"([^\"]+)\"", series_block))
    if (length(title_match) > 0) {
      config$title <- gsub("title\\s*=\\s*\"|\"", "", title_match)
    }

    # Span: span=(1997.1, 2021.4) or span=(1997.1,) for open-ended
    span_match <- regmatches(series_block, regexpr("span\\s*=\\s*\\([^)]+\\)", series_block, ignore.case = TRUE))
    if (length(span_match) > 0) {
      span_content <- gsub("span\\s*=\\s*\\(|\\)", "", span_match, ignore.case = TRUE)
      span_parts <- strsplit(span_content, ",")[[1]]
      if (length(span_parts) >= 1 && nchar(trimws(span_parts[1])) > 0) {
        start_match <- regmatches(span_parts[1], regexpr("(\\d{4})\\.(\\d+)", span_parts[1]))
        if (length(start_match) > 0) {
          config$span_start <- c(
            as.integer(substr(start_match, 1, 4)),
            as.integer(gsub(".*\\.", "", start_match)))
        }
      }
      if (length(span_parts) >= 2 && nchar(trimws(span_parts[2])) > 0) {
        end_match <- regmatches(span_parts[2], regexpr("(\\d{4})\\.(\\d+)", span_parts[2]))
        if (length(end_match) > 0) {
          config$span_end <- c(
            as.integer(substr(end_match, 1, 4)),
            as.integer(gsub(".*\\.", "", end_match)))
        }
      }
    }
  }

  # ---------------------------------------------------------------------------
  # Parse transform{} block
  # ---------------------------------------------------------------------------
  transform_block <- regmatches(content, regexpr("transform\\s*\\{[^}]*\\}", content, ignore.case = TRUE))
  if (length(transform_block) > 0) {
    if (grepl("function\\s*=\\s*log", transform_block, ignore.case = TRUE)) {
      config$transform <- "log"
    } else if (grepl("function\\s*=\\s*none", transform_block, ignore.case = TRUE)) {
      config$transform <- "none"
    } else if (grepl("function\\s*=\\s*auto", transform_block, ignore.case = TRUE)) {
      config$transform <- "auto"
    }

    # Constant added before log transform (for series with values near zero)
    const_match <- regmatches(transform_block, regexpr(
      "constant\\s*=\\s*([0-9.]+)", transform_block, ignore.case = TRUE))
    if (length(const_match) > 0) {
      config$transform_constant <- as.numeric(gsub("constant\\s*=\\s*", "", const_match))
    }

    # Permanent prior adjustment factors from external file
    if (grepl("type\\s*=\\s*(perm|permanent)", transform_block, ignore.case = TRUE)) {
      config$prior_adjustment <- list(type = "permanent")
      file_match <- regmatches(transform_block, regexpr(
        "file\\s*=\\s*[^\\s}]+", transform_block, ignore.case = TRUE))
      if (length(file_match) > 0) {
        config$prior_adjustment$file <- gsub("file\\s*=\\s*", "", file_match)
      }
      mode_match <- regmatches(transform_block, regexpr(
        "mode\\s*=\\s*(ratio|diff)", transform_block, ignore.case = TRUE))
      if (length(mode_match) > 0) {
        config$prior_adjustment$mode <- gsub("mode\\s*=\\s*", "", mode_match)
      }
    }
  }

  # ---------------------------------------------------------------------------
  # Parse arima{} block
  # ---------------------------------------------------------------------------
  arima_block <- regmatches(content, regexpr("arima\\s*\\{[^}]*\\}", content, ignore.case = TRUE))
  if (length(arima_block) > 0) {
    # Full seasonal model: (p,d,q)(P,D,Q)
    model_match <- regmatches(arima_block, regexpr("model\\s*=\\s*\\(([0-9,]+)\\)\\s*\\(([0-9,]+)\\)", arima_block))
    if (length(model_match) > 0) {
      parts <- regmatches(model_match, gregexpr("\\([0-9,]+\\)", model_match))[[1]]
      if (length(parts) >= 1) {
        nums <- as.integer(strsplit(gsub("[()]", "", parts[1]), ",")[[1]])
        if (length(nums) == 3) { config$arima_order <- nums; config$arima_auto <- FALSE }
      }
      if (length(parts) >= 2) {
        nums <- as.integer(strsplit(gsub("[()]", "", parts[2]), ",")[[1]])
        if (length(nums) == 3) config$seasonal_order <- nums
      }
    } else {
      # Non-seasonal only: (p,d,q)
      model_match2 <- regmatches(arima_block, regexpr("model\\s*=\\s*\\(([0-9,]+)\\)", arima_block))
      if (length(model_match2) > 0) {
        nums <- as.integer(strsplit(gsub("model\\s*=\\s*|[()]", "", model_match2), ",")[[1]])
        if (length(nums) == 3) {
          config$arima_order <- nums
          config$arima_auto <- FALSE
          config$seasonal_order <- c(0, 0, 0)
        }
      }
    }
  }

  # ---------------------------------------------------------------------------
  # Parse automdl{} or pickmdl{} blocks
  # ---------------------------------------------------------------------------
  automdl_block <- regmatches(content, regexpr("automdl\\s*\\{[^}]*\\}", content, ignore.case = TRUE))
  pickmdl_block <- regmatches(content, regexpr("pickmdl\\s*\\{[^}]*\\}", content, ignore.case = TRUE))
  if (length(automdl_block) > 0 || length(pickmdl_block) > 0) {
    config$arima_auto <- TRUE
  }

  # Extract automdl constraints (maxorder, maxdiff) - these cannot be passed to
  # RJDemetra but are captured for transparency and warning purposes.
  # JDemetra+ hardcodes: max regular order = 3, max seasonal order = 1,
  # max regular diff = 2, max seasonal diff = 1.
  if (length(automdl_block) > 0) {
    maxorder_match <- regmatches(automdl_block, regexpr(
      "maxorder\\s*=\\s*\\(\\s*(\\d+)\\s+(\\d+)\\s*\\)", automdl_block, ignore.case = TRUE))
    if (length(maxorder_match) > 0) {
      nums <- as.integer(regmatches(maxorder_match, gregexpr("\\d+", maxorder_match))[[1]])
      if (length(nums) == 2) config$automdl_maxorder <- nums
    }

    maxdiff_match <- regmatches(automdl_block, regexpr(
      "maxdiff\\s*=\\s*\\(\\s*(\\d+)\\s+(\\d+)\\s*\\)", automdl_block, ignore.case = TRUE))
    if (length(maxdiff_match) > 0) {
      nums <- as.integer(regmatches(maxdiff_match, gregexpr("\\d+", maxdiff_match))[[1]])
      if (length(nums) == 2) config$automdl_maxdiff <- nums
    }
  }

  # ---------------------------------------------------------------------------
  # Parse regression{} block
  # ---------------------------------------------------------------------------
  #
  # Distinguishes between:
  #   aictest=(easter, td) → pre-tested calendar effects (conditional inclusion)
  #   variables=(Easter[1] TD AO2009.1) → forced effects (always included)
  #
  # Also handles: td1coef (1-coefficient working day), Const (mean/intercept),
  # user-defined outliers (AO/LS/TC with numeric or month-name dates),
  # and user-defined seasonal regressors (user=..., usertype=..., file=...).
  # ---------------------------------------------------------------------------
  regression_block <- regmatches(content, regexpr("regression\\s*\\{[^}]*\\}", content, ignore.case = TRUE))
  if (length(regression_block) > 0) {

    # --- aictest: pre-tested calendar effects ---
    aictest_match <- regmatches(regression_block, regexpr(
      "aictest\\s*=\\s*\\([^)]+\\)", regression_block, ignore.case = TRUE))
    if (length(aictest_match) > 0) {
      if (grepl("easter", aictest_match, ignore.case = TRUE)) {
        config$easter <- TRUE
        config$easter_pretest <- TRUE
      }
      if (grepl("\\btd\\b", aictest_match, ignore.case = TRUE)) {
        config$trading_days <- TRUE
        config$trading_days_type <- "TradingDays"
        config$td_pretest <- TRUE
      }
    }

    # --- variables: forced calendar effects, outliers, Const ---
    vars_match <- regmatches(regression_block, regexpr(
      "variables\\s*=\\s*\\([^)]*\\)", regression_block, ignore.case = TRUE))
    # Also handle single-value syntax: variables=td, variables=easter[1]
    if (length(vars_match) == 0) {
      vars_single <- regmatches(regression_block, regexpr(
        "variables\\s*=\\s*[^(\\s][^\\s}]*", regression_block, ignore.case = TRUE))
      if (length(vars_single) > 0) {
        vars_match <- vars_single
      }
    }

    if (length(vars_match) > 0) {
      vars_content <- gsub("variables\\s*=\\s*\\(?|\\)?", "", vars_match, ignore.case = TRUE)
      vars_parts <- strsplit(trimws(vars_content), "\\s+")[[1]]

      # Month name to period number mapping
      month_map <- c(jan=1, feb=2, mar=3, apr=4, may=5, jun=6,
                     jul=7, aug=8, sep=9, oct=10, nov=11, dec=12)

      for (part in vars_parts) {
        part_trimmed <- trimws(part)
        if (nchar(part_trimmed) == 0) next

        # Easter effect (forced, no pre-testing)
        if (grepl("^easter", part_trimmed, ignore.case = TRUE)) {
          config$easter <- TRUE
          config$easter_pretest <- FALSE
          easter_dur <- regmatches(part_trimmed, regexpr(
            "\\[(\\d+)\\]", part_trimmed))
          if (length(easter_dur) > 0) {
            config$easter_duration <- as.integer(gsub("\\D", "", easter_dur))
          }
          next
        }

        # Trading days - td1coef (1-coefficient working day, forced)
        if (grepl("^td1coef$", part_trimmed, ignore.case = TRUE)) {
          config$trading_days <- TRUE
          config$trading_days_type <- "WorkingDays"
          config$td_pretest <- FALSE
          next
        }

        # Trading days - TD (full 6-coefficient, forced)
        if (grepl("^td$", part_trimmed, ignore.case = TRUE)) {
          config$trading_days <- TRUE
          config$trading_days_type <- "TradingDays"
          config$td_pretest <- FALSE
          next
        }

        # Const (mean/intercept for stationary ARIMA)
        if (grepl("^const$", part_trimmed, ignore.case = TRUE)) {
          config$arima_mean <- TRUE
          next
        }

        # User-defined outliers - numeric period: AO2009.1, LS2013.2
        if (grepl("^(ao|ls|tc)(\\d{4})\\.(\\d+)$", part_trimmed, ignore.case = TRUE)) {
          type <- toupper(substr(part_trimmed, 1, 2))
          year <- as.integer(substr(part_trimmed, 3, 6))
          period <- as.integer(gsub(".*\\.", "", part_trimmed))
          config$user_outliers[[length(config$user_outliers) + 1]] <- list(
            type = type, year = year, period = period)
          next
        }

        # User-defined outliers - month name: AO2011.Jan, LS2009.Oct
        if (grepl("^(ao|ls|tc)(\\d{4})\\.(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec)$",
                  part_trimmed, ignore.case = TRUE)) {
          type <- toupper(substr(part_trimmed, 1, 2))
          year <- as.integer(substr(part_trimmed, 3, 6))
          month_name <- tolower(gsub(".*\\.", "", part_trimmed))
          period <- month_map[month_name]
          config$user_outliers[[length(config$user_outliers) + 1]] <- list(
            type = type, year = year, period = period)
          next
        }
      }
    }

    # --- Easter duration from aictest context (not in variables) ---
    # If Easter was set via aictest, check for duration in the block
    if (config$easter && config$easter_pretest) {
      easter_dur <- regmatches(regression_block, regexpr(
        "easter\\[(\\d+)\\]", regression_block, ignore.case = TRUE))
      if (length(easter_dur) > 0) {
        config$easter_duration <- as.integer(gsub("\\D", "", easter_dur))
      }
    }

    # --- User-defined seasonal regressors (from external file) ---
    if (grepl("\\buser\\s*=", regression_block, ignore.case = TRUE)) {
      user_match <- regmatches(regression_block, regexpr(
        "user\\s*=\\s*\\([^)]+\\)", regression_block, ignore.case = TRUE))
      usertype_match <- regmatches(regression_block, regexpr(
        "usertype\\s*=\\s*\\(?[^)\\s}]+\\)?", regression_block, ignore.case = TRUE))
      config$user_regressors <- list(
        user = if (length(user_match) > 0) user_match else NULL,
        usertype = if (length(usertype_match) > 0) usertype_match else NULL
      )
    }
  }

  # ---------------------------------------------------------------------------
  # Parse outlier{} block
  # ---------------------------------------------------------------------------
  outlier_block <- regmatches(content, regexpr("outlier\\s*\\{[^}]*\\}", content, ignore.case = TRUE))
  if (length(outlier_block) > 0) {
    config$outlier_enabled <- TRUE
    types_match <- regmatches(outlier_block, regexpr("types\\s*=\\s*\\([^)]+\\)", outlier_block, ignore.case = TRUE))
    if (length(types_match) > 0) {
      config$outlier_ao <- grepl("\\bao\\b", types_match, ignore.case = TRUE)
      config$outlier_ls <- grepl("\\bls\\b", types_match, ignore.case = TRUE)
      config$outlier_tc <- grepl("\\btc\\b", types_match, ignore.case = TRUE)
    } else {
      config$outlier_ao <- grepl("\\bao\\b", outlier_block, ignore.case = TRUE)
      config$outlier_ls <- grepl("\\bls\\b", outlier_block, ignore.case = TRUE)
      config$outlier_tc <- grepl("\\btc\\b", outlier_block, ignore.case = TRUE)
    }
    if (!config$outlier_ao && !config$outlier_ls && !config$outlier_tc) {
      config$outlier_enabled <- FALSE
    }

    cv_match <- regmatches(outlier_block, regexpr("critical\\s*=\\s*([0-9.]+)", outlier_block, ignore.case = TRUE))
    if (length(cv_match) > 0) {
      config$outlier_cv <- as.numeric(gsub("critical\\s*=\\s*", "", cv_match))
    }
  }

  # ---------------------------------------------------------------------------
  # Parse x11{} block
  # ---------------------------------------------------------------------------
  x11_block <- regmatches(content, regexpr("x11\\s*\\{[^}]*\\}", content, ignore.case = TRUE))
  if (length(x11_block) > 0) {
    if (grepl("type\\s*=\\s*summary", x11_block, ignore.case = TRUE)) {
      config$x11_type <- "summary"
    }
    if (grepl("mode\\s*=\\s*add\\b", x11_block, ignore.case = TRUE)) {
      config$x11_mode <- "add"
    } else if (grepl("mode\\s*=\\s*pseudoadd", x11_block, ignore.case = TRUE)) {
      config$x11_mode <- "pseudoadd"
    } else if (grepl("mode\\s*=\\s*mult", x11_block, ignore.case = TRUE)) {
      config$x11_mode <- "mult"
    }

    # Seasonal filter: seasonalma or sma (alias used in some specs)
    seasonalma_match <- regmatches(x11_block, regexpr("seasonalma\\s*=\\s*(\\w+)", x11_block, ignore.case = TRUE))
    if (length(seasonalma_match) > 0) {
      config$x11_seasonalma <- tolower(gsub("seasonalma\\s*=\\s*", "", seasonalma_match))
    } else {
      sma_match <- regmatches(x11_block, regexpr("\\bsma\\s*=\\s*(\\w+)", x11_block, ignore.case = TRUE))
      if (length(sma_match) > 0) {
        config$x11_seasonalma <- tolower(gsub("\\bsma\\s*=\\s*", "", sma_match))
      }
    }

    # Trend filter: trendma or tma (alias used in some specs)
    trendma_match <- regmatches(x11_block, regexpr("trendma\\s*=\\s*(\\d+)", x11_block, ignore.case = TRUE))
    if (length(trendma_match) > 0) {
      config$x11_trendma <- as.integer(gsub("\\D", "", trendma_match))
    } else {
      tma_match <- regmatches(x11_block, regexpr("\\btma\\s*=\\s*(\\d+)", x11_block, ignore.case = TRUE))
      if (length(tma_match) > 0) {
        config$x11_trendma <- as.integer(gsub("\\D", "", tma_match))
      }
    }
  }

  # ---------------------------------------------------------------------------
  # Parse forecast{} block
  # ---------------------------------------------------------------------------
  forecast_block <- regmatches(content, regexpr("forecast\\s*\\{[^}]*\\}", content, ignore.case = TRUE))
  if (length(forecast_block) > 0) {
    maxlead_match <- regmatches(forecast_block, regexpr("maxlead\\s*=\\s*(\\d+)", forecast_block, ignore.case = TRUE))
    if (length(maxlead_match) > 0) {
      config$forecast_maxlead <- as.integer(gsub("\\D", "", maxlead_match))
    }
    maxback_match <- regmatches(forecast_block, regexpr("maxback\\s*=\\s*(\\d+)", forecast_block, ignore.case = TRUE))
    if (length(maxback_match) > 0) {
      config$forecast_maxback <- as.integer(gsub("\\D", "", maxback_match))
    }
  }

  # ---------------------------------------------------------------------------
  # Parse force{} block (Denton benchmarking)
  # ---------------------------------------------------------------------------
  force_block <- regmatches(content, regexpr("force\\s*\\{[^}]*\\}", content, ignore.case = TRUE))
  if (length(force_block) > 0) {
    if (grepl("type\\s*=\\s*denton", force_block, ignore.case = TRUE)) {
      config$force_enabled <- TRUE
      rho_match <- regmatches(force_block, regexpr("rho\\s*=\\s*([0-9.]+)", force_block, ignore.case = TRUE))
      if (length(rho_match) > 0) config$force_rho <- as.numeric(gsub("rho\\s*=\\s*", "", rho_match))
      lambda_match <- regmatches(force_block, regexpr("lambda\\s*=\\s*([0-9.]+)", force_block, ignore.case = TRUE))
      if (length(lambda_match) > 0) config$force_lambda <- as.numeric(gsub("lambda\\s*=\\s*", "", lambda_match))
      if (grepl("mode\\s*=\\s*ratio", force_block, ignore.case = TRUE)) config$force_mode <- "ratio"
      else if (grepl("mode\\s*=\\s*diff", force_block, ignore.case = TRUE)) config$force_mode <- "diff"
    }
  }

  config
}


# =============================================================================
# CREATE RJDEMETRA X13 SPECIFICATION (for seasonal adjustment)
# =============================================================================
#
# Translates a parsed X13 config into an RJDemetra x13_spec object.
# Includes X11 decomposition settings (mode, seasonal filter, trend filter).
#
# @param config        Named list from parse_x13_spec() or get_default_config()
# @param ts_obj        Optional time series for auto-transform detection
# @param transform_fun Pre-resolved transform ("Log"/"None"), or NULL to resolve here
# @return RJDemetra SA_spec object
# =============================================================================

create_rjdemetra_spec <- function(config, ts_obj = NULL, transform_fun = NULL) {

  if (!requireNamespace("RJDemetra", quietly = TRUE)) {
    stop("RJDemetra package is required. Install with: install.packages('RJDemetra')")
  }

  if (is.null(transform_fun)) {
    transform_fun <- resolve_transform(config$transform, ts_obj)
  }

  # Map X11 decomposition mode
  x11_mode <- switch(tolower(config$x11_mode %||% "mult"),
    "add" = "Additive",
    "mult" = "Multiplicative",
    "pseudoadd" = "PseudoAdditive",
    "Multiplicative"
  )

  # Map seasonal filter
  seasonal_filter <- switch(tolower(config$x11_seasonalma %||% "msr"),
    "s3x1" = "S3X1", "s3x3" = "S3X3", "s3x5" = "S3X5",
    "s3x9" = "S3X9", "s3x15" = "S3X15", "msr" = "Msr",
    "Msr"
  )

  # Forecast horizon: -1 means 1 year (RJDemetra/X13 convention)
  fcst_horizon <- config$forecast_maxlead %||% -1

  # Pre-testing: determined by how calendar effects were specified.
  # aictest=(easter) → pre-test (conditional inclusion via significance test)
  # variables=(Easter[1]) → forced (always included, no pre-testing)
  # Default configs use pre-testing to match the default .spc aictest behaviour.
  # RJDemetra expects string values: "Add", "Remove", "None" for both
  # easter.test and tradingdays.test parameters.
  easter_raw <- config$easter_pretest %||% config$easter
  easter_test <- if (isTRUE(easter_raw)) "Add" else if (is.character(easter_raw)) easter_raw else "None"
  td_raw <- config$td_pretest %||% config$trading_days
  td_test <- if (!config$trading_days) "None"
             else if (isTRUE(td_raw)) "Add"
             else if (is.character(td_raw) && td_raw %in% c("Add", "Remove")) td_raw
             else "None"

  # Map trading day type: TradingDays (6 coef) vs WorkingDays (1 coef = td1coef)
  td_option <- if (!config$trading_days) "None"
               else config$trading_days_type %||% "TradingDays"

  # Warn about unsupported custom spec features
  if (!is.null(config$transform_constant)) {
    warning(sprintf(
      "X13 spec uses transform{constant=%.4f} but RJDemetra does not support this. The constant will NOT be applied.",
      config$transform_constant))
  }
  if (!is.null(config$prior_adjustment)) {
    warning("X13 spec uses permanent prior adjustment (transform{type=permanent}) which is not supported in RJDemetra.")
  }
  if (!is.null(config$user_regressors)) {
    warning("X13 spec uses user-defined regressors (regression{user=...}) which are not yet implemented in the spec translator.")
  }

  # Warn if automdl constraints from spec file differ from JDemetra+ hardcoded limits.
  # See create_regarima_spec() for the detailed comment about this limitation.
  if (config$arima_auto) {
    if (!is.null(config$automdl_maxorder)) {
      jd_limits <- c(3, 1)
      if (!identical(config$automdl_maxorder, jd_limits)) {
        warning(sprintf(
          paste0("X13 spec sets automdl maxorder=(%d,%d) but JDemetra+ hardcodes (%d,%d). ",
                 "RJDemetra does not expose this parameter. ",
                 "Model search space will differ from X13."),
          config$automdl_maxorder[1], config$automdl_maxorder[2],
          jd_limits[1], jd_limits[2]))
      }
    }
    if (!is.null(config$automdl_maxdiff)) {
      jd_limits <- c(2, 1)
      if (!identical(config$automdl_maxdiff, jd_limits)) {
        warning(sprintf(
          paste0("X13 spec sets automdl maxdiff=(%d,%d) but JDemetra+ hardcodes (%d,%d). ",
                 "RJDemetra does not expose this parameter."),
          config$automdl_maxdiff[1], config$automdl_maxdiff[2],
          jd_limits[1], jd_limits[2]))
      }
    }
  }

  # Build base specification
  if (config$arima_auto) {
    spec <- RJDemetra::x13_spec("RSA5c",
      transform.function = transform_fun,
      automdl.enabled = TRUE,
      outlier.enabled = config$outlier_enabled,
      outlier.ao = config$outlier_ao,
      outlier.ls = config$outlier_ls,
      outlier.tc = config$outlier_tc,
      tradingdays.option = td_option,
      tradingdays.test = td_test,
      easter.enabled = config$easter,
      easter.test = easter_test,
      easter.duration = config$easter_duration,
      fcst.horizon = fcst_horizon,
      x11.mode = x11_mode,
      x11.seasonalComp = TRUE,
      x11.seasonalma = seasonal_filter
    )
  } else {
    arima <- config$arima_order %||% c(0, 1, 1)
    seasonal <- config$seasonal_order %||% c(0, 1, 1)

    spec <- RJDemetra::x13_spec("RSA5c",
      transform.function = transform_fun,
      automdl.enabled = FALSE,
      arima.p = arima[1], arima.d = arima[2], arima.q = arima[3],
      arima.bp = seasonal[1], arima.bd = seasonal[2], arima.bq = seasonal[3],
      arima.mu = config$arima_mean %||% FALSE,
      outlier.enabled = config$outlier_enabled,
      outlier.ao = config$outlier_ao,
      outlier.ls = config$outlier_ls,
      outlier.tc = config$outlier_tc,
      tradingdays.option = td_option,
      tradingdays.test = td_test,
      easter.enabled = config$easter,
      easter.test = easter_test,
      easter.duration = config$easter_duration,
      fcst.horizon = fcst_horizon,
      x11.mode = x11_mode,
      x11.seasonalComp = TRUE,
      x11.seasonalma = seasonal_filter
    )
  }

  # Outlier critical value
  if (!is.null(config$outlier_cv) && config$outlier_enabled) {
    spec <- RJDemetra::x13_spec(spec, outlier.cv = config$outlier_cv)
  }

  # Henderson trend filter
  if (!is.null(config$x11_trendma)) {
    henderson <- switch(as.character(config$x11_trendma),
      "3" = "H3", "5" = "H5", "7" = "H7", "9" = "H9",
      "13" = "H13", "23" = "H23", NULL)
    if (!is.null(henderson)) {
      spec <- RJDemetra::x13_spec(spec, x11.trendAuto = FALSE, x11.trendma = henderson)
    }
  }

  # User-defined outliers
  if (length(config$user_outliers) > 0) {
    user_types <- c()
    user_dates <- c()
    freq <- config$periodicity

    for (uo in config$user_outliers) {
      if (freq == 12) {
        date <- sprintf("%d-%02d-01", uo$year, uo$period)
      } else if (freq == 4) {
        month <- (uo$period - 1) * 3 + 1
        date <- sprintf("%d-%02d-01", uo$year, month)
      } else {
        date <- sprintf("%d-01-01", uo$year)
      }
      user_types <- c(user_types, uo$type)
      user_dates <- c(user_dates, date)
    }

    spec <- RJDemetra::x13_spec(spec,
      usrdef.outliersEnabled = TRUE,
      usrdef.outliersType = user_types,
      usrdef.outliersDate = user_dates)
  }

  spec
}


# =============================================================================
# CREATE REGARIMA SPECIFICATION (for forecast, backcast, mave)
# =============================================================================
#
# Unified spec creator for RegARIMA-only operations (no X11 decomposition).
# Replaces the separate create_regarima_spec, create_regarima_spec_backcast,
# create_mave_spec, and create_mave_spec_backcast functions.
#
# @param config          Named list from parse_x13_spec() or get_*_config()
# @param ts_obj          Optional time series for auto-transform detection
# @param transform_fun   Pre-resolved transform ("Log"/"None"), or NULL
# @param disable_calendar Disable trading days, Easter, and user-defined outliers
#                         (for backcasting on time-reversed series where
#                         calendar effects and date-specific outliers are
#                         meaningless)
# @return RJDemetra regarima_spec object
# =============================================================================

create_regarima_spec <- function(config, ts_obj = NULL, transform_fun = NULL,
                                 disable_calendar = FALSE) {

  if (!requireNamespace("RJDemetra", quietly = TRUE)) {
    stop("RJDemetra package is required. Install with: install.packages('RJDemetra')")
  }

  if (is.null(transform_fun)) {
    transform_fun <- resolve_transform(config$transform, ts_obj)
  }

  freq <- config$periodicity %||% frequency(ts_obj)

  # Calendar effects (disabled for backcast on reversed series)
  easter_on <- if (!disable_calendar) config$easter else FALSE

  if (disable_calendar) {
    td_option <- "None"
  } else if (config$trading_days) {
    td_option <- config$trading_days_type %||% "TradingDays"
  } else {
    td_option <- "None"
  }

  # Pre-testing: use parsed flags (aictest = pre-test, variables = forced)
  # RJDemetra expects string values: "Add", "Remove", "None" for easter.test
  easter_raw <- if (easter_on) (config$easter_pretest %||% TRUE) else FALSE
  easter_test <- if (isTRUE(easter_raw)) "Add" else if (is.character(easter_raw)) easter_raw else "None"
  td_raw <- config$td_pretest %||% config$trading_days
  td_test <- if (td_option != "None" && isTRUE(td_raw)) "Add" else "None"

  # Warn about unsupported custom spec features
  if (!is.null(config$transform_constant)) {
    warning(sprintf(
      "X13 spec uses transform{constant=%.4f} but RJDemetra does not support this.",
      config$transform_constant))
  }
  if (!is.null(config$prior_adjustment)) {
    warning("X13 spec uses permanent prior adjustment which is not supported in RJDemetra.")
  }
  if (!is.null(config$user_regressors)) {
    warning("X13 spec uses user-defined regressors which are not yet implemented in the spec translator.")
  }

  # Warn if automdl constraints from spec file differ from JDemetra+ hardcoded limits.
  # JDemetra+ hardcodes: max regular ARMA order = 3, max seasonal ARMA order = 1,
  # max regular diff = 2, max seasonal diff = 1.
  # RJDemetra does not expose automdl.maxorder or automdl.maxdiff parameters.
  if (config$arima_auto) {
    if (!is.null(config$automdl_maxorder)) {
      jd_limits <- c(3, 1)  # JDemetra+ hardcoded limits
      if (!identical(config$automdl_maxorder, jd_limits)) {
        warning(sprintf(
          paste0("X13 spec sets automdl maxorder=(%d,%d) but JDemetra+ hardcodes (%d,%d). ",
                 "RJDemetra does not expose this parameter. ",
                 "Model search space will differ from X13."),
          config$automdl_maxorder[1], config$automdl_maxorder[2],
          jd_limits[1], jd_limits[2]))
      }
    }
    if (!is.null(config$automdl_maxdiff)) {
      jd_limits <- c(2, 1)  # JDemetra+ hardcoded limits
      if (!identical(config$automdl_maxdiff, jd_limits)) {
        warning(sprintf(
          paste0("X13 spec sets automdl maxdiff=(%d,%d) but JDemetra+ hardcodes (%d,%d). ",
                 "RJDemetra does not expose this parameter."),
          config$automdl_maxdiff[1], config$automdl_maxdiff[2],
          jd_limits[1], jd_limits[2]))
      }
    }
  }

  if (config$arima_auto) {
    spec <- RJDemetra::regarima_spec_x13("RG5c",
      transform.function = transform_fun,
      automdl.enabled = TRUE,
      outlier.enabled = config$outlier_enabled,
      outlier.ao = config$outlier_ao,
      outlier.ls = config$outlier_ls,
      outlier.tc = config$outlier_tc,
      tradingdays.option = td_option,
      tradingdays.test = td_test,
      easter.enabled = easter_on,
      easter.test = easter_test,
      easter.duration = config$easter_duration,
      fcst.horizon = config$forecast_horizon %||% -1
    )
  } else {
    arima <- config$arima_order %||% c(0, 1, 1)
    seasonal <- config$seasonal_order %||% c(0, 1, 1)

    # Annual data has no seasonal component
    if (freq == 1) seasonal <- c(0, 0, 0)

    spec <- RJDemetra::regarima_spec_x13("RG5c",
      transform.function = transform_fun,
      automdl.enabled = FALSE,
      arima.p = arima[1], arima.d = arima[2], arima.q = arima[3],
      arima.bp = seasonal[1], arima.bd = seasonal[2], arima.bq = seasonal[3],
      arima.mu = config$arima_mean %||% FALSE,
      outlier.enabled = config$outlier_enabled,
      outlier.ao = config$outlier_ao,
      outlier.ls = config$outlier_ls,
      outlier.tc = config$outlier_tc,
      tradingdays.option = td_option,
      tradingdays.test = td_test,
      easter.enabled = easter_on,
      easter.test = easter_test,
      easter.duration = config$easter_duration,
      fcst.horizon = config$forecast_horizon %||% -1
    )
  }

  # Outlier critical value
  if (!is.null(config$outlier_cv) && config$outlier_enabled) {
    spec <- RJDemetra::regarima_spec_x13(spec, outlier.cv = config$outlier_cv)
  }

  # User-defined outliers (from custom spec files)
  # Disabled on reversed series (backcast) because outlier dates refer to specific
  # calendar events — on a time-reversed series these dates map to different data points.
  if (!disable_calendar && length(config$user_outliers %||% list()) > 0) {
    user_types <- c()
    user_dates <- c()
    for (uo in config$user_outliers) {
      if (freq == 12) {
        date <- sprintf("%d-%02d-01", uo$year, uo$period)
      } else if (freq == 4) {
        month <- (uo$period - 1) * 3 + 1
        date <- sprintf("%d-%02d-01", uo$year, month)
      } else {
        date <- sprintf("%d-01-01", uo$year)
      }
      user_types <- c(user_types, uo$type)
      user_dates <- c(user_dates, date)
    }
    spec <- RJDemetra::regarima_spec_x13(spec,
      usrdef.outliersEnabled = TRUE,
      usrdef.outliersType = user_types,
      usrdef.outliersDate = user_dates)
  }

  spec
}
