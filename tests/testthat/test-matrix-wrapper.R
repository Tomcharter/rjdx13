# =============================================================================
# Test: Matrix Wrapper Functions
# =============================================================================
#
# Tests for:
#   - parse_target_date()
#   - matrix_row_to_ts()
#   - calculate_periods_to_target()
#   - periodicity_to_freq()
#   - generate_date_labels()
#   - rjd_forecast_matrix()
#   - rjd_backcast_matrix()
#   - rjd_seasadj_matrix()
#   - rjd_mave_matrix()
#   - Error code stubs (record_ts_error / method_error)
#
# =============================================================================


# =============================================================================
# UTILITY FUNCTION TESTS (no RJDemetra needed)
# =============================================================================

# --- parse_target_date -------------------------------------------------------

test_that("parse_target_date handles annual format", {
  result <- parse_target_date("2030")
  expect_equal(result$year, 2030L)
  expect_equal(result$period, 1L)
  expect_equal(result$frequency, 1L)
})

test_that("parse_target_date handles quarterly format", {
  result <- parse_target_date("2030Q3")
  expect_equal(result$year, 2030L)
  expect_equal(result$period, 3L)
  expect_equal(result$frequency, 4L)
})

test_that("parse_target_date handles lowercase q", {
  result <- parse_target_date("2025q1")
  expect_equal(result$year, 2025L)
  expect_equal(result$period, 1L)
  expect_equal(result$frequency, 4L)
})

test_that("parse_target_date handles monthly name format", {
  result <- parse_target_date("2030May")
  expect_equal(result$year, 2030L)
  expect_equal(result$period, 5L)
  expect_equal(result$frequency, 12L)
})

test_that("parse_target_date handles ISO date format", {
  result <- parse_target_date("2030-05-01")
  expect_equal(result$year, 2030L)
  expect_equal(result$period, 5L)
  expect_equal(result$frequency, 12L)
})

test_that("parse_target_date returns NULL for empty/null input", {
  expect_null(parse_target_date(NULL))
  expect_null(parse_target_date(""))
  expect_null(parse_target_date("   "))
})

test_that("parse_target_date errors on invalid format", {
  expect_error(parse_target_date("abc"), "Cannot parse")
  expect_error(parse_target_date("2030Q5"), "Cannot parse")
  expect_error(parse_target_date("2030Xxx"), "Cannot parse")
})


# --- periodicity_to_freq ----------------------------------------------------

test_that("periodicity_to_freq maps correctly", {
  expect_equal(periodicity_to_freq("A"), 1L)
  expect_equal(periodicity_to_freq("Q"), 4L)
  expect_equal(periodicity_to_freq("M"), 12L)
  expect_equal(periodicity_to_freq("a"), 1L)  # case-insensitive
  expect_equal(periodicity_to_freq("m"), 12L)
})

test_that("periodicity_to_freq errors on unknown", {
  expect_error(periodicity_to_freq("X"), "Unknown periodicity")
})


# --- calculate_periods_to_target ---------------------------------------------

test_that("calculate_periods_to_target works for monthly", {
  expect_equal(calculate_periods_to_target(2020, 6, 2021, 6, 12), 12)
  expect_equal(calculate_periods_to_target(2020, 6, 2020, 9, 12), 3)
  expect_equal(calculate_periods_to_target(2020, 6, 2020, 6, 12), 0)
  expect_equal(calculate_periods_to_target(2021, 6, 2020, 6, 12), -12)
})

test_that("calculate_periods_to_target works for quarterly", {
  expect_equal(calculate_periods_to_target(2020, 4, 2021, 4, 4), 4)
  expect_equal(calculate_periods_to_target(2020, 2, 2021, 1, 4), 3)
})

test_that("calculate_periods_to_target works for annual", {
  expect_equal(calculate_periods_to_target(2020, 1, 2025, 1, 1), 5)
})


# --- generate_date_labels ----------------------------------------------------

test_that("generate_date_labels produces annual labels", {
  labels <- generate_date_labels(2020, 1, 3, 1)
  expect_equal(labels, c("2020", "2021", "2022"))
})

test_that("generate_date_labels produces quarterly labels", {
  labels <- generate_date_labels(2020, 3, 4, 4)
  expect_equal(labels, c("2020Q3", "2020Q4", "2021Q1", "2021Q2"))
})

test_that("generate_date_labels produces monthly labels", {
  labels <- generate_date_labels(2020, 11, 4, 12)
  expect_equal(labels, c("2020Nov", "2020Dec", "2021Jan", "2021Feb"))
})


# --- matrix_row_to_ts --------------------------------------------------------

test_that("matrix_row_to_ts extracts contiguous non-NA range", {
  col_dates <- c("2020Q1", "2020Q2", "2020Q3", "2020Q4", "2021Q1", "2021Q2")
  row <- c(NA, 10, 20, 30, 40, NA)
  result <- matrix_row_to_ts(row, col_dates, 4)

  expect_false(is.null(result))
  expect_s3_class(result$ts_obj, "ts")
  expect_equal(result$start_col, 2L)
  expect_equal(result$end_col, 5L)
  expect_equal(as.numeric(result$ts_obj), c(10, 20, 30, 40))
  expect_equal(start(result$ts_obj), c(2020, 2))
})

test_that("matrix_row_to_ts returns NULL for all-NA row", {
  col_dates <- c("2020Q1", "2020Q2", "2020Q3")
  row <- c(NA, NA, NA)
  expect_null(matrix_row_to_ts(row, col_dates, 4))
})


# --- resolve_series_spec -----------------------------------------------------

test_that("resolve_series_spec returns NULL when no custom specs", {
  expect_null(resolve_series_spec("s1", NULL, NULL, NULL))
  expect_null(resolve_series_spec("s1", NULL, "/tmp", character(0)))
})

test_that("resolve_series_spec returns NULL for non-matching series", {
  result <- resolve_series_spec("s3",
    spec_names = c("a.spc", "b.spc"),
    spec_folder = "/data/specs",
    custom_spec_series = c("s1", "s2"))
  expect_null(result)
})

test_that("resolve_series_spec returns correct path for matching series", {
  result <- resolve_series_spec("s2",
    spec_names = c("a.spc", "b.spc"),
    spec_folder = "/data/specs",
    custom_spec_series = c("s1", "s2"))
  expect_equal(result, file.path("/data/specs", "b.spc"))
})

test_that("resolve_series_spec returns first match for first series", {
  result <- resolve_series_spec("s1",
    spec_names = c("first.spc", "second.spc"),
    spec_folder = "/specs",
    custom_spec_series = c("s1", "s2"))
  expect_equal(result, file.path("/specs", "first.spc"))
})


# --- Error stubs -------------------------------------------------------------

test_that("record_ts_error is silent by default", {
  expect_silent(record_ts_error("CAS02140", "series1"))
})

test_that("method_error returns formatted string by default", {
  msg <- method_error("CAS02140")
  expect_type(msg, "character")
  expect_match(msg, "CAS02140")
})

test_that("record_ts_error dispatches to option function", {
  calls <- list()
  mock_fn <- function(code, names) calls[[length(calls) + 1L]] <<- list(code = code, names = names)
  old <- options(rjdx13.record_ts_error = mock_fn)
  on.exit(options(old), add = TRUE)

  record_ts_error("CAS_RJD_FAIL", "seriesA")
  expect_length(calls, 1)
  expect_equal(calls[[1]]$code, "CAS_RJD_FAIL")
  expect_equal(calls[[1]]$names, "seriesA")
})

test_that("method_error dispatches to option function", {
  mock_fn <- function(code) simpleCondition(sprintf("custom [%s]", code), message = sprintf("custom [%s]", code))
  old <- options(rjdx13.method_error = mock_fn)
  on.exit(options(old), add = TRUE)

  result <- method_error("CAS08040")
  expect_match(result$message, "CAS08040")
})

test_that("error code constants are defined", {
  expect_equal(FORECAST_FREQ_MISMATCH, "CAS02140")
  expect_equal(FORECAST_NO_HORIZON, "CAS08040")
  expect_equal(FORECAST_RJD_FAIL, "CAS_RJD_FAIL")
  expect_equal(BACKCAST_FREQ_MISMATCH, "CAS02140")
  expect_equal(BACKCAST_NO_HORIZON, "CAS08040")
  expect_equal(BACKCAST_RJD_FAIL, "CAS_RJD_FAIL")
  expect_equal(SEASADJ_FREQ_MISMATCH, "CAS02140")
  expect_equal(SEASADJ_RJD_FAIL, "CAS_RJD_FAIL")
  expect_equal(MAVE_FREQ_MISMATCH, "CAS02140")
  expect_equal(MAVE_NO_HORIZON, "CAS08040")
  expect_equal(MAVE_RJD_FAIL, "CAS_RJD_FAIL")
})


# =============================================================================
# MATRIX WRAPPER TESTS — frequency mismatch errors (no RJDemetra needed)
# =============================================================================

test_that("rjd_forecast_matrix stops on frequency mismatch — monthly date with quarterly data", {
  mat <- matrix(1:8, nrow = 1, dimnames = list("s1", paste0("2020Q", 1:8)))
  # This should always fail: "2030May" is monthly freq, but periodicity is "Q"
  # Depending on whether method_error option is set, the stop message differs
  expect_error(rjd_forecast_matrix(mat, date = "2030May", periodicity = "Q"))
})

test_that("rjd_backcast_matrix stops on frequency mismatch — quarterly date with monthly data", {
  col_dates <- generate_date_labels(2020, 1, 12, 12)
  mat <- matrix(1:12, nrow = 1, dimnames = list("s1", col_dates))
  expect_error(rjd_backcast_matrix(mat, date = "2018Q1", periodicity = "M"))
})

test_that("rjd_forecast_matrix stops when no date and no maxlead in spec", {
  col_dates <- generate_date_labels(2020, 1, 4, 4)
  mat <- matrix(1:4, nrow = 1, dimnames = list("s1", col_dates))
  expect_error(rjd_forecast_matrix(mat, date = NULL, periodicity = "Q"))
})

test_that("rjd_backcast_matrix stops when no date and no maxback in spec", {
  col_dates <- generate_date_labels(2020, 1, 4, 4)
  mat <- matrix(1:4, nrow = 1, dimnames = list("s1", col_dates))
  expect_error(rjd_backcast_matrix(mat, date = NULL, periodicity = "Q"))
})

test_that("rjd_seasadj_matrix stops on annual periodicity", {
  col_labels <- generate_date_labels(2000, 1, 10, 1)
  mat <- matrix(1:10, nrow = 1, dimnames = list("s1", col_labels))
  expect_error(rjd_seasadj_matrix(mat, periodicity = "A"))
})

test_that("rjd_mave_matrix stops on frequency mismatch — monthly date with quarterly data", {
  col_dates <- generate_date_labels(2020, 1, 8, 4)
  mat <- matrix(1:8, nrow = 1, dimnames = list("s1", col_dates))
  expect_error(rjd_mave_matrix(mat, end_date = "2030May", periodicity = "Q"))
})

test_that("rjd_mave_matrix stops on frequency mismatch — start_date", {
  col_dates <- generate_date_labels(2020, 1, 8, 4)
  mat <- matrix(1:8, nrow = 1, dimnames = list("s1", col_dates))
  expect_error(rjd_mave_matrix(mat, start_date = "2015May", periodicity = "Q"))
})

test_that("rjd_mave_matrix stops when no dates and no extend", {
  col_dates <- generate_date_labels(2020, 1, 4, 4)
  mat <- matrix(1:4, nrow = 1, dimnames = list("s1", col_dates))
  expect_error(rjd_mave_matrix(mat, periodicity = "Q"))
})


# =============================================================================
# MATRIX WRAPPER INTEGRATION TESTS (require RJDemetra)
# =============================================================================

skip_if_not_installed("RJDemetra")

# --- Helper: build a matrix from ts objects ----------------------------------

build_test_matrix <- function(ts_list, target_date, freq) {
  # Determine full date range across all series + target
  all_starts <- vapply(ts_list, function(x) start(x)[1] * freq + start(x)[2], numeric(1))
  all_ends   <- vapply(ts_list, function(x) end(x)[1] * freq + end(x)[2], numeric(1))
  range_start_abs <- min(all_starts)
  range_end_abs   <- max(all_ends)

  if (!is.null(target_date)) {
    tp <- parse_target_date(target_date)
    target_abs <- tp$year * freq + tp$period
    range_end_abs <- max(range_end_abs, target_abs)
  }

  n_cols <- range_end_abs - range_start_abs + 1L
  start_year   <- (range_start_abs - 1L) %/% freq
  start_period <- range_start_abs - start_year * freq

  col_labels <- generate_date_labels(start_year, start_period, n_cols, freq)

  mat <- matrix(NA_real_, nrow = length(ts_list), ncol = n_cols,
                dimnames = list(names(ts_list), col_labels))

  for (nm in names(ts_list)) {
    s <- ts_list[[nm]]
    s_start_abs <- start(s)[1] * freq + start(s)[2]
    offset <- s_start_abs - range_start_abs + 1L
    mat[nm, offset:(offset + length(s) - 1L)] <- as.numeric(s)
  }
  mat
}


# --- Forecast matrix tests ---------------------------------------------------

test_that("rjd_forecast_matrix fills forecast values for single quarterly series", {
  set.seed(42)
  ts_obj <- create_quarterly_ts(n = 60, start_year = 2005)
  ts_list <- list(series1 = ts_obj)
  mat <- build_test_matrix(ts_list, "2021Q4", 4)

  # Before: columns past series end should be NA
  result <- rjd_forecast_matrix(mat, date = "2021Q4", periodicity = "Q")

  # Result should have same dimensions or more columns

  expect_true(ncol(result) >= ncol(mat))
  # The series should have values beyond the original end
  non_na <- sum(!is.na(result["series1", ]))
  expect_gt(non_na, sum(!is.na(mat["series1", ])))
})

test_that("rjd_forecast_matrix handles multiple series with different end dates", {
  set.seed(123)
  ts1 <- create_quarterly_ts(n = 40, start_year = 2005)  # ends ~2014Q4
  ts2 <- create_quarterly_ts(n = 48, start_year = 2005)  # ends ~2016Q4

  ts_list <- list(short = ts1, long = ts2)
  mat <- build_test_matrix(ts_list, "2018Q4", 4)

  result <- rjd_forecast_matrix(mat, date = "2018Q4", periodicity = "Q")

  # Both series should now have some forecast values filled in
  non_na_short <- sum(!is.na(result["short", ]))
  non_na_long  <- sum(!is.na(result["long", ]))
  expect_gt(non_na_short, length(ts1))
  expect_gt(non_na_long, length(ts2))

  # Short series needs more forecasts than long series
  orig_short <- sum(!is.na(mat["short", ]))
  orig_long  <- sum(!is.na(mat["long", ]))
  filled_short <- non_na_short - orig_short
  filled_long  <- non_na_long - orig_long
  expect_gt(filled_short, filled_long)
})

test_that("rjd_forecast_matrix extends matrix when target date is beyond column range", {
  set.seed(99)
  ts_obj <- create_annual_ts(n = 20, start_year = 2000)
  ts_list <- list(s1 = ts_obj)
  col_labels <- generate_date_labels(2000, 1, 20, 1)
  mat <- matrix(as.numeric(ts_obj), nrow = 1, dimnames = list("s1", col_labels))

  # Target is 2025, matrix only goes to 2019
  result <- rjd_forecast_matrix(mat, date = "2025", periodicity = "A")

  expect_true(ncol(result) > 20)
  expect_true("2025" %in% colnames(result))
})

test_that("rjd_forecast_matrix skips series already past target", {
  set.seed(55)
  ts_obj <- create_quarterly_ts(n = 60, start_year = 2005)  # ends 2019Q4
  ts_list <- list(s1 = ts_obj)
  mat <- build_test_matrix(ts_list, NULL, 4)

  # Target is 2010Q1 — well before end of series
  result <- rjd_forecast_matrix(mat, date = "2010Q1", periodicity = "Q")

  # No new values should be added
  expect_equal(sum(!is.na(result["s1", ])), sum(!is.na(mat["s1", ])))
})

test_that("rjd_forecast_matrix records error for failing series", {
  calls <- list()
  mock_fn <- function(code, names) calls[[length(calls) + 1L]] <<- list(code = code, names = names)
  old <- options(rjdx13.record_ts_error = mock_fn)
  on.exit(options(old), add = TRUE)

  # Constant series will fail RJDemetra validation
  col_labels <- generate_date_labels(2000, 1, 30, 1)
  mat <- matrix(100, nrow = 1, ncol = 30, dimnames = list("bad_series", col_labels))

  result <- rjd_forecast_matrix(mat, date = "2035", periodicity = "A")

  # Should have recorded at least one error
  expect_true(length(calls) >= 1)
  expect_equal(calls[[1]]$code, "CAS_RJD_FAIL")
  expect_equal(calls[[1]]$names, "bad_series")
})


# --- Backcast matrix tests ---------------------------------------------------

test_that("rjd_backcast_matrix fills backcast values for single quarterly series", {
  set.seed(42)
  ts_obj <- create_quarterly_ts(n = 60, start_year = 2010)
  ts_list <- list(series1 = ts_obj)
  mat <- build_test_matrix(ts_list, "2008Q1", 4)

  result <- rjd_backcast_matrix(mat, date = "2008Q1", periodicity = "Q")

  expect_true(ncol(result) >= ncol(mat))
  non_na <- sum(!is.na(result["series1", ]))
  expect_gt(non_na, sum(!is.na(mat["series1", ])))
})

test_that("rjd_backcast_matrix prepends columns when target is before range", {
  set.seed(77)
  ts_obj <- create_annual_ts(n = 20, start_year = 2000)
  ts_list <- list(s1 = ts_obj)
  col_labels <- generate_date_labels(2000, 1, 20, 1)
  mat <- matrix(as.numeric(ts_obj), nrow = 1, dimnames = list("s1", col_labels))

  result <- rjd_backcast_matrix(mat, date = "1995", periodicity = "A")

  expect_true(ncol(result) > 20)
  expect_true("1995" %in% colnames(result))
})

test_that("rjd_backcast_matrix skips series already past target", {
  set.seed(88)
  ts_obj <- create_quarterly_ts(n = 60, start_year = 2005)
  ts_list <- list(s1 = ts_obj)
  mat <- build_test_matrix(ts_list, NULL, 4)

  # Target is 2010Q1 — well after start of series (2005Q1)
  result <- rjd_backcast_matrix(mat, date = "2010Q1", periodicity = "Q")

  expect_equal(sum(!is.na(result["s1", ])), sum(!is.na(mat["s1", ])))
})

test_that("rjd_backcast_matrix records error for failing series", {
  calls <- list()
  mock_fn <- function(code, names) calls[[length(calls) + 1L]] <<- list(code = code, names = names)
  old <- options(rjdx13.record_ts_error = mock_fn)
  on.exit(options(old), add = TRUE)

  col_labels <- generate_date_labels(2010, 1, 30, 1)
  mat <- matrix(100, nrow = 1, ncol = 30, dimnames = list("bad_series", col_labels))

  result <- rjd_backcast_matrix(mat, date = "2005", periodicity = "A")

  expect_true(length(calls) >= 1)
  expect_equal(calls[[1]]$code, "CAS_RJD_FAIL")
  expect_equal(calls[[1]]$names, "bad_series")
})


# --- Seasadj matrix tests ----------------------------------------------------

test_that("rjd_seasadj_matrix replaces values with SA for single quarterly series", {
  set.seed(42)
  ts_obj <- create_quarterly_ts(n = 60, start_year = 2005)
  ts_list <- list(series1 = ts_obj)
  mat <- build_test_matrix(ts_list, NULL, 4)

  result <- rjd_seasadj_matrix(mat, periodicity = "Q")

  # Dimensions unchanged — SA replaces in place
  expect_equal(dim(result), dim(mat))
  # Values should differ (seasonally adjusted)
  expect_false(identical(result["series1", ], mat["series1", ]))
  # Same number of non-NA values
  expect_equal(sum(!is.na(result["series1", ])), sum(!is.na(mat["series1", ])))
})

test_that("rjd_seasadj_matrix handles multiple monthly series", {
  set.seed(123)
  ts1 <- create_monthly_ts(n = 60, start_year = 2010)
  ts2 <- create_monthly_ts(n = 72, start_year = 2010)

  ts_list <- list(short = ts1, long = ts2)
  mat <- build_test_matrix(ts_list, NULL, 12)

  result <- rjd_seasadj_matrix(mat, periodicity = "M")

  expect_equal(nrow(result), 2)
  # Both series should have been adjusted
  expect_false(identical(result["short", ], mat["short", ]))
  expect_false(identical(result["long", ], mat["long", ]))
})

test_that("rjd_seasadj_matrix records error for failing series", {
  calls <- list()
  mock_fn <- function(code, names) calls[[length(calls) + 1L]] <<- list(code = code, names = names)
  old <- options(rjdx13.record_ts_error = mock_fn)
  on.exit(options(old), add = TRUE)

  # Constant series will fail
  col_labels <- generate_date_labels(2010, 1, 60, 4)
  mat <- matrix(100, nrow = 1, ncol = 60, dimnames = list("bad_series", col_labels))

  result <- rjd_seasadj_matrix(mat, periodicity = "Q")

  expect_true(length(calls) >= 1)
  expect_equal(calls[[1]]$code, "CAS_RJD_FAIL")
  expect_equal(calls[[1]]$names, "bad_series")
})


# --- MAVE matrix tests -------------------------------------------------------

test_that("rjd_mave_matrix extends both ends with fixed extend", {
  set.seed(42)
  ts_obj <- create_quarterly_ts(n = 60, start_year = 2005)
  ts_list <- list(series1 = ts_obj)
  mat <- build_test_matrix(ts_list, NULL, 4)

  result <- rjd_mave_matrix(mat, periodicity = "Q", extend = 4)

  non_na_result <- sum(!is.na(result["series1", ]))
  non_na_orig   <- sum(!is.na(mat["series1", ]))
  # Should have extended by 4 on each end = 8 more non-NA values
  expect_gt(non_na_result, non_na_orig)
})

test_that("rjd_mave_matrix extends with target dates", {
  set.seed(99)
  ts_obj <- create_annual_ts(n = 20, start_year = 2000)
  ts_list <- list(s1 = ts_obj)
  col_labels <- generate_date_labels(2000, 1, 20, 1)
  mat <- matrix(as.numeric(ts_obj), nrow = 1, dimnames = list("s1", col_labels))

  result <- rjd_mave_matrix(mat, start_date = "1997", end_date = "2022",
                             periodicity = "A")

  expect_true("1997" %in% colnames(result))
  expect_true("2022" %in% colnames(result))
  expect_true(ncol(result) > 20)
})

test_that("rjd_mave_matrix extends only forward with end_date only", {
  set.seed(55)
  ts_obj <- create_quarterly_ts(n = 60, start_year = 2005)
  ts_list <- list(s1 = ts_obj)
  mat <- build_test_matrix(ts_list, NULL, 4)
  orig_non_na <- sum(!is.na(mat["s1", ]))

  result <- rjd_mave_matrix(mat, end_date = "2021Q4", periodicity = "Q")

  # Forward should be extended, backward should not
  non_na_result <- sum(!is.na(result["s1", ]))
  expect_gt(non_na_result, orig_non_na)
})

test_that("rjd_mave_matrix records error for failing series", {
  calls <- list()
  mock_fn <- function(code, names) calls[[length(calls) + 1L]] <<- list(code = code, names = names)
  old <- options(rjdx13.record_ts_error = mock_fn)
  on.exit(options(old), add = TRUE)

  col_labels <- generate_date_labels(2000, 1, 30, 1)
  mat <- matrix(100, nrow = 1, ncol = 30, dimnames = list("bad_series", col_labels))

  result <- rjd_mave_matrix(mat, periodicity = "A", extend = 3)

  expect_true(length(calls) >= 1)
  expect_equal(calls[[1]]$code, "CAS_RJD_FAIL")
  expect_equal(calls[[1]]$names, "bad_series")
})


# --- Per-series custom spec tests --------------------------------------------

test_that("rjd_forecast_matrix uses per-series custom spec files", {
  set.seed(42)
  ts1 <- create_quarterly_ts(n = 60, start_year = 2005)
  ts2 <- create_quarterly_ts(n = 60, start_year = 2005)
  ts_list <- list(seriesA = ts1, seriesB = ts2)
  mat <- build_test_matrix(ts_list, "2021Q4", 4)

  spec_dir <- testthat::test_path("fixtures", "spec_files")

  # Only seriesA gets a custom spec
  result <- rjd_forecast_matrix(mat, date = "2021Q4", periodicity = "Q",
    spec_names = c("forecast-quarterly.spc"),
    spec_folder = spec_dir,
    custom_spec_series = c("seriesA"))

  # Both series should have forecast values
  expect_gt(sum(!is.na(result["seriesA", ])), sum(!is.na(mat["seriesA", ])))
  expect_gt(sum(!is.na(result["seriesB", ])), sum(!is.na(mat["seriesB", ])))
})

test_that("rjd_backcast_matrix uses per-series custom spec files", {
  set.seed(42)
  ts1 <- create_quarterly_ts(n = 60, start_year = 2010)
  ts2 <- create_quarterly_ts(n = 60, start_year = 2010)
  ts_list <- list(seriesA = ts1, seriesB = ts2)
  mat <- build_test_matrix(ts_list, "2008Q1", 4)

  spec_dir <- testthat::test_path("fixtures", "spec_files")

  result <- rjd_backcast_matrix(mat, date = "2008Q1", periodicity = "Q",
    spec_names = c("backcast-quarterly.spc"),
    spec_folder = spec_dir,
    custom_spec_series = c("seriesA"))

  expect_gt(sum(!is.na(result["seriesA", ])), sum(!is.na(mat["seriesA", ])))
  expect_gt(sum(!is.na(result["seriesB", ])), sum(!is.na(mat["seriesB", ])))
})

test_that("rjd_seasadj_matrix uses per-series custom spec files", {
  set.seed(42)
  ts1 <- create_quarterly_ts(n = 60, start_year = 2005)
  ts2 <- create_quarterly_ts(n = 60, start_year = 2005)
  ts_list <- list(seriesA = ts1, seriesB = ts2)
  mat <- build_test_matrix(ts_list, NULL, 4)

  spec_dir <- testthat::test_path("fixtures", "spec_files")

  result <- rjd_seasadj_matrix(mat, periodicity = "Q",
    spec_names = c("seasadj_quarterly.spc"),
    spec_folder = spec_dir,
    custom_spec_series = c("seriesA"))

  # Both should be seasonally adjusted
  expect_false(identical(result["seriesA", ], mat["seriesA", ]))
  expect_false(identical(result["seriesB", ], mat["seriesB", ]))
})

test_that("rjd_forecast_matrix with no custom specs uses defaults for all series", {
  set.seed(42)
  ts_obj <- create_quarterly_ts(n = 60, start_year = 2005)
  ts_list <- list(s1 = ts_obj)
  mat <- build_test_matrix(ts_list, "2021Q4", 4)

  # No spec_names at all — all series use defaults
  result <- rjd_forecast_matrix(mat, date = "2021Q4", periodicity = "Q")

  expect_gt(sum(!is.na(result["s1", ])), sum(!is.na(mat["s1", ])))
})
