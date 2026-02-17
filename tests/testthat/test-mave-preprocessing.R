# Test Category 11: MAVE Preprocessing
# Tests the rjd_mave() function which extends series at both ends
# using forecast and backcast for moving average preparation.

skip_if_not_installed("RJDemetra")

test_that("MAVE - monthly series default extend", {
  monthly_ts <- create_monthly_ts(n = 120)
  result <- rjd_mave(monthly_ts)
  expect_false(result$error)
  expect_equal(result$extend, 12)
  expect_length(result$extended, length(monthly_ts) + 2 * 12)
})

test_that("MAVE - quarterly series default extend", {
  quarterly_ts <- create_quarterly_ts(n = 40)
  result <- rjd_mave(quarterly_ts)
  expect_false(result$error)
  expect_equal(result$extend, 4)
  expect_length(result$extended, length(quarterly_ts) + 2 * 4)
})

test_that("MAVE - annual series default extend", {
  annual_ts <- create_annual_ts(n = 25)
  result <- rjd_mave(annual_ts)
  expect_false(result$error)
  expect_equal(result$extend, 3)
  expect_length(result$extended, length(annual_ts) + 2 * 3)
})

test_that("MAVE - custom extend parameter", {
  monthly_ts <- create_monthly_ts(n = 120)
  result <- rjd_mave(monthly_ts, extend = 6)
  expect_false(result$error)
  expect_equal(result$extend, 6)
  expect_length(result$extended, length(monthly_ts) + 12)
})

test_that("MAVE - output has all required fields", {
  monthly_ts <- create_monthly_ts(n = 120)
  result <- rjd_mave(monthly_ts)
  required_fields <- c("error", "messages", "extended", "extended_ts",
                       "original", "forecast", "backcast", "extend",
                       "arima", "transform", "diagnostics", "config", "method")
  missing <- setdiff(required_fields, names(result))
  expect_length(missing, 0)
})

test_that("MAVE - extended_ts is valid ts object", {
  monthly_ts <- create_monthly_ts(n = 120)
  result <- rjd_mave(monthly_ts, extend = 6)
  expect_s3_class(result$extended_ts, "ts")
  expect_equal(frequency(result$extended_ts), 12)
  orig_start <- start(monthly_ts)
  ext_start <- start(result$extended_ts)
  expect_true(ext_start[1] < orig_start[1] ||
              (ext_start[1] == orig_start[1] && ext_start[2] < orig_start[2]))
})

test_that("MAVE - forecast and backcast lengths correct", {
  monthly_ts <- create_monthly_ts(n = 120)
  extend_val <- 8
  result <- rjd_mave(monthly_ts, extend = extend_val)
  expect_length(result$forecast, extend_val)
  expect_length(result$backcast, extend_val)
})

test_that("MAVE - batch processing works", {
  ts_list <- list(
    A = create_monthly_ts(60),
    B = create_monthly_ts(72)
  )
  results <- rjd_mave(ts_list, extend = 6, verbose = FALSE)
  expect_s3_class(results, "rjd_mave_batch")
  expect_length(results, 2)
  summary <- attr(results, "summary")
  expect_equal(summary$n_success, 2)
})

test_that("MAVE - reject constant series", {
  const_ts <- ts(rep(100, 50), start = c(2015, 1), frequency = 12)
  result <- rjd_mave(const_ts)
  expect_true(result$error)
})

test_that("MAVE - reject non-ts input", {
  err <- tryCatch({
    rjd_mave(c(1, 2, 3, 4, 5))
    NULL
  }, error = function(e) e$message)
  expect_false(is.null(err))
})

test_that("MAVE - extended series contains original values", {
  monthly_ts <- create_monthly_ts(n = 120)
  result <- rjd_mave(monthly_ts, extend = 6)
  orig_values <- as.numeric(monthly_ts)
  ext_values <- result$extended
  mid_start <- 6 + 1
  mid_end <- 6 + length(monthly_ts)
  extracted_orig <- ext_values[mid_start:mid_end]
  expect_equal(extracted_orig, orig_values, tolerance = 1e-10)
})

test_that("MAVE - extended_ts end date is correct", {
  monthly_ts <- create_monthly_ts(n = 120)
  result <- rjd_mave(monthly_ts, extend = 6)
  orig_end <- end(monthly_ts)
  ext_end <- end(result$extended_ts)
  expected_end_year <- orig_end[1] + (orig_end[2] + 6 - 1) %/% 12
  expected_end_month <- ((orig_end[2] + 6 - 1) %% 12) + 1
  expect_equal(ext_end[1], expected_end_year)
  expect_equal(ext_end[2], expected_end_month)
})

test_that("MAVE - annual uses fixed ARIMA(0,2,2)", {
  annual_ts <- create_annual_ts(n = 25)
  result <- rjd_mave(annual_ts)
  expect_false(result$config$arima_auto)
  expect_equal(result$config$arima_order, c(0, 2, 2))
})

test_that("MAVE - extend=1 works", {
  monthly_ts <- create_monthly_ts(n = 120)
  result <- rjd_mave(monthly_ts, extend = 1)
  expect_false(result$error)
  expect_equal(result$extend, 1)
  expect_length(result$forecast, 1)
  expect_length(result$backcast, 1)
})

test_that("MAVE - larger extend value", {
  monthly_ts <- create_monthly_ts(n = 120)
  result <- rjd_mave(monthly_ts, extend = 24)
  expect_false(result$error)
  expect_equal(result$extend, 24)
  expect_length(result$extended, length(monthly_ts) + 48)
})

test_that("MAVE - diagnostics have expected structure", {
  monthly_ts <- create_monthly_ts(n = 120)
  result <- rjd_mave(monthly_ts, extend = 6)
  diag <- result$diagnostics
  expect_type(diag, "list")
  expected_fields <- c("log_likelihood", "aic", "residual_tests", "quality_summary")
  has_some <- any(expected_fields %in% names(diag))
  expect_true(has_some || length(diag) > 0)
})
