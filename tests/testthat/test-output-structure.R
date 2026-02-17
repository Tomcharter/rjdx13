# Test Category 6: Output Structure Validation
# Tests that output objects contain all required fields with correct types.

skip_if_not_installed("RJDemetra")

test_that("Seasadj output has all required fields", {
  monthly_ts <- create_monthly_ts(n = 120)
  result <- rjd_seasadj(monthly_ts)
  required_fields <- c("error", "messages", "sa", "ts", "original",
                       "trend", "seasonal", "irregular", "arima",
                       "outliers", "transform", "x11_mode", "denton_applied",
                       "diagnostics", "config", "method")
  missing <- setdiff(required_fields, names(result))
  expect_length(missing, 0)
})

test_that("Seasadj output ts object is valid", {
  monthly_ts <- create_monthly_ts(n = 120)
  result <- rjd_seasadj(monthly_ts)
  expect_s3_class(result$ts, "ts")
  expect_equal(frequency(result$ts), 12)
  expect_equal(start(result$ts), start(monthly_ts))
})

test_that("Seasadj ARIMA order structure", {
  monthly_ts <- create_monthly_ts(n = 120)
  result <- rjd_seasadj(monthly_ts)
  expect_length(result$arima$order, 3)
  expect_length(result$arima$seasonal, 3)
})

test_that("Forecast output has all required fields", {
  monthly_ts <- create_monthly_ts(n = 120)
  result <- rjd_forecast(monthly_ts)
  required_fields <- c("error", "messages", "forecast", "forecast_ts",
                       "horizon", "original", "arima", "outliers",
                       "transform", "diagnostics", "config", "method")
  missing <- setdiff(required_fields, names(result))
  expect_length(missing, 0)
})

test_that("Forecast ts starts after original ends", {
  monthly_ts <- create_monthly_ts(n = 120)
  result <- rjd_forecast(monthly_ts)
  orig_end <- end(monthly_ts)
  fcst_start <- start(result$forecast_ts)
  expected_start <- if (orig_end[2] == 12) c(orig_end[1] + 1, 1) else c(orig_end[1], orig_end[2] + 1)
  expect_equal(fcst_start, expected_start)
})

test_that("Backcast output has all required fields", {
  monthly_ts <- create_monthly_ts(n = 120)
  result <- rjd_backcast(monthly_ts)
  required_fields <- c("error", "messages", "backcast", "backcast_ts",
                       "horizon", "original", "arima", "outliers",
                       "transform", "diagnostics", "config", "method")
  missing <- setdiff(required_fields, names(result))
  expect_length(missing, 0)
})

test_that("Backcast ts ends before original starts", {
  monthly_ts <- create_monthly_ts(n = 120)
  result <- rjd_backcast(monthly_ts)
  orig_start <- start(monthly_ts)
  bcast_end <- end(result$backcast_ts)
  expected_end <- if (orig_start[2] == 1) c(orig_start[1] - 1, 12) else c(orig_start[1], orig_start[2] - 1)
  expect_equal(bcast_end, expected_end)
})

test_that("Seasadj outliers structure is correct", {
  monthly_ts <- create_monthly_ts(n = 120)
  result <- rjd_seasadj(monthly_ts)
  expect_type(result$outliers, "list")
  expect_true("types" %in% names(result$outliers))
  expect_true("dates" %in% names(result$outliers))
  expect_type(result$outliers$types, "character")
  expect_type(result$outliers$dates, "character")
})

test_that("Seasadj config is attached to result", {
  monthly_ts <- create_monthly_ts(n = 120)
  result <- rjd_seasadj(monthly_ts)
  expect_false(is.null(result$config))
  expect_type(result$config, "list")
  expect_equal(result$config$periodicity, 12)
})

test_that("Forecast values are numeric and finite", {
  monthly_ts <- create_monthly_ts(n = 120)
  result <- rjd_forecast(monthly_ts)
  expect_type(result$forecast, "double")
  expect_true(all(is.finite(result$forecast)))
})

test_that("Backcast values are numeric and finite", {
  monthly_ts <- create_monthly_ts(n = 120)
  result <- rjd_backcast(monthly_ts)
  expect_type(result$backcast, "double")
  expect_true(all(is.finite(result$backcast)))
})

test_that("MAVE output has all required fields", {
  monthly_ts <- create_monthly_ts(n = 120)
  result <- rjd_mave(monthly_ts, extend = 6)
  required_fields <- c("error", "messages", "extended", "extended_ts",
                       "original", "forecast", "backcast", "extend",
                       "arima", "transform", "diagnostics", "config", "method")
  missing <- setdiff(required_fields, names(result))
  expect_length(missing, 0)
})
