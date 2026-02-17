# Test Category 4: Single Series Processing
# Tests single series processing for seasadj, forecast, and backcast.

skip_if_not_installed("RJDemetra")

test_that("Seasonal adjustment - monthly", {
  monthly_ts <- create_monthly_ts(n = 120)
  result <- rjd_seasadj(monthly_ts)
  expect_false(result$error)
  expect_length(result$sa, length(monthly_ts))
  expect_false(is.null(result$trend))
  expect_false(is.null(result$seasonal))
})

test_that("Seasonal adjustment - quarterly", {
  quarterly_ts <- create_quarterly_ts(n = 40)
  result <- rjd_seasadj(quarterly_ts)
  expect_false(result$error)
  expect_length(result$sa, length(quarterly_ts))
})

test_that("Forecast - monthly default horizon", {
  monthly_ts <- create_monthly_ts(n = 120)
  result <- rjd_forecast(monthly_ts)
  expect_false(result$error)
  expect_length(result$forecast, 12)
})

test_that("Forecast - quarterly default horizon", {
  quarterly_ts <- create_quarterly_ts(n = 40)
  result <- rjd_forecast(quarterly_ts)
  expect_false(result$error)
  expect_length(result$forecast, 4)
})

test_that("Forecast - custom horizon", {
  monthly_ts <- create_monthly_ts(n = 120)
  result <- rjd_forecast(monthly_ts, horizon = 6)
  expect_false(result$error)
  expect_length(result$forecast, 6)
})

test_that("Backcast - monthly default horizon", {
  monthly_ts <- create_monthly_ts(n = 120)
  result <- rjd_backcast(monthly_ts)
  expect_false(result$error)
  expect_length(result$backcast, 12)
})

test_that("Backcast - quarterly default horizon", {
  quarterly_ts <- create_quarterly_ts(n = 40)
  result <- rjd_backcast(quarterly_ts)
  expect_false(result$error)
  expect_length(result$backcast, 4)
})

test_that("Backcast - custom horizon", {
  quarterly_ts <- create_quarterly_ts(n = 40)
  result <- rjd_backcast(quarterly_ts, horizon = 8)
  expect_false(result$error)
  expect_length(result$backcast, 8)
})

test_that("Seasadj - series starting mid-year", {
  mid_year_ts <- ts(create_monthly_ts(60), start = c(2015, 7), frequency = 12)
  result <- rjd_seasadj(mid_year_ts)
  expect_false(result$error)
  expect_equal(start(result$ts), c(2015, 7))
})

test_that("Forecast - series starting Q3", {
  q3_ts <- ts(create_quarterly_ts(30), start = c(2010, 3), frequency = 4)
  result <- rjd_forecast(q3_ts, horizon = 4)
  expect_false(result$error)
})

test_that("Seasadj - components are consistent with original", {
  monthly_ts <- create_monthly_ts(n = 120)
  result <- rjd_seasadj(monthly_ts)
  if (!result$error && result$x11_mode == "mult" &&
      !is.null(result$seasonal) && !is.null(result$irregular)) {
    reconstructed <- result$trend * result$seasonal * result$irregular
    orig_values <- as.numeric(result$original)
    rel_error <- abs(reconstructed - orig_values) / abs(orig_values)
    max_rel_error <- max(rel_error, na.rm = TRUE)
    expect_lt(max_rel_error, 0.05)
  }
})
