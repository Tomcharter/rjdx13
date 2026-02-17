# Test Category 1: Input Validation
# Tests that invalid inputs are properly rejected across all operations.

skip_if_not_installed("RJDemetra")

test_that("Reject non-ts input (seasadj)", {
  err <- tryCatch({
    rjd_seasadj(c(1, 2, 3, 4, 5))
    NULL
  }, error = function(e) e$message)
  expect_false(is.null(err))
  expect_match(err, "ts", ignore.case = TRUE)
})

test_that("Reject non-ts input (forecast)", {
  err <- tryCatch({
    rjd_forecast(c(1, 2, 3, 4, 5))
    NULL
  }, error = function(e) e$message)
  expect_false(is.null(err))
})

test_that("Reject non-ts input (backcast)", {
  err <- tryCatch({
    rjd_backcast(c(1, 2, 3, 4, 5))
    NULL
  }, error = function(e) e$message)
  expect_false(is.null(err))
})

test_that("Reject annual frequency for seasadj", {
  annual_ts <- ts(1:20, start = 2000, frequency = 1)
  result <- rjd_seasadj(annual_ts)
  expect_true(result$error)
  expect_match(result$messages, "freq", ignore.case = TRUE)
})

test_that("Reject insufficient monthly data", {
  short_ts <- ts(1:20, start = c(2020, 1), frequency = 12)
  result <- rjd_seasadj(short_ts)
  expect_true(result$error)
  expect_match(result$messages, "insufficient", ignore.case = TRUE)
})

test_that("Reject insufficient quarterly data", {
  short_ts <- ts(1:10, start = c(2020, 1), frequency = 4)
  result <- rjd_seasadj(short_ts)
  expect_true(result$error)
})

test_that("Reject constant series", {
  const_ts <- ts(rep(100, 50), start = c(2015, 1), frequency = 12)
  result <- rjd_seasadj(const_ts)
  expect_true(result$error)
  expect_match(result$messages, "constant", ignore.case = TRUE)
})

test_that("Accept valid monthly ts", {
  valid_ts <- create_monthly_ts(n = 60)
  result <- rjd_seasadj(valid_ts)
  expect_false(result$error)
})

test_that("Accept valid quarterly ts", {
  valid_ts <- create_quarterly_ts(n = 24)
  result <- rjd_seasadj(valid_ts)
  expect_false(result$error)
})

test_that("Reject near-constant series", {
  near_const_ts <- ts(100 + rnorm(60, 0, 0.0001), start = c(2015, 1), frequency = 12)
  result <- rjd_seasadj(near_const_ts)
  expect_true(result$error)
})

test_that("Reject insufficient data for forecast", {
  short_ts <- ts(1:15, start = c(2020, 1), frequency = 12)
  result <- rjd_forecast(short_ts)
  expect_true(result$error)
})

test_that("Reject insufficient data for backcast", {
  short_ts <- ts(1:8, start = c(2020, 1), frequency = 4)
  result <- rjd_backcast(short_ts)
  expect_true(result$error)
})

test_that("MAVE rejects bimonthly frequency", {
  bimonthly_ts <- ts(1:30, start = c(2020, 1), frequency = 6)
  result <- rjd_mave(bimonthly_ts)
  expect_true(result$error)
})

test_that("MAVE accepts annual data", {
  annual_ts <- create_annual_ts(n = 20)
  result <- rjd_mave(annual_ts)
  expect_false(result$error)
})
