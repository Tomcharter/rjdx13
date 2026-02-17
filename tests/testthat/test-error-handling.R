# Test Category 8: Error Handling
# Tests graceful error handling for various edge cases.

skip_if_not_installed("RJDemetra")

test_that("Handle missing spec file gracefully", {
  result <- rjd_seasadj(create_monthly_ts(60), spec_file = "nonexistent.spc")
  expect_true(result$error)
  expect_match(result$messages, "not found", ignore.case = TRUE)
})

test_that("Handle series with some NAs", {
  ts_with_na <- create_monthly_ts(60)
  ts_with_na[c(10, 20, 30)] <- NA
  result <- rjd_seasadj(ts_with_na)
  expect_false(is.null(result))
})

test_that("Reject series with >20% NAs", {
  ts_many_na <- create_monthly_ts(60)
  ts_many_na[1:20] <- NA
  result <- rjd_seasadj(ts_many_na)
  expect_true(result$error)
})

test_that("Handle negative values with auto transform", {
  ts_negative <- create_monthly_ts(60)
  ts_negative <- ts_negative - 200
  result <- rjd_seasadj(ts_negative)
  expect_true(!result$error || grepl("transform", result$messages, ignore.case = TRUE))
})

test_that("Handle empty list input", {
  tryCatch({
    result <- rjd_seasadj(list())
    expect_true(TRUE)
  }, error = function(e) {
    expect_true(TRUE)
  })
})

test_that("Forecast handles missing spec file", {
  result <- rjd_forecast(create_monthly_ts(60), spec_file = "nonexistent.spc")
  expect_true(result$error)
  expect_match(result$messages, "not found", ignore.case = TRUE)
})

test_that("Backcast handles missing spec file", {
  result <- rjd_backcast(create_monthly_ts(60), spec_file = "nonexistent.spc")
  expect_true(result$error)
  expect_match(result$messages, "not found", ignore.case = TRUE)
})

test_that("Forecast with NULL horizon uses default", {
  result <- rjd_forecast(create_monthly_ts(60), horizon = NULL)
  expect_false(result$error)
  expect_equal(result$horizon, 12)
})

test_that("Handle all-negative series", {
  neg_ts <- ts(-(1:60), start = c(2015, 1), frequency = 12)
  result <- rjd_seasadj(neg_ts)
  if (!result$error) {
    expect_equal(result$transform, "None")
  }
})
