# Test Category 10: Helper Functions
# Tests utility functions: extract_sa, batch_summary, extract_forecasts, etc.

skip_if_not_installed("RJDemetra")

test_that("extract_sa returns values from single result", {
  ts_data <- create_monthly_ts(60)
  result <- rjd_seasadj(ts_data)
  sa <- extract_sa(result)
  expect_type(sa, "double")
  expect_length(sa, 60)
})

test_that("extract_sa returns ts from single result", {
  ts_data <- create_monthly_ts(60)
  result <- rjd_seasadj(ts_data)
  sa_ts <- extract_sa(result, as_ts = TRUE)
  expect_s3_class(sa_ts, "ts")
})

test_that("extract_sa works on batch results", {
  ts_list <- list(
    A = create_monthly_ts(60),
    B = create_monthly_ts(72)
  )
  results <- rjd_seasadj(ts_list, verbose = FALSE)
  sa_list <- extract_sa(results)
  expect_type(sa_list, "list")
  expect_length(sa_list, 2)
})

test_that("batch_summary creates data frame", {
  ts_list <- list(
    A = create_monthly_ts(60),
    B = create_monthly_ts(72)
  )
  results <- rjd_seasadj(ts_list, verbose = FALSE)
  summary_df <- batch_summary(results)
  expect_s3_class(summary_df, "data.frame")
  expect_equal(nrow(summary_df), 2)
  expect_true("series" %in% names(summary_df))
  expect_true("success" %in% names(summary_df))
})

test_that("get_messages works on single result", {
  ts_data <- create_monthly_ts(60)
  result <- rjd_seasadj(ts_data)
  msg <- get_messages(result)
  expect_type(msg, "character")
})

test_that("get_messages works on batch results", {
  ts_list <- list(
    A = create_monthly_ts(60),
    B = create_monthly_ts(72)
  )
  results <- rjd_seasadj(ts_list, verbose = FALSE)
  msg <- get_messages(results)
  expect_type(msg, "character")
})

test_that("has_messages returns logical", {
  ts_data <- create_monthly_ts(60)
  result <- rjd_seasadj(ts_data)
  hm <- has_messages(result)
  expect_type(hm, "logical")
})

test_that("extract_forecasts works on single result", {
  ts_data <- create_monthly_ts(60)
  result <- rjd_forecast(ts_data)
  fcst <- extract_forecasts(result)
  expect_type(fcst, "double")
})

test_that("extract_backcasts works on single result", {
  ts_data <- create_monthly_ts(60)
  result <- rjd_backcast(ts_data)
  bcast <- extract_backcasts(result)
  expect_type(bcast, "double")
})

test_that("extract_sa na_for_errors=FALSE omits failed", {
  ts_mixed <- list(
    good = create_monthly_ts(60),
    bad = ts(rep(100, 50), start = c(2020, 1), frequency = 12)
  )
  results <- rjd_seasadj(ts_mixed, verbose = FALSE)
  sa_with_na <- extract_sa(results, na_for_errors = TRUE)
  sa_without_na <- extract_sa(results, na_for_errors = FALSE)
  expect_length(sa_with_na, 2)
  expect_length(sa_without_na, 1)
})

test_that("print.rjd_seasadj_batch works", {
  ts_list <- list(
    A = create_monthly_ts(60),
    B = create_monthly_ts(72)
  )
  results <- rjd_seasadj(ts_list, verbose = FALSE)
  output <- capture.output(print(results))
  expect_gt(length(output), 0)
  expect_true(any(grepl("Batch", output)))
})

test_that("print.rjd_forecast_batch works", {
  ts_list <- list(
    A = create_monthly_ts(60),
    B = create_monthly_ts(72)
  )
  results <- rjd_forecast(ts_list, verbose = FALSE)
  output <- capture.output(print(results))
  expect_gt(length(output), 0)
})

test_that("extract_extended works on single result", {
  ts_data <- create_monthly_ts(60)
  result <- rjd_mave(ts_data, extend = 6)
  ext <- extract_extended(result)
  expect_type(ext, "double")
})

test_that("extract_extended works on batch results", {
  ts_list <- list(
    A = create_monthly_ts(60),
    B = create_monthly_ts(72)
  )
  results <- rjd_mave(ts_list, extend = 6, verbose = FALSE)
  ext_list <- extract_extended(results)
  expect_type(ext_list, "list")
  expect_length(ext_list, 2)
})

test_that("extract_extended as_ts returns ts objects", {
  ts_data <- create_monthly_ts(60)
  result <- rjd_mave(ts_data, extend = 6)
  ext_ts <- extract_extended(result, as_ts = TRUE)
  expect_s3_class(ext_ts, "ts")
})

test_that("print.rjd_mave_batch works", {
  ts_list <- list(
    A = create_monthly_ts(60),
    B = create_monthly_ts(72)
  )
  results <- rjd_mave(ts_list, extend = 6, verbose = FALSE)
  output <- capture.output(print(results))
  expect_gt(length(output), 0)
})
