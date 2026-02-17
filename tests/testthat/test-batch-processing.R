# Test Category 5: Batch Processing
# Tests batch processing for seasadj, forecast, backcast, and mave.

skip_if_not_installed("RJDemetra")

test_that("Batch seasadj - process multiple series", {
  ts_list <- list(
    series_A = create_monthly_ts(n = 60, start_year = 2015),
    series_B = create_monthly_ts(n = 84, start_year = 2012),
    series_C = create_monthly_ts(n = 120, start_year = 2010)
  )
  results <- rjd_seasadj(ts_list, verbose = FALSE)
  expect_s3_class(results, "rjd_seasadj_batch")
  expect_length(results, 3)
  expect_false(is.null(attr(results, "summary")))

  summary <- attr(results, "summary")
  expect_equal(summary$n_series, 3)
})

test_that("Batch seasadj - all series succeed", {
  ts_list <- list(
    series_A = create_monthly_ts(n = 60, start_year = 2015),
    series_B = create_monthly_ts(n = 84, start_year = 2012),
    series_C = create_monthly_ts(n = 120, start_year = 2010)
  )
  results <- rjd_seasadj(ts_list, verbose = FALSE)
  summary <- attr(results, "summary")
  expect_equal(summary$n_success, 3)
  expect_equal(summary$n_failed, 0)
})

test_that("Batch seasadj - individual results accessible", {
  ts_list <- list(
    series_A = create_monthly_ts(n = 60, start_year = 2015),
    series_B = create_monthly_ts(n = 84, start_year = 2012),
    series_C = create_monthly_ts(n = 120, start_year = 2010)
  )
  results <- rjd_seasadj(ts_list, verbose = FALSE)
  expect_false(is.null(results$series_A))
  expect_false(is.null(results[["series_B"]]))
  expect_length(results$series_A$sa, 60)
})

test_that("Batch seasadj - handle mixed success/failure", {
  ts_list_mixed <- list(
    good_series = create_monthly_ts(n = 60),
    bad_series = ts(rep(100, 50), start = c(2020, 1), frequency = 12),
    another_good = create_monthly_ts(n = 72)
  )
  results <- rjd_seasadj(ts_list_mixed, verbose = FALSE)
  summary <- attr(results, "summary")
  expect_equal(summary$n_success, 2)
  expect_equal(summary$n_failed, 1)
  expect_true(results$bad_series$error)
})

test_that("Batch forecast - process multiple series", {
  ts_list <- list(
    series_A = create_monthly_ts(n = 60, start_year = 2015),
    series_B = create_monthly_ts(n = 84, start_year = 2012),
    series_C = create_monthly_ts(n = 120, start_year = 2010)
  )
  results <- rjd_forecast(ts_list, verbose = FALSE)
  expect_s3_class(results, "rjd_forecast_batch")
  expect_length(results, 3)
})

test_that("Batch backcast - process multiple series", {
  ts_list <- list(
    series_A = create_monthly_ts(n = 60, start_year = 2015),
    series_B = create_monthly_ts(n = 84, start_year = 2012),
    series_C = create_monthly_ts(n = 120, start_year = 2010)
  )
  results <- rjd_backcast(ts_list, verbose = FALSE)
  expect_s3_class(results, "rjd_backcast_batch")
  expect_length(results, 3)
})

test_that("Batch seasadj - unnamed list gets numeric names", {
  unnamed_list <- list(create_monthly_ts(60), create_monthly_ts(72))
  results <- rjd_seasadj(unnamed_list, verbose = FALSE)
  expect_equal(names(results)[1], "1")
  expect_equal(names(results)[2], "2")
})

test_that("Batch mave - process multiple series", {
  ts_list <- list(
    series_A = create_monthly_ts(n = 60, start_year = 2015),
    series_B = create_monthly_ts(n = 84, start_year = 2012),
    series_C = create_monthly_ts(n = 120, start_year = 2010)
  )
  results <- rjd_mave(ts_list, extend = 6, verbose = FALSE)
  expect_s3_class(results, "rjd_mave_batch")
  expect_length(results, 3)
})

test_that("Batch mave - handle mixed success/failure", {
  ts_list_mixed <- list(
    good = create_monthly_ts(60),
    bad = ts(rep(100, 30), start = c(2020, 1), frequency = 12)
  )
  results <- rjd_mave(ts_list_mixed, verbose = FALSE)
  summary <- attr(results, "summary")
  expect_equal(summary$n_success, 1)
  expect_equal(summary$n_failed, 1)
})
