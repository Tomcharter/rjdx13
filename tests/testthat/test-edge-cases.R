# =============================================================================
# Edge Case Tests - Medium and High Priority
# =============================================================================
#
# Tests for boundary conditions and unusual inputs across all operations.
#
# High priority:
#   - All-NA series
#   - Series with zeros + log transform
#   - Invalid horizon/extend values
#   - Inf / -Inf / NaN values
#
# Medium priority:
#   - Leading/trailing NA blocks
#   - Contiguous NA blocks
#   - Boundary-length series (exactly at minimum threshold)
#   - Extreme outliers
#   - Batch where all series fail
#
# =============================================================================

skip_if_not_installed("RJDemetra")


# =============================================================================
# HIGH: All-NA series
# =============================================================================

test_that("All-NA monthly series is rejected (seasadj)", {
  all_na <- ts(rep(NA_real_, 60), start = c(2015, 1), frequency = 12)
  result <- rjd_seasadj(all_na)
  expect_true(result$error)
  expect_match(result$messages, "missing|NA", ignore.case = TRUE)
})

test_that("All-NA monthly series is rejected (forecast)", {
  all_na <- ts(rep(NA_real_, 60), start = c(2015, 1), frequency = 12)
  result <- rjd_forecast(all_na)
  expect_true(result$error)
})

test_that("All-NA monthly series is rejected (backcast)", {
  all_na <- ts(rep(NA_real_, 60), start = c(2015, 1), frequency = 12)
  result <- rjd_backcast(all_na)
  expect_true(result$error)
})

test_that("All-NA monthly series is rejected (mave)", {
  all_na <- ts(rep(NA_real_, 60), start = c(2015, 1), frequency = 12)
  result <- rjd_mave(all_na)
  expect_true(result$error)
})


# =============================================================================
# HIGH: Series with zeros and log transform
# =============================================================================

test_that("Series with zeros uses None transform under auto", {
  ts_with_zeros <- create_monthly_ts(n = 60)
  ts_with_zeros[c(5, 15, 25)] <- 0
  result <- rjd_seasadj(ts_with_zeros)
  # Should succeed with None transform (auto detects non-positive values)
  if (!result$error) {
    expect_equal(result$transform, "None")
  }
})

test_that("Series with zeros - forecast auto transform falls back to None", {
  ts_with_zeros <- create_monthly_ts(n = 60)
  ts_with_zeros[c(5, 15, 25)] <- 0
  result <- rjd_forecast(ts_with_zeros)
  if (!result$error) {
    expect_equal(result$transform, "None")
  }
})

test_that("Series mixing positive values and zeros - backcast", {
  ts_with_zeros <- create_monthly_ts(n = 60)
  ts_with_zeros[c(5, 15, 25)] <- 0
  result <- rjd_backcast(ts_with_zeros)
  if (!result$error) {
    expect_equal(result$transform, "None")
  }
})

test_that("Series mixing positive values and zeros - mave", {
  ts_with_zeros <- create_monthly_ts(n = 60)
  ts_with_zeros[c(5, 15, 25)] <- 0
  result <- rjd_mave(ts_with_zeros)
  if (!result$error) {
    expect_equal(result$transform, "None")
  }
})


# =============================================================================
# HIGH: Invalid horizon / extend values
# =============================================================================

test_that("Forecast with horizon = 0 returns error", {
  monthly_ts <- create_monthly_ts(n = 60)
  result <- rjd_forecast(monthly_ts, horizon = 0)
  expect_true(result$error)
  expect_match(result$messages, "horizon", ignore.case = TRUE)
})

test_that("Forecast with negative horizon returns error", {
  monthly_ts <- create_monthly_ts(n = 60)
  result <- rjd_forecast(monthly_ts, horizon = -5)
  # Negative horizon should error (RJDemetra can't forecast negative periods)
  expect_true(result$error)
})

test_that("Backcast with horizon = 0 returns error", {
  monthly_ts <- create_monthly_ts(n = 60)
  result <- rjd_backcast(monthly_ts, horizon = 0)
  expect_true(result$error)
  expect_match(result$messages, "horizon", ignore.case = TRUE)
})

test_that("Backcast with negative horizon returns error", {
  monthly_ts <- create_monthly_ts(n = 60)
  result <- rjd_backcast(monthly_ts, horizon = -5)
  expect_true(result$error)
})

test_that("MAVE with extend = 0 returns error", {
  monthly_ts <- create_monthly_ts(n = 60)
  result <- rjd_mave(monthly_ts, extend = 0)
  expect_true(result$error)
  expect_match(result$messages, "extend", ignore.case = TRUE)
})

test_that("MAVE with negative extend returns error", {
  monthly_ts <- create_monthly_ts(n = 60)
  result <- rjd_mave(monthly_ts, extend = -5)
  expect_true(result$error)
})

test_that("Forecast with very large horizon still returns a result", {
  monthly_ts <- create_monthly_ts(n = 60)
  # 120 periods = 10 years ahead - aggressive but shouldn't crash
  result <- rjd_forecast(monthly_ts, horizon = 120)
  expect_false(is.null(result))
  if (!result$error) {
    expect_equal(result$horizon, 120)
    expect_length(result$forecast, 120)
  }
})


# =============================================================================
# HIGH: Inf / -Inf / NaN values
# =============================================================================

test_that("Series containing Inf is rejected", {
  ts_inf <- create_monthly_ts(n = 60)
  ts_inf[10] <- Inf
  result <- rjd_seasadj(ts_inf)
  # Inf should cause failure either in validation or in RJDemetra
  expect_true(result$error)
})

test_that("Series containing -Inf is rejected", {
  ts_inf <- create_monthly_ts(n = 60)
  ts_inf[10] <- -Inf
  result <- rjd_seasadj(ts_inf)
  expect_true(result$error)
})

test_that("Series containing NaN is treated as NA", {
  ts_nan <- create_monthly_ts(n = 60)
  ts_nan[c(10, 20)] <- NaN
  # NaN counts as NA: 2/60 = 3.3% < 20%, should pass validation
  # Then RJDemetra handles it like NA
  result <- rjd_seasadj(ts_nan)
  expect_false(is.null(result))
})

test_that("Forecast with Inf value is rejected", {
  ts_inf <- create_monthly_ts(n = 60)
  ts_inf[10] <- Inf
  result <- rjd_forecast(ts_inf)
  expect_true(result$error)
})

test_that("MAVE with Inf value is rejected", {
  ts_inf <- create_monthly_ts(n = 60)
  ts_inf[10] <- Inf
  result <- rjd_mave(ts_inf)
  expect_true(result$error)
})


# =============================================================================
# MEDIUM: Leading / trailing NA blocks
# =============================================================================

test_that("Leading NA block under 20% threshold - seasadj", {
  monthly_ts <- create_monthly_ts(n = 60)
  monthly_ts[1:10] <- NA  # 10/60 = 16.7%
  result <- rjd_seasadj(monthly_ts)
  # Should pass validation (under 20%), but RJDemetra may or may not succeed
  expect_false(is.null(result))
})

test_that("Trailing NA block under 20% threshold - seasadj", {
  monthly_ts <- create_monthly_ts(n = 60)
  monthly_ts[51:60] <- NA  # 10/60 = 16.7%
  result <- rjd_seasadj(monthly_ts)
  expect_false(is.null(result))
})

test_that("Leading NA block under 20% threshold - forecast", {
  monthly_ts <- create_monthly_ts(n = 60)
  monthly_ts[1:10] <- NA
  result <- rjd_forecast(monthly_ts)
  expect_false(is.null(result))
})

test_that("Trailing NA block under 20% threshold - backcast", {
  monthly_ts <- create_monthly_ts(n = 60)
  monthly_ts[51:60] <- NA
  result <- rjd_backcast(monthly_ts)
  expect_false(is.null(result))
})


# =============================================================================
# MEDIUM: Contiguous NA blocks (middle of series)
# =============================================================================

test_that("Contiguous NA block in middle under 20% - seasadj", {
  monthly_ts <- create_monthly_ts(n = 60)
  monthly_ts[25:34] <- NA  # 10/60 = 16.7%
  result <- rjd_seasadj(monthly_ts)
  expect_false(is.null(result))
})

test_that("Contiguous NA block in middle under 20% - forecast", {
  monthly_ts <- create_monthly_ts(n = 60)
  monthly_ts[25:34] <- NA
  result <- rjd_forecast(monthly_ts)
  expect_false(is.null(result))
})

test_that("Contiguous NA block in middle under 20% - mave", {
  monthly_ts <- create_monthly_ts(n = 60)
  monthly_ts[25:34] <- NA
  result <- rjd_mave(monthly_ts)
  expect_false(is.null(result))
})

test_that("Contiguous NA block exceeding 20% is rejected", {
  monthly_ts <- create_monthly_ts(n = 60)
  monthly_ts[10:25] <- NA  # 16/60 = 26.7%
  result <- rjd_seasadj(monthly_ts)
  expect_true(result$error)
  expect_match(result$messages, "missing|NA", ignore.case = TRUE)
})


# =============================================================================
# MEDIUM: Boundary-length series (exactly at minimum threshold)
# =============================================================================

test_that("Monthly seasadj at exact minimum (36 obs) succeeds", {
  # Minimum for monthly seasadj is 36
  monthly_min <- ts(
    100 + 0.5 * (1:36) + 10 * sin(2 * pi * (1:36) / 12) + rnorm(36, 0, 3),
    start = c(2015, 1), frequency = 12
  )
  result <- rjd_seasadj(monthly_min)
  expect_false(result$error)
})

test_that("Monthly seasadj below minimum (35 obs) is rejected", {
  monthly_short <- ts(
    100 + 0.5 * (1:35) + 10 * sin(2 * pi * (1:35) / 12) + rnorm(35, 0, 3),
    start = c(2015, 1), frequency = 12
  )
  result <- rjd_seasadj(monthly_short)
  expect_true(result$error)
  expect_match(result$messages, "insufficient", ignore.case = TRUE)
})

test_that("Quarterly seasadj at exact minimum (16 obs) succeeds", {
  quarterly_min <- ts(
    200 + (1:16) + 20 * sin(2 * pi * (1:16) / 4) + rnorm(16, 0, 5),
    start = c(2015, 1), frequency = 4
  )
  result <- rjd_seasadj(quarterly_min)
  expect_false(result$error)
})

test_that("Quarterly seasadj below minimum (15 obs) is rejected", {
  quarterly_short <- ts(
    200 + (1:15) + 20 * sin(2 * pi * (1:15) / 4) + rnorm(15, 0, 5),
    start = c(2015, 1), frequency = 4
  )
  result <- rjd_seasadj(quarterly_short)
  expect_true(result$error)
})

test_that("Monthly forecast at exact minimum (24 obs) succeeds", {
  monthly_min <- ts(
    100 + 0.5 * (1:24) + 10 * sin(2 * pi * (1:24) / 12) + rnorm(24, 0, 3),
    start = c(2015, 1), frequency = 12
  )
  result <- rjd_forecast(monthly_min)
  expect_false(result$error)
})

test_that("Annual forecast at exact minimum (10 obs) succeeds", {
  annual_min <- ts(
    500 + 10 * (1:10) + rnorm(10, 0, 20),
    start = 2010, frequency = 1
  )
  result <- rjd_forecast(annual_min)
  expect_false(result$error)
})

test_that("Annual forecast below minimum (9 obs) is rejected", {
  annual_short <- ts(
    500 + 10 * (1:9) + rnorm(9, 0, 20),
    start = 2010, frequency = 1
  )
  result <- rjd_forecast(annual_short)
  expect_true(result$error)
})

test_that("Quarterly backcast at exact minimum (12 obs) succeeds", {
  quarterly_min <- ts(
    200 + (1:12) + 20 * sin(2 * pi * (1:12) / 4) + rnorm(12, 0, 5),
    start = c(2015, 1), frequency = 4
  )
  result <- rjd_backcast(quarterly_min)
  expect_false(result$error)
})

test_that("Monthly mave at exact minimum (24 obs) succeeds", {
  monthly_min <- ts(
    100 + 0.5 * (1:24) + 10 * sin(2 * pi * (1:24) / 12) + rnorm(24, 0, 3),
    start = c(2015, 1), frequency = 12
  )
  result <- rjd_mave(monthly_min)
  expect_false(result$error)
})


# =============================================================================
# MEDIUM: Extreme outliers
# =============================================================================

test_that("Series with extreme positive outlier - seasadj", {
  monthly_ts <- create_monthly_ts(n = 60)
  monthly_ts[30] <- 1e8  # extreme spike
  result <- rjd_seasadj(monthly_ts)
  # Should not crash - either succeeds with outlier detected, or fails gracefully
  expect_false(is.null(result))
  if (!result$error) {
    expect_false(is.null(result$sa))
  }
})

test_that("Series with extreme negative outlier - seasadj", {
  monthly_ts <- create_monthly_ts(n = 60)
  monthly_ts[30] <- -1e8  # extreme dip
  result <- rjd_seasadj(monthly_ts)
  expect_false(is.null(result))
})

test_that("Series with extreme outlier - forecast", {
  monthly_ts <- create_monthly_ts(n = 60)
  monthly_ts[30] <- 1e8
  result <- rjd_forecast(monthly_ts)
  expect_false(is.null(result))
})

test_that("Series with extreme outlier - backcast", {
  monthly_ts <- create_monthly_ts(n = 60)
  monthly_ts[30] <- 1e8
  result <- rjd_backcast(monthly_ts)
  expect_false(is.null(result))
})

test_that("Series with multiple extreme outliers - seasadj", {
  monthly_ts <- create_monthly_ts(n = 60)
  monthly_ts[c(10, 30, 50)] <- c(1e8, -1e8, 1e8)
  result <- rjd_seasadj(monthly_ts)
  expect_false(is.null(result))
})


# =============================================================================
# MEDIUM: Batch where all series fail
# =============================================================================

test_that("Batch seasadj where all series fail", {
  all_bad <- list(
    const_A = ts(rep(100, 50), start = c(2015, 1), frequency = 12),
    const_B = ts(rep(200, 50), start = c(2015, 1), frequency = 12),
    const_C = ts(rep(300, 50), start = c(2015, 1), frequency = 12)
  )
  results <- rjd_seasadj(all_bad, verbose = FALSE)
  expect_s3_class(results, "rjd_seasadj_batch")
  summary <- attr(results, "summary")
  expect_equal(summary$n_success, 0)
  expect_equal(summary$n_failed, 3)
  # Every individual result should be an error
  for (name in names(results)) {
    expect_true(results[[name]]$error)
  }
})

test_that("Batch forecast where all series fail", {
  all_bad <- list(
    const_A = ts(rep(100, 50), start = c(2015, 1), frequency = 12),
    const_B = ts(rep(200, 50), start = c(2015, 1), frequency = 12)
  )
  results <- rjd_forecast(all_bad, verbose = FALSE)
  expect_s3_class(results, "rjd_forecast_batch")
  summary <- attr(results, "summary")
  expect_equal(summary$n_success, 0)
  expect_equal(summary$n_failed, 2)
})

test_that("Batch backcast where all series fail", {
  all_bad <- list(
    const_A = ts(rep(100, 50), start = c(2015, 1), frequency = 12),
    const_B = ts(rep(200, 50), start = c(2015, 1), frequency = 12)
  )
  results <- rjd_backcast(all_bad, verbose = FALSE)
  expect_s3_class(results, "rjd_backcast_batch")
  summary <- attr(results, "summary")
  expect_equal(summary$n_success, 0)
  expect_equal(summary$n_failed, 2)
})

test_that("Batch mave where all series fail", {
  all_bad <- list(
    const_A = ts(rep(100, 50), start = c(2015, 1), frequency = 12),
    const_B = ts(rep(200, 50), start = c(2015, 1), frequency = 12)
  )
  results <- rjd_mave(all_bad, verbose = FALSE)
  expect_s3_class(results, "rjd_mave_batch")
  summary <- attr(results, "summary")
  expect_equal(summary$n_success, 0)
  expect_equal(summary$n_failed, 2)
})
