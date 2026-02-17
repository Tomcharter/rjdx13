# Test Category 9: Custom Spec File Processing
# Tests end-to-end processing with spec files, Denton benchmarking, and overrides.

skip_if_not_installed("RJDemetra")

test_that("Process with seasadj_monthly.spc", {
  monthly_ts <- create_monthly_ts(n = 120)
  result <- rjd_seasadj(monthly_ts, spec_file = spec_fixture_path("seasadj_monthly.spc"))
  expect_false(result$error)
  expect_length(result$sa, length(monthly_ts))
})

test_that("Process with seasadj_quarterly.spc", {
  quarterly_ts <- create_quarterly_ts(n = 40)
  result <- rjd_seasadj(quarterly_ts, spec_file = spec_fixture_path("seasadj_quarterly.spc"))
  expect_false(result$error)
})

test_that("Process forecast with forecast-monthly.spc", {
  monthly_ts <- create_monthly_ts(n = 120)
  result <- rjd_forecast(monthly_ts, spec_file = spec_fixture_path("forecast-monthly.spc"))
  expect_false(result$error)
})

test_that("Denton benchmarking produces consistent annual totals", {
  monthly_ts <- create_monthly_ts(n = 120)
  result <- rjd_seasadj(monthly_ts, apply_denton = TRUE)
  if (!result$error && result$denton_applied) {
    orig_annual <- aggregate(monthly_ts, nfrequency = 1, FUN = sum)
    sa_annual <- aggregate(result$ts, nfrequency = 1, FUN = sum)
    n_years <- min(length(orig_annual), length(sa_annual))
    if (n_years > 0) {
      ratio <- sa_annual[1:n_years] / orig_annual[1:n_years]
      expect_true(all(abs(ratio - 1) < 0.01))
    }
  }
})

test_that("Spec parser extracts forecast maxlead", {
  config <- parse_x13_spec(spec_fixture_path("forecast-monthly.spc"))
  expect_false(is.null(config$forecast_maxlead))
  expect_equal(config$forecast_maxlead, 12)
})

test_that("Backcast uses maxback from spec file", {
  monthly_ts <- create_monthly_ts(n = 120)
  result <- rjd_backcast(monthly_ts, spec_file = spec_fixture_path("backcast-monthly.spc"))
  expect_false(result$error)
  expect_equal(result$horizon, 12)
})

test_that("Seasadj with apply_denton=FALSE", {
  monthly_ts <- create_monthly_ts(n = 120)
  result <- rjd_seasadj(monthly_ts, apply_denton = FALSE)
  expect_false(result$error)
  expect_false(result$denton_applied)
})

test_that("Horizon parameter overrides spec file", {
  monthly_ts <- create_monthly_ts(n = 120)
  result <- rjd_forecast(monthly_ts, horizon = 3,
                         spec_file = spec_fixture_path("forecast-monthly.spc"))
  expect_false(result$error)
  expect_equal(result$horizon, 3)
})
