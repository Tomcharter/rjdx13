# Test Category 2: Default Configuration
# Tests that default config functions return correct settings.
# No RJDemetra required - these test pure R functions.

test_that("Monthly default config matches spec", {
  config <- get_default_config(12)
  expect_equal(config$periodicity, 12)
  expect_equal(config$transform, "auto")
  expect_true(config$arima_auto)
  expect_true(config$trading_days)
  expect_true(config$easter)
  expect_true(config$outlier_ao)
  expect_true(config$outlier_ls)
  expect_true(config$force_enabled)
  expect_equal(config$forecast_maxlead, 12)
})

test_that("Quarterly default config matches spec", {
  config <- get_default_config(4)
  expect_equal(config$periodicity, 4)
  expect_false(config$trading_days)
  expect_equal(config$forecast_maxlead, 4)
})

test_that("Forecast default config - monthly", {
  config <- get_forecast_config(12)
  expect_true(config$arima_auto)
  expect_equal(config$forecast_horizon, 12)
  expect_false(config$force_enabled)
})

test_that("Forecast default config - annual", {
  config <- get_forecast_config(1)
  expect_false(config$arima_auto)
  expect_equal(config$arima_order, c(0, 2, 2))
  expect_equal(config$forecast_horizon, 3)
  expect_false(config$easter)
  expect_false(config$trading_days)
})

test_that("Backcast default config", {
  config <- get_backcast_config(4)
  expect_true(config$arima_auto)
  expect_equal(config$backcast_horizon, 4)
  expect_false(config$force_enabled)
})

test_that("MAVE default config - monthly", {
  config <- get_mave_config(12)
  expect_true(config$arima_auto)
  expect_equal(config$default_extend, 12)
  expect_true(config$trading_days)
  expect_true(config$easter)
})

test_that("MAVE default config - quarterly", {
  config <- get_mave_config(4)
  expect_true(config$arima_auto)
  expect_equal(config$default_extend, 4)
  expect_false(config$trading_days)
})

test_that("MAVE default config - annual", {
  config <- get_mave_config(1)
  expect_false(config$arima_auto)
  expect_equal(config$arima_order, c(0, 2, 2))
  expect_equal(config$default_extend, 3)
  expect_false(config$trading_days)
  expect_false(config$easter)
})
