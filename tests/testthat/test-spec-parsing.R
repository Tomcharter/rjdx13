# Test Category 3: Custom Spec File Parsing
# Tests that the spec file parser correctly extracts parameters.
# No RJDemetra required - these test pure R parsing functions.

test_that("Parse seasadj_quarterly.spc", {
  config <- parse_x13_spec(spec_fixture_path("seasadj_quarterly.spc"))
  expect_false(is.null(config))
  expect_true(config$arima_auto)
})

test_that("Parse seasadj_monthly.spc", {
  config <- parse_x13_spec(spec_fixture_path("seasadj_monthly.spc"))
  expect_false(is.null(config))
  expect_true(config$trading_days)
})

test_that("Parse forecast-quarterly.spc", {
  config <- parse_x13_spec(spec_fixture_path("forecast-quarterly.spc"))
  expect_false(is.null(config))
})

test_that("Parse forecast-monthly.spc", {
  config <- parse_x13_spec(spec_fixture_path("forecast-monthly.spc"))
  expect_false(is.null(config))
})

test_that("Parse backcast-monthly.spc", {
  config <- parse_x13_spec(spec_fixture_path("backcast-monthly.spc"))
  expect_false(is.null(config))
  expect_false(is.null(config$forecast_maxback))
})

test_that("Parser extracts transform setting", {
  config <- parse_x13_spec(spec_fixture_path("seasadj_monthly.spc"))
  expect_equal(config$transform, "auto")
})

test_that("Parser extracts outlier types", {
  config <- parse_x13_spec(spec_fixture_path("seasadj_monthly.spc"))
  expect_true(config$outlier_ao)
  expect_true(config$outlier_ls)
})

test_that("Parse mave_monthly.spc", {
  config <- parse_x13_spec(spec_fixture_path("mave_monthly.spc"))
  expect_false(is.null(config))
})

test_that("Parse mave_quarterly.spc", {
  config <- parse_x13_spec(spec_fixture_path("mave_quarterly.spc"))
  expect_false(is.null(config))
})

# Test custom spec files if directory exists
test_that("Parse custom spec files", {
  custom_spec_dir <- file.path(testthat::test_path(), "..", "..", "..", "test_files",
                               "Seas_adj_custom_spec files")
  skip_if_not(dir.exists(custom_spec_dir), "Custom spec directory not found")

  spec_files <- list.files(custom_spec_dir, pattern = "\\.spc$", full.names = TRUE)
  n_specs <- length(spec_files)
  skip_if(n_specs == 0, "No custom spec files found")

  success_count <- 0
  for (sf in spec_files) {
    tryCatch({
      config <- parse_x13_spec(sf)
      if (!is.null(config)) success_count <- success_count + 1
    }, error = function(e) { })
  }
  expect_equal(success_count, n_specs)
})
