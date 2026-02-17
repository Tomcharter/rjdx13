# Test Category 7: Diagnostics Extraction
# Tests that diagnostics are correctly extracted and formatted.

skip_if_not_installed("RJDemetra")

test_that("Seasadj extracts diagnostics object", {
  monthly_ts <- create_monthly_ts(n = 120)
  result <- rjd_seasadj(monthly_ts)
  expect_false(is.null(result$diagnostics))
  expect_type(result$diagnostics, "list")
})

test_that("Seasadj diagnostics include M-statistics", {
  monthly_ts <- create_monthly_ts(n = 120)
  result <- rjd_seasadj(monthly_ts)
  diag <- result$diagnostics
  expect_true("m_statistics" %in% names(diag) || "m_overall" %in% names(diag))
})

test_that("Seasadj messages include diagnostics summary", {
  monthly_ts <- create_monthly_ts(n = 120)
  result <- rjd_seasadj(monthly_ts)
  if (!is.null(result$diagnostics$m_overall) || !is.null(result$diagnostics$aic)) {
    expect_match(result$messages, "DIAGNOSTICS")
  }
})

test_that("Forecast extracts diagnostics object", {
  monthly_ts <- create_monthly_ts(n = 120)
  result <- rjd_forecast(monthly_ts)
  expect_false(is.null(result$diagnostics))
  expect_type(result$diagnostics, "list")
})

test_that("Backcast extracts diagnostics object", {
  monthly_ts <- create_monthly_ts(n = 120)
  result <- rjd_backcast(monthly_ts)
  expect_false(is.null(result$diagnostics))
  expect_type(result$diagnostics, "list")
})

test_that("format_diagnostics produces string output", {
  monthly_ts <- create_monthly_ts(n = 120)
  result <- rjd_seasadj(monthly_ts)
  formatted <- format_diagnostics(result$diagnostics)
  expect_type(formatted, "character")
})

test_that("format_diagnostics handles NULL input", {
  formatted <- format_diagnostics(NULL)
  expect_type(formatted, "character")
  expect_equal(formatted, "")
})

test_that("format_diagnostics handles empty list", {
  formatted <- format_diagnostics(list())
  expect_type(formatted, "character")
  expect_equal(formatted, "")
})

test_that("Diagnostics AIC is numeric when present", {
  monthly_ts <- create_monthly_ts(n = 120)
  result <- rjd_seasadj(monthly_ts)
  if (!is.null(result$diagnostics$aic)) {
    expect_type(result$diagnostics$aic, "double")
  }
})

test_that("MAVE extracts diagnostics object", {
  monthly_ts <- create_monthly_ts(n = 120)
  result <- rjd_mave(monthly_ts, extend = 6)
  expect_false(is.null(result$diagnostics))
  expect_type(result$diagnostics, "list")
})
