# =============================================================================
# Test Data Generators and Helpers
# =============================================================================
#
# Auto-loaded by testthat before running tests.
# Provides test data generators and fixture path helpers.
#
# =============================================================================


# --- Test time series generators ---

create_monthly_ts <- function(n = 120, start_year = 2010) {
  t <- 1:n
  trend <- 100 + 0.5 * t
  seasonal <- 10 * sin(2 * pi * t / 12)
  noise <- rnorm(n, 0, 3)
  values <- trend + seasonal + noise
  ts(values, start = c(start_year, 1), frequency = 12)
}

create_quarterly_ts <- function(n = 60, start_year = 2005) {
  t <- 1:n
  trend <- 200 + t
  seasonal <- 20 * sin(2 * pi * t / 4)
  noise <- rnorm(n, 0, 5)
  values <- trend + seasonal + noise
  ts(values, start = c(start_year, 1), frequency = 4)
}

create_annual_ts <- function(n = 30, start_year = 1990) {
  t <- 1:n
  trend <- 500 + 10 * t
  noise <- rnorm(n, 0, 20)
  values <- trend + noise
  ts(values, start = start_year, frequency = 1)
}


# --- Spec fixture path helper ---

spec_fixture_path <- function(filename) {
  testthat::test_path("fixtures", "spec_files", filename)
}
