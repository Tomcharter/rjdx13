# =============================================================================
# Error Code Constants and Stub Functions
# =============================================================================
#
# Removable at integration time: when the real record_ts_error() and
# method_error() are available in the host application, delete this file
# and let those functions take over.
#
# =============================================================================


# --- Error code constants ----------------------------------------------------

FORECAST_FREQ_MISMATCH <- "CAS02140"
FORECAST_NO_HORIZON    <- "CAS08040"
FORECAST_RJD_FAIL      <- "CAS_RJD_FAIL"

BACKCAST_FREQ_MISMATCH <- "CAS02140"
BACKCAST_NO_HORIZON    <- "CAS08040"
BACKCAST_RJD_FAIL      <- "CAS_RJD_FAIL"

SEASADJ_FREQ_MISMATCH <- "CAS02140"
SEASADJ_RJD_FAIL       <- "CAS_RJD_FAIL"

MAVE_FREQ_MISMATCH <- "CAS02140"
MAVE_NO_HORIZON    <- "CAS08040"
MAVE_RJD_FAIL      <- "CAS_RJD_FAIL"


# --- Stub functions ----------------------------------------------------------
# Overridable via options, or remove this file and replace with real
# implementations at integration time.

record_ts_error <- function(error_code, ts_names) {
  fn <- getOption("rjdx13.record_ts_error")
  if (is.function(fn)) fn(error_code, ts_names)
  invisible(NULL)
}

method_error <- function(error_code) {
  fn <- getOption("rjdx13.method_error")
  if (is.function(fn)) return(fn(error_code))
  sprintf("Method error [%s]", error_code)
}
