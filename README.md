# rjdx13

R package for seasonal adjustment, forecasting, backcasting, and moving-average
extension (MAVE) using X13-ARIMA-SEATS methodology via the RJDemetra package.

Translates X13 `.spc` spec files into RJDemetra specifications, providing a
bridge between existing X13 workflows and R-based processing.

## Functions

| Function | Description |
|---|---|
| `rjd_seasadj()` | Seasonal adjustment (X11 decomposition) |
| `rjd_forecast()` | Forward forecasting (RegARIMA) |
| `rjd_backcast()` | Backcasting via time-reversal (RegARIMA) |
| `rjd_mave()` | Moving-average extension (forecast + backcast) |

Each function accepts a single `ts` object or a named list for batch processing.
Matrix wrapper variants (`*_matrix()`) accept a matrix of series (rows) by dates
(columns) with optional per-series custom spec files.

## Installation

Requires R >= 3.5 and Java 8+ (for RJDemetra).

```r
# Install dependencies
install.packages(c("RJDemetra", "tempdisagg"))

# Install from local source
install.packages("path/to/rjdx13", repos = NULL, type = "source")
```

## Quick Start

```r
library(rjdx13)

# Seasonal adjustment with defaults
result <- rjd_seasadj(my_quarterly_ts)
result$sa        # Seasonally adjusted values
result$trend     # Trend component
result$arima     # Fitted ARIMA model info

# Forecast with defaults
result <- rjd_forecast(my_quarterly_ts, horizon = 8)
result$forecast  # Forecast values

# With a custom X13 spec file
result <- rjd_seasadj(my_ts, spec_file = "path/to/custom.spc")
```

## Custom Spec Files

X13 `.spc` files are parsed and translated to RJDemetra specifications. Supported
parameters include ARIMA models, transform, outlier detection, calendar effects,
X11 filters, Denton benchmarking, and more. See `docs/spec_translator_reference.md`
for the full mapping.

## Dependencies

- **RJDemetra** (required) -- R interface to JDemetra+
- **tempdisagg** (optional) -- Denton-Cholette benchmarking, with fallback if missing

## Documentation

- `docs/spec_translator_reference.md` -- X13 to RJDemetra parameter mapping
- `docs/x13_replication.md` -- Why X13 and RJDemetra produce different results
- `docs/three_systems_comparison.md` -- X13 CLI vs seasonal vs RJDemetra

## License

Proprietary. For internal use only.
