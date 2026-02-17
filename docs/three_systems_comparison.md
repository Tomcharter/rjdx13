# Comparison: X13 Command Line vs seasonal Package vs RJDemetra

This document compares three approaches to X-13ARIMA-SEATS seasonal adjustment.

| System | X13 Replication | R Integration | Core Engine |
|---|---|---|---|
| X13 Command Line | Exact (reference) | None | Fortran (Census Bureau) |
| seasonal Package | Exact | Excellent | Fortran (same binary as CLI) |
| RJDemetra | Approximate | Good | Java (JDemetra+) |

**Related docs:**
- `x13_replication.md` — detailed analysis of why RJDemetra cannot exactly replicate X13
- `spec_translator_reference.md` — how we translate X13 spec files to RJDemetra parameters

---

## Table of Contents

1. [Architecture](#1-architecture)
2. [Installation and Dependencies](#2-installation-and-dependencies)
3. [Input Specification](#3-input-specification)
4. [Output Access](#4-output-access)
5. [Numerical Accuracy](#5-numerical-accuracy)
6. [Feature Comparison](#6-feature-comparison)
7. [Performance](#7-performance)
8. [When to Use Which](#8-when-to-use-which)

---

## 1. Architecture

### X13 Command Line (Census Bureau)

```
User -> .spc file -> x13as.exe (Fortran) -> Output files (.d11, .d12, etc.)
```

The original X-13ARIMA-SEATS program from the U.S. Census Bureau. Standalone
Fortran binary, command-line interface with specification files (.spc), output
as multiple files in Census Bureau format.

### seasonal R Package

```
User -> R code -> seasonal::seas() -> .spc file -> x13as.exe -> Output files -> R objects
```

R wrapper that calls the **exact same X13 binary** internally. It translates R
arguments to .spc format, calls the X13 executable via `system()`, and parses
the output files back into R objects. Results are identical to the command line
because it is the same engine.

### RJDemetra

```
User -> R code -> RJDemetra -> JDemetra+ (Java) -> R objects
```

R interface to JDemetra+, a **complete re-implementation** of X13 algorithms in
Java by the National Bank of Belgium and Eurostat. Does not use Census Bureau
code. Results differ from X13 due to algorithmic differences (see
`x13_replication.md`).

---

## 2. Installation and Dependencies

### X13 Command Line

- **Download:** https://www.census.gov/data/software/x13as.html
- **Dependencies:** None (standalone executable)
- **Pros:** No runtime dependencies, works on any system
- **Cons:** Manual download, must manage file I/O yourself, no R integration

### seasonal Package

```r
install.packages("seasonal")
# x13binary package auto-installed as dependency
library(seasonal)
checkX13()  # Verify binary is working
```

- **Dependencies:** R >= 2.15, x13binary package (provides pre-compiled X13 binaries)
- **Pros:** One-line install, binaries auto-managed, cross-platform
- **Cons:** Corporate firewalls may block binary download

### RJDemetra

```r
install.packages("RJDemetra")
```

- **Dependencies:** R >= 3.5, Java 8+ (JRE or JDK), rJava package
- **Pros:** No external binary management
- **Cons:** Java dependency can be problematic, rJava installation issues are common, higher memory usage

---

## 3. Input Specification

The same seasonal adjustment expressed in each system:

### X13 Command Line (.spc file)

```
series{
    file = "data.csv"
    period = 4
    span = (1997.1,)
}
transform{function=log}
arima{model=(0,1,1)(0,1,1)}
outlier{types=(ao ls)}
x11{mode=mult seasonalma=s3x5}
force{type=denton rho=1 lambda=1 mode=ratio}
```

### seasonal Package (R code)

```r
seas(
  data,
  transform.function = "log",
  arima.model = "(0 1 1)(0 1 1)",
  outlier.types = c("ao", "ls"),
  x11.mode = "mult",
  x11.seasonalma = "s3x5",
  force.type = "denton",
  force.rho = 1,
  force.lambda = 1
)
```

Uses `spec.argument` syntax that maps directly to X13 spec file parameters.

### RJDemetra (R code)

```r
spec <- x13_spec("RSA5c",
  transform.function = "Log",
  automdl.enabled = FALSE,
  arima.p = 0, arima.d = 1, arima.q = 1,
  arima.bp = 0, arima.bd = 1, arima.bq = 1,
  outlier.ao = TRUE,
  outlier.ls = TRUE,
  x11.mode = "Multiplicative",
  x11.seasonalma = "S3X5"
)
x13(data, spec)
```

Uses pre-defined base specifications (RSA0-RSA5c) that are then modified. ARIMA
orders are specified as individual parameters rather than a model string. Some
parameters use different names or capitalisation from X13.

---

## 4. Output Access

### X13 Command Line

Creates multiple output files: `.d11` (SA series), `.d12` (trend), `.d10`
(seasonal factors), `.d13` (irregular), `.out` (diagnostics), `.log`, `.err`,
and many more specialised outputs. Comprehensive but requires file parsing.

### seasonal Package

```r
m <- seas(data)
final(m)              # SA series (equivalent to .d11)
trend(m)              # Trend-cycle
series(m, "d11")      # Direct access to any X13 output table
udg(m, "f3.m01")      # Any diagnostic statistic
summary(m)            # Full diagnostics
spc(m)                # View what was sent to X13
```

Full access to every X13 output table and diagnostic through R objects.

### RJDemetra

```r
result <- x13(data, spec)
result$final$series[, "sa"]   # SA series
result$final$series[, "t"]    # Trend
result$final$series[, "s"]    # Seasonal
result$final$series[, "i"]    # Irregular
result$regarima                # Model info
result$diagnostics             # Diagnostics (subset of X13)
```

Returns core components and diagnostics but not the full set of X13 output
tables.

---

## 5. Numerical Accuracy

| Comparison | Expected Difference |
|---|---|
| X13 CLI vs seasonal | **Zero** — same binary, same computation |
| X13 CLI vs RJDemetra | Variable — depends on series characteristics |
| seasonal vs RJDemetra | Same as above |

The seasonal package produces identical results to X13 CLI because it calls the
same Fortran executable. Any difference would only come from floating-point
display precision when parsing output files.

RJDemetra produces different results because JDemetra+ is an independent
re-implementation with different algorithm internals, default parameters, and
numerical routines. For a detailed analysis of these differences, see
`x13_replication.md`.

---

## 6. Feature Comparison

| Feature | X13 CLI | seasonal | RJDemetra |
|---|---|---|---|
| X-11 Decomposition | Yes | Yes | Yes |
| SEATS Decomposition | Yes | Yes | Yes |
| TRAMO-SEATS | No | No | Yes |
| Automatic Model Selection | Yes | Yes | Yes |
| Outlier Detection | Yes | Yes | Yes |
| Trading Day Adjustment | Yes | Yes | Yes |
| Easter Adjustment | Yes | Yes | Yes |
| Denton Benchmarking | Yes | Yes | Partial |
| User-Defined Regressors | Yes | Yes | Yes |
| Sliding Spans | Yes | Yes | No |
| History Analysis | Yes | Yes | No |
| Spectrum Diagnostics | Yes | Yes | Yes |
| All X13 Output Tables | Yes | Yes | Subset |
| Interactive GUI | No | seasonalview | JDemetra+ |
| ESS Validation | No | No | Yes |

**Key differences:**
- **seasonal** exposes the full X13 feature set since it runs the actual binary
- **RJDemetra** adds TRAMO-SEATS and ESS validation but lacks some X13-specific
  features (sliding spans, history analysis, full output table access)
- **Denton benchmarking** in RJDemetra is partial — it must be applied as a
  post-processing step rather than being integrated into the X11 pipeline

---

## 7. Performance

| Aspect | X13 CLI | seasonal | RJDemetra |
|---|---|---|---|
| **Single series** | Fast | Slightly slower (file I/O overhead per call) | Fast (after JVM warmup) |
| **Batch processing** | Fast (parallelisable via shell) | File I/O overhead scales linearly | JVM optimises repeated calls |
| **Memory** | Low (per-process) | Low-medium (R + subprocess) | Higher (JVM heap allocation) |
| **Startup** | Instant | Instant | Slow first call (JVM initialisation) |

For batch processing of many series, RJDemetra's JVM overhead is amortised and
it performs well. The seasonal package's file I/O overhead (writing .spc,
reading output files) becomes noticeable at scale but is rarely a bottleneck in
practice.

---

## 8. When to Use Which

### X13 Command Line

- Non-R production pipelines
- Regulatory requirements specifying "Census Bureau X13"
- Debugging specification issues (direct access to all files)
- Integration with other programming languages

### seasonal Package

- **Exact X13 replication is required**
- Reproducing or validating results from X13 command line
- Need access to all X13 output tables and diagnostics
- Migrating existing .spc workflows to R

### RJDemetra

- European Statistical System (ESS) compliance required
- Need TRAMO-SEATS methodology
- Want to avoid external binary dependencies
- Exact X13 match is not required
- Prefer pure R/Java solution
