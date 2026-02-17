# Technical Analysis: X13 vs RJDemetra Replication

This document explains why the U.S. Census Bureau's X-13ARIMA-SEATS program
cannot be exactly replicated using the RJDemetra R package, despite both
implementing X-11 seasonal adjustment methodology. The differences stem from
distinct algorithmic implementations, default parameter values, and underlying
statistical engines.

For how we handle these differences in practice, see `spec_translator_reference.md`.
For a comparison of all three systems (X13 CLI, seasonal, RJDemetra), see
`three_systems_comparison.md`.

---

## Table of Contents

1. [Background](#1-background)
2. [ARIMA Model Selection](#2-arima-model-selection)
3. [Unit Root Testing](#3-unit-root-testing)
4. [Outlier Detection](#4-outlier-detection)
5. [Other Algorithmic Differences](#5-other-algorithmic-differences)
6. [Empirical Evidence](#6-empirical-evidence)
7. [Practical Implications](#7-practical-implications)

---

## 1. Background

### X-13ARIMA-SEATS (X13)

- Developed and maintained by the U.S. Census Bureau
- Combines X-11 seasonal adjustment with ARIMA time series modelling
- Uses the AUTOMDL procedure for automatic ARIMA model selection
- Fortran-based implementation
- Industry standard for official statistics in North America

### RJDemetra / JDemetra+

- R interface to JDemetra+, developed by the National Bank of Belgium and Eurostat
- Based on TRAMO-SEATS methodology with X-11 decomposition option
- Uses a distinct automatic model selection algorithm
- Java-based implementation
- European standard for official statistics

Both systems implement the same conceptual methodology (RegARIMA + X-11), but
they are independent implementations with different codebases, numerical
routines, and default behaviours.

---

## 2. ARIMA Model Selection

This is the single largest source of divergence between the two systems. When
automatic model selection is enabled (`automdl` or `pickmdl`), X13 and RJDemetra
frequently choose different ARIMA models for the same series.

### 2.1 Default Parameter Differences

| Parameter | X13 Default | RJDemetra Default | Effect |
|---|---|---|---|
| `cancel` | 0.05 | 0.10 | RJDemetra is less aggressive at cancelling near-equal AR/MA roots |
| `ub1` | 0.97 | 1.04 | Different seasonal unit root boundary |
| `ub2` | 0.91 | 0.88 | Different regular unit root boundary |
| `balanced` | TRUE | FALSE | X13 favours balanced models by default; RJDemetra does not |
| `acceptdefault` | FALSE | FALSE | Match |
| `mixed` | TRUE | TRUE | Match |

### 2.2 Why Parameter Matching Doesn't Help

We tested explicitly matching X13's automdl parameters in RJDemetra:

```r
spec <- RJDemetra::x13_spec("RSA5c",
    automdl.balanced = TRUE,
    automdl.cancel = 0.05)
```

This produced no improvement in replication rate. The reason is that the
parameter values sit on top of fundamentally different internals:

- **Hannan-Rissanen initialization** — the starting estimates for ARIMA
  parameter estimation differ between X13 and JDemetra+
- **BIC calculation** — slight differences in the information criterion
  computation lead to different model rankings
- **Convergence criteria** — the optimisation routines use different stopping
  rules, so even the same model specification can produce different parameter
  estimates

### 2.3 Hardcoded Model Order Limits

RJDemetra does not expose `automdl.maxorder` or `automdl.maxdiff`. These are
hardcoded in JDemetra+:

| Constraint | X13 Typical | JDemetra+ Hardcoded | Consequence |
|---|---|---|---|
| Max regular AR/MA order | 2 | 3 | JDemetra+ may select higher-order regular models |
| Max seasonal AR/MA order | 2 | 1 | JDemetra+ will never select seasonal AR(2) or MA(2) |
| Max regular differencing | 2 | 2 | No difference |
| Max seasonal differencing | 1 | 1 | No difference |

---

## 3. Unit Root Testing

X13 uses specific thresholds for determining differencing orders:
- Seasonal unit root test with `ub1 = 0.97`
- Regular unit root test with `ub2 = 0.91`

JDemetra+ uses TRAMO-based unit root testing with different critical values and
test statistics. Although RJDemetra exposes `ub1` and `ub2` parameters, they do
not map to the same underlying test implementation as X13. Setting identical
values does not produce identical differencing decisions.

This matters because the differencing order (d and D in ARIMA) determines the
model class before AR/MA orders are selected. A different differencing decision
cascades into a different final model.

---

## 4. Outlier Detection

| Aspect | X13 | RJDemetra |
|---|---|---|
| Detection method | Iterative reweighted least squares | Similar iterative approach, different convergence |
| Critical values | Formula based on series length | Different formula, same inputs |
| Outlier types | AO, LS, TC, SO | AO, LS, TC, SO |
| Interaction with ARIMA | Sequential (model then outliers) | Can be joint or sequential |

The practical effect is that for the same series, X13 and RJDemetra may detect
different outliers at different dates. Since detected outliers feed back into
the ARIMA model estimation and X11 decomposition, this creates a chain of
differences in the final SA output.

When a fixed ARIMA model is specified (no automatic selection), outlier detection
is the primary remaining source of divergence.

---

## 5. Other Algorithmic Differences

### X-11 Filters

Both systems implement Henderson and seasonal moving average filters, but the
implementations differ in edge effect handling (how filters are applied near the
start and end of the series), extreme value replacement thresholds, and iteration
convergence criteria. These are low-level numerical differences that accumulate
through the multi-pass X-11 filter chain.

### Calendar Effects

| Feature | X13 | RJDemetra |
|---|---|---|
| Trading day regressors | U.S. calendar-based | Configurable, different defaults |
| Easter pre-testing | AIC comparison | Significance test (t-statistic) |
| TD pre-testing | AIC comparison | Individual t-tests (`Separate_T`) |
| Leap year | Separate regressor option | Different parameterisation |

The pre-testing methodology is the most impactful difference here. X13 decides
whether to include calendar effects using AIC (information criterion), while
RJDemetra uses statistical significance tests. Borderline cases — where the
calendar effect is marginally significant — may be decided differently.

### Numerical Precision

X13 is Fortran-based, JDemetra+ is Java-based. Different floating-point
arithmetic, matrix inversion routines, and optimisation algorithms mean that
even identical inputs can produce slightly different outputs. These differences
are individually small but compound through the processing pipeline.

---

## 6. Empirical Evidence

We tested 122 seasonal adjustment cases comparing X13 command-line output to
RJDemetra output using the same input series:

| Configuration | Pass Rate |
|---|---|
| RJDemetra with standard defaults (RSA5c) | 17.2% |
| Best of 4 alternative configurations per series | 28.7% |

The 4 configurations tested were: RSA5c (standard automdl), RSA5c with Denton
benchmarking, RSA1 with S3X9 seasonal filter, and fixed airline model
(0,1,1)(0,1,1). No single configuration consistently outperformed the others —
different series matched X13 under different settings.

Explicitly matching X13's automdl parameters (`balanced=TRUE`, `cancel=0.05`)
produced no improvement over RJDemetra's defaults, confirming that the
differences are algorithmic rather than configurational.

These results established that pursuing exact X13 replication through RJDemetra
parameter tuning is not a viable path. The current implementation instead
focuses on faithful translation of spec file settings (see
`spec_translator_reference.md`) and accepts methodological equivalence rather
than numerical equivalence.

---

## 7. Practical Implications

### What Is Achievable

- **Similar results** for well-behaved series with clear seasonal patterns
- **Faithful spec translation** — all key X13 parameters are mapped to their
  closest RJDemetra equivalents (see `spec_translator_reference.md`)
- **Consistent methodology** when using RJDemetra exclusively across all series

### What Is Not Achievable

- **Bit-for-bit identical results** to X13
- **Exact ARIMA model matching** when using automatic model selection
- **Identical outlier detection** in all cases

### Our Approach

We translate X13 spec files to RJDemetra specifications as faithfully as
possible, document the known gaps and their impact, and emit warnings where
parameters cannot be exactly mapped. The goal is methodological equivalence —
applying the same statistical concepts with a different implementation — rather
than numerical replication.

---

## References

1. U.S. Census Bureau. (2017). *X-13ARIMA-SEATS Reference Manual*. Version 1.1.
2. Eurostat. (2017). *JDemetra+ Reference Manual*. Version 2.x.
3. Gomez, V., & Maravall, A. (1996). *Programs TRAMO and SEATS*. Bank of Spain Working Paper.
4. Findley, D.F., et al. (1998). *New Capabilities and Methods of the X-12-ARIMA Seasonal-Adjustment Program*. Journal of Business & Economic Statistics.
