# Spec Translator Reference: X13 .spc to RJDemetra Mapping

Implementation reference for `spec_translator.R` — how each X13 spec file setting
is translated to RJDemetra parameters, what cannot be translated, and what
compensating measures are in place.

**Related docs:**
- `x13_replication.md` — detailed technical analysis with empirical results
- `three_systems_comparison.md` — comparison of X13 CLI, seasonal, and RJDemetra

---

## Table of Contents

1. [Quick Reference: What Matches, What Doesn't](#1-quick-reference-what-matches-what-doesnt)
2. [Known Gaps & Limitations](#2-known-gaps--limitations)
3. [Parameter Mapping](#3-parameter-mapping)
4. [Custom Spec File Parameters](#4-custom-spec-file-parameters)
5. [Default Spec Equivalence](#5-default-spec-equivalence)
6. [Implementation Details](#6-implementation-details)

---

## 1. Quick Reference: What Matches, What Doesn't

| Aspect | Status |
|---|---|
| ARIMA fixed model specification | Exact match |
| Transform (log/none) | Exact match |
| Transform (auto) | Approximate — positivity check vs AIC test |
| Outlier types and critical value | Exact match |
| User-defined outliers | Exact match |
| Calendar effect specification | Exact match |
| Calendar pre-testing methodology | Approximate — t-test vs AIC |
| Forecast/backcast horizon | Exact match |
| X11 decomposition mode/filters | Exact match |
| Denton benchmarking | Approximate — post-processing vs integrated |
| automdl maxorder/maxdiff | **Cannot match** — warnings emitted |
| automdl cancel/ub1/ub2/balanced | **Cannot improve** — empirically tested |
| estimate maxiter | **Cannot map** — low impact |
| ARIMA model selection algorithm | **Different** — fundamental algorithmic difference |
| Outlier detection algorithm | **Different** — numerical implementation |
| X11 numerical precision | **Different** — Fortran vs Java |

For the broader context on expected numerical differences, see
`x13_replication.md`.

---

## 2. Known Gaps & Limitations

This section consolidates all limitations in one place for quick reference when
debugging unexpected results.

### 2.1 Cannot Be Bridged

These are fundamental differences between X13 and RJDemetra that cannot be
resolved through parameter tuning or workarounds.

| Gap | Detail |
|---|---|
| **ARIMA model selection algorithm** | X13 uses Hannan-Rissanen initialization and its own BIC calculation. JDemetra+ uses different initialization and convergence criteria. The same series may get different ARIMA models. |
| **Outlier detection algorithm** | Different numerical implementations mean X13 and RJDemetra may detect different outliers for the same series. |
| **X11 numerical precision** | X13 is Fortran-based, JDemetra+ is Java-based. Floating-point differences accumulate through the X11 filter chain. |
| **automdl maxorder** | JDemetra+ hardcodes max regular order at 3 and max seasonal order at 1. X13 defaults to (2,2). This means JDemetra+ may select higher regular-order models and will never select seasonal AR(2) or MA(2). See [Section 6.1](#61-automdl-constraints). |
| **automdl internal parameters** | `cancel`, `ub1`, `ub2`, `balanced` differ between X13 and RJDemetra defaults. Overriding these does not improve replication rates due to the deeper algorithmic differences above. |

### 2.2 Approximate Mappings

These parameters are translated but the methodology differs, so borderline
cases may produce different results.

| Gap | Detail | Impact |
|---|---|---|
| **Transform auto-detection** | X13 uses AIC-based log/level test. We use a positivity check (all values > 0 = Log, otherwise None). | Low — only affects rare cases where X13's AIC test rejects Log despite all-positive values. |
| **Calendar pre-testing** | X13 uses AIC comparison. RJDemetra uses significance tests (t-test for TD, test statistic for Easter). | Medium — borderline calendar effects may be included/excluded differently. |
| **Denton benchmarking** | X13 applies Denton internally during X11. We apply it as a post-processing step using `tempdisagg::td()`. | Low — small differences at year boundaries. |

### 2.3 Parsed With Warnings (Not Applied)

These parameters are recognised by the parser and stored in the config, but
cannot be applied. A warning is emitted during spec creation.

| Parameter | Why Not Applied | Workaround |
|---|---|---|
| `transform{constant=N}` | RJDemetra does not support adding a constant before log transform. | Pre-add the constant to the ts object before calling `rjd_seasadj()`, then adjust the output. |
| Permanent prior adjustment factors (`transform{type=permanent file=...}`) | Requires loading and interpreting external `.ppp` files. | Load the `.ppp` file manually, apply multiplicative/additive factors to the ts object before processing. |
| User-defined seasonal regressors (`regression{user=(...) file=...}`) | Requires loading external `.rmx` files and passing via `usrdef.var`. | Load the `.rmx` file manually, pass to RJDemetra via `usrdef.varEnabled = TRUE, usrdef.var = matrix, usrdef.varType = "Seasonal"`. |

### 2.4 Unmappable Parameters

| Parameter | Reason |
|---|---|
| `estimate{maxiter=N}` | No equivalent in RJDemetra. JDemetra+ uses its own convergence criteria. Low impact. |
| `automdl{maxorder=(...)}` | Hardcoded in JDemetra+. Parsed and stored; warning emitted when values differ. |
| `automdl{maxdiff=(...)}` | Hardcoded in JDemetra+. Default values happen to match. |

---

## 3. Parameter Mapping

### 3.1 Successfully Mapped Parameters

| X13 Spec Setting | RJDemetra Parameter | Scope | Notes |
|---|---|---|---|
| `transform{function=auto/log/none}` | `transform.function` | All | Resolved via `resolve_transform()` before spec creation |
| `automdl{}` / `pickmdl{}` | `automdl.enabled = TRUE` | All | See [Section 6.1](#61-automdl-constraints) for caveats |
| `arima{model=(p,d,q)(P,D,Q)}` | `arima.p/d/q`, `arima.bp/bd/bq` | All | Direct mapping, all standard ARIMA models supported |
| `outlier{types=(ao ls tc)}` | `outlier.ao/ls/tc` | All | Individual boolean flags |
| `outlier{critical=N}` | `outlier.cv = N` | All | |
| `regression{aictest=(easter)}` | `easter.enabled=TRUE, easter.test=TRUE` | All | See [Section 6.2](#62-calendar-effect-pre-testing) |
| `regression{aictest=(td)}` | `tradingdays.option="TradingDays", tradingdays.test="Separate_T"` | All | See [Section 6.2](#62-calendar-effect-pre-testing) |
| `regression{variables=(Easter[N])}` | `easter.enabled=TRUE, easter.test=FALSE` | All | Forced inclusion (no pre-test) |
| `regression{variables=(TD)}` | `tradingdays.option="TradingDays", tradingdays.test="None"` | All | Forced inclusion (no pre-test) |
| `regression{variables=(td1coef)}` | `tradingdays.option="WorkingDays"` | All | Single-coefficient working day regressor |
| `regression{variables=(Const)}` | `arima.mu = TRUE` | All | Mean/intercept term for stationary models |
| `easter[N]` | `easter.duration = N` | All | |
| `forecast{maxlead=N}` | `fcst.horizon = N` | All | |
| `forecast{maxback=N}` | `fcst.horizon` on reversed series | Backcast | Via time-reversal approach |
| `regression{variables=(AO2009.1 ...)}` | `usrdef.outliersType/Date` | All | Supports both numeric (`AO2009.1`) and month name (`AO2011.Jan`) date formats |
| `x11{mode=mult/add}` | `x11.mode` | SA only | |
| `x11{seasonalma=msr/s3x5/...}` | `x11.seasonalma` | SA only | Also accepts `sma` abbreviation |
| `x11{trendma=N}` | `x11.trendma = "HN"` | SA only | Also accepts `tma` abbreviation |
| `x11{type=summary}` | Pass-through (no decomposition) | SA only | Series returned unchanged except for optional Denton |
| `series{span=(Y.P, Y.P)}` | Applied via `window()` before model | All | Pre-processing step |
| `force{type=denton rho=1 lambda=1 mode=ratio}` | Post-processing Denton benchmarking | SA only | See [Section 6.5](#65-denton-benchmarking) |

**Scope key:** "All" = `create_rjdemetra_spec()` (SA) and `create_regarima_spec()` (forecast/backcast/mave).
"SA only" = `create_rjdemetra_spec()` only. "Backcast" = backcast time-reversal only.

### 3.2 Parsed But Not Passable to RJDemetra

These are extracted from spec files into the config for transparency. Warnings
are emitted when they differ from JDemetra+'s hardcoded values.

| X13 Setting | Config Field | JDemetra+ Hardcoded | Impact |
|---|---|---|---|
| `automdl{maxorder=(2 2)}` | `automdl_maxorder` | (3, 1) | **Medium** — see [Section 6.1](#61-automdl-constraints) |
| `automdl{maxdiff=(2 1)}` | `automdl_maxdiff` | (2, 1) | **Low** — default values match. Only matters for custom specs with non-standard values. |
| `estimate{maxiter=5000}` | Not stored | N/A | **Low** — JDemetra+ uses its own convergence criteria. |

### 3.3 Not Mapped (No RJDemetra Equivalent)

| X13 Setting | Reason |
|---|---|
| `forecast{save=fct/bct}` | X13 file output directive. RJDemetra returns R objects. |
| `x11{appendfcst=yes}` | RJDemetra handles forecast extension internally. |
| `series{comptype=add/mult}` | Composite series type. Not applicable to single-series processing. |
| `check{print=all}` | X13 diagnostic output directive. |

### 3.4 Output-Only Parameters (No Action Needed)

These X13 parameters control file output or metadata and have no effect on the
statistical computation. They are silently ignored.

| Parameter | Description |
|---|---|
| `series{save=(a1 b1)}` | Save original/prior-adjusted series to files |
| `forecast{save=fct}` | Save forecasts to file |
| `force{save=saa}` | Save benchmarked SA to file |
| `x11{save=(d10 d11 d12 d13)}` | Save X11 output tables to files |
| `force{round=no}` | Don't round benchmarked values |
| `force{usefcst=no}` | Don't use forecasts for benchmarking |
| `series{name="..."}` | Series name metadata |
| `series{start=Y.P}` | Series start date (overridden by ts object) |
| `estimate{parms=estimated}` | Parameter estimation mode (always estimated) |

### 3.5 Parsed But Unused

| X13 Setting | Config Field | Reason |
|---|---|---|
| `series{period=N}` | `periodicity` | Overridden by `frequency()` of the ts object |
| `series{title="..."}` | `title` | Stored but not used in processing |

---

## 4. Custom Spec File Parameters

Custom spec files can be passed to `rjd_seasadj()`, `rjd_forecast()`, and
`rjd_backcast()`. MAVE always uses built-in defaults and does not accept custom specs.

### 4.1 Which Parameters Apply to Which Function

When a custom spec file is parsed, all parameters are extracted into the config.
However, not all parameters are relevant to every function.

| Parameter Category | SA | Forecast | Backcast |
|---|---|---|---|
| **Transform** (function, constant) | Used | Used | Used |
| **ARIMA** (model, automdl, mean/intercept) | Used | Used | Used |
| **Outliers** (types, critical value, user-defined) | Used | Used | Used* |
| **Calendar** (Easter, TD, pre-testing, td1coef) | Used | Used | Used* |
| **Horizon** (maxlead) | Used | Used | — |
| **Horizon** (maxback) | — | — | Used |
| **Span** (series start/end filter) | Used | Used | Used |
| **X11** (mode, seasonalma, trendma, type) | Used | — | — |
| **Denton** (force type/rho/lambda/mode) | Used | — | — |
| **Prior adjustment** (external file) | Warning | Warning | Warning |
| **User regressors** (external file) | Warning | Warning | Warning |

*Backcast uses time-reversal: calendar effects and user-defined outliers are
disabled on the reversed series because calendar dates are meaningless in
reversed time. See [Section 6.6](#66-backcast-time-reversal).

### 4.2 Supported ARIMA Models

All standard ARIMA(p,d,q)(P,D,Q) specifications are supported, including:

| Category | Example Models |
|---|---|
| **Airline model** | (0,1,1)(0,1,1) |
| **Non-seasonal** | (2,1,0), (1,1,0), (1,1,2), (0,1,1) |
| **Stationary (d=0)** | (2,0,0)(0,1,1), (1,0,1)(1,0,1), (1,0,0)(1,0,0) |
| **Low-order seasonal** | (0,1,0)(0,1,1), (0,1,1)(1,0,0), (0,0,0)(1,1,0) |
| **Higher-order** | (2,1,0)(0,1,1), (0,1,2)(0,1,1), (1,1,2)(0,0,1) |
| **Automatic selection** | `automdl{}` or `pickmdl{}` |

### 4.3 Unsupported X13 Blocks

The following X13 spec blocks are not supported. If encountered, they are
silently ignored:

`pickmdl{}`, `slidingspans{}`, `history{}`, `spectrum{}`, `seats{}`,
`x11regression{}`, `identify{}`

### 4.4 Custom Spec Coverage Summary

| Category | Parameters | Status |
|---|---|---|
| **Fully handled** | transform (log/none/auto), arima (all models), outlier types/cv, seasonalma/trendma (incl. sma/tma aliases), x11 type/mode, forecast maxlead/maxback, force/denton, span, user outliers (incl. month name dates), calendar (aictest + variables distinction), td1coef/WorkingDays, Const/arima.mu | Implemented |
| **Parsed with warnings** | transform constant, prior adjustment factors, user-defined seasonal regressors | Warning emitted, not applied |
| **Output-only** | save directives, check, name, start, parms, round, usefcst, appendfcst | No action needed |
| **Cannot map** | estimate maxiter, automdl maxorder/maxdiff | Documented, warning emitted |

---

## 5. Default Spec Equivalence

Each function has a hardcoded default config (`get_*_config()`) that matches the
corresponding `.spc` reference file. The spec files in `inst/spec_files/` are
reference copies only — they are not read at runtime unless explicitly passed as
custom specs.

### 5.1 Forecast Defaults

| Setting | Annual | Quarterly | Monthly |
|---|---|---|---|
| **ARIMA** | (0,2,2) fixed | automdl | automdl |
| **Transform** | auto | auto | auto |
| **Calendar** | None | Easter (pre-tested) | Easter + TD (pre-tested) |
| **Outliers** | AO + LS | AO + LS | AO + LS |
| **Horizon** | maxlead=3 | maxlead=4 | maxlead=12 |
| **automdl limits** | N/A (fixed ARIMA) | maxorder=(2,2) — partial* | maxorder=(2,2) — partial* |

### 5.2 Backcast Defaults

| Setting | Annual | Quarterly | Monthly |
|---|---|---|---|
| **ARIMA** | (0,2,2) fixed | automdl | automdl |
| **Transform** | auto | auto | auto |
| **Calendar** | None | Easter (disabled on reversed series) | Easter + TD (disabled on reversed series) |
| **Outliers** | AO + LS | AO + LS | AO + LS |
| **Horizon** | maxback=3 | maxback=4 | maxback=12 |

### 5.3 Seasonal Adjustment Defaults

| Setting | Quarterly | Monthly |
|---|---|---|
| **ARIMA** | automdl (pickmdl) | automdl (pickmdl) |
| **Transform** | auto | auto |
| **Calendar** | Easter (pre-tested) | Easter + TD (pre-tested) |
| **Outliers** | AO + LS | AO + LS |
| **X11 mode** | Multiplicative | Multiplicative |
| **X11 seasonal filter** | MSR | MSR |
| **Denton** | Enabled (rho=1, lambda=1, ratio) | Enabled (rho=1, lambda=1, ratio) |
| **Horizon** | maxlead=4 | maxlead=12 |

### 5.4 MAVE Defaults

MAVE extends both ends of the series for moving average boundary handling. It
uses both `maxlead` and `maxback`.

| Setting | Annual | Quarterly | Monthly |
|---|---|---|---|
| **ARIMA** | (0,2,2) fixed | automdl | automdl |
| **Transform** | auto | auto | auto |
| **Calendar** | None | Easter (pre-tested) | Easter + TD (pre-tested) |
| **Outliers** | AO + LS | AO + LS | AO + LS |
| **Extend** | 3/3 | 4/4 | 12/12 |

*"Partial" for automdl: JDemetra+ hardcodes max regular order at 3 and max
seasonal order at 1, vs X13's typical (2,2). See [Section 6.1](#61-automdl-constraints).

---

## 6. Implementation Details

### 6.1 Automdl Constraints

#### The Problem

X13 spec files commonly contain:
```
automdl{maxdiff=(2 1)
        maxorder=(2 2)}
```

RJDemetra does not expose `automdl.maxorder` or `automdl.maxdiff` in either
`x13_spec()` or `regarima_spec_x13()`. These are hardcoded in JDemetra+.

#### JDemetra+ Hardcoded Limits vs X13

| Constraint | X13 Spec Value | JDemetra+ Hardcoded | Consequence |
|---|---|---|---|
| Max regular AR/MA order | 2 | 3 | JDemetra+ may select ARIMA(3,d,q) or (p,d,3) models that X13 would never consider |
| Max seasonal AR/MA order | 2 | 1 | JDemetra+ will **never** select seasonal AR(2) or MA(2) — a restriction vs X13 |
| Max regular differencing | 2 | 2 | No difference |
| Max seasonal differencing | 1 | 1 | No difference |

#### Internal Parameter Differences

Beyond `maxorder`/`maxdiff`, RJDemetra's automatic model selection has different
internal defaults from X13:

| Parameter | X13 Default | RJDemetra Default | Our Code |
|---|---|---|---|
| `cancel` | 0.05 | 0.10 | Not set (uses RJDemetra default) |
| `ub1` | 0.97 | 1.04 | Not set |
| `ub2` | 0.91 | 0.88 | Not set |
| `balanced` | TRUE | FALSE | Not set |
| `acceptdefault` | FALSE | FALSE | Match |
| `mixed` | TRUE | TRUE | Match |

We do not override these because empirical testing showed that explicitly
matching X13's values did not improve the replication rate. The differences
stem from deeper algorithmic divergences that cannot be bridged through
parameter tuning.

#### Implementation

- `parse_x13_spec()` extracts values into `config$automdl_maxorder` and `config$automdl_maxdiff`
- Both `create_rjdemetra_spec()` and `create_regarima_spec()` emit `warning()` when parsed values differ from JDemetra+'s limits
- Warnings are captured by `run_with_warnings()` and included in result messages

#### Affected Functions

| Function | When Affected |
|---|---|
| `rjd_seasadj` | Custom specs with `automdl{}` |
| `rjd_forecast` | Default quarterly/monthly specs + custom specs |
| `rjd_backcast` | Default quarterly/monthly specs + custom specs |
| `rjd_mave` | Default quarterly/monthly specs |
| Annual series | **Not affected** — use fixed ARIMA(0,2,2), not automdl |

### 6.2 Calendar Effect Pre-Testing

#### X13 vs RJDemetra Approach

| | X13 | Our RJDemetra Mapping |
|---|---|---|
| Easter pre-test | `aictest=(easter)` — AIC comparison | `easter.test = TRUE` — significance test |
| TD pre-test | `aictest=(td)` — AIC comparison | `tradingdays.test = "Separate_T"` — individual t-tests |

The pre-testing methodology differs: X13 uses information criterion (AIC)
comparison, RJDemetra uses statistical significance tests. Borderline cases may
be decided differently.

#### Pre-testing vs Forced Inclusion

Custom specs distinguish between conditional and forced calendar effects:

| Spec Syntax | Meaning | RJDemetra Mapping |
|---|---|---|
| `aictest=(easter)` | Include Easter if pre-test passes | `easter.test = TRUE` |
| `variables=(Easter[N])` | Always include Easter | `easter.test = FALSE` |
| `aictest=(td)` | Include TD if pre-test passes | `tradingdays.test = "Separate_T"` |
| `variables=(TD)` | Always include TD (6 coefficients) | `tradingdays.test = "None"` |
| `variables=(td1coef)` | Always include working day (1 coefficient) | `tradingdays.option = "WorkingDays", tradingdays.test = "None"` |

#### Alternative `tradingdays.test` Values

| Value | Behaviour |
|---|---|
| `"Separate_T"` | Individual t-tests per regressor (used for `aictest`) |
| `"Joint_F"` | Joint F-test on all TD regressors |
| `"None"` | No testing — always include if enabled (used for `variables`) |

### 6.3 Transform Auto-Detection

**X13:** `transform{function=auto}` uses an AIC-based log/level test internally.

**Our approach:** `resolve_transform()` uses a positivity check — if all values > 0,
use Log; otherwise None. This is resolved **before** passing to RJDemetra.

**Why:** Resolving early ensures the same transform value is used in both the spec
and the result metadata. Passing `"Auto"` to RJDemetra would work, but then
`config$transform` stays "auto" while the result should report "Log" or "None".

**Limitation:** In rare cases where X13's AIC test would choose None even though
all values are positive, our code chooses Log instead.

### 6.4 Forecast Horizon (`fcst.horizon`)

#### SA (`create_rjdemetra_spec`)

Passes `config$forecast_maxlead %||% -1` to `x13_spec()`. The `-1` convention
means "1 year" (4 for quarterly, 12 for monthly), which matches the default
spec files.

The forecast horizon affects X11 decomposition — forecasts are appended
to the series before seasonal filtering, affecting estimates at the end of the
series. Custom specs with non-standard maxlead values will correctly change
this behaviour.

#### Forecast/Backcast/MAVE (`create_regarima_spec`)

Passes `config$forecast_horizon %||% -1` to `regarima_spec_x13()`. The calling
functions (`rjd_forecast_impl`, `rjd_backcast_impl`, `rjd_mave_impl`) resolve the
horizon from either the user's argument, the spec file, or the default config
into `config$forecast_horizon` before calling `create_regarima_spec()`.

#### Backcast Horizon Mapping Flow

```
X13 spec: forecast{maxback=N}
  -> parse_x13_spec(): config$forecast_maxback = N
  -> rjd_backcast_impl(): config$backcast_horizon = N
  -> rjd_backcast_impl(): config$forecast_horizon = N   (for reversed series)
  -> create_regarima_spec(): fcst.horizon = N
  -> RJDemetra::regarima() on reversed series
  -> rev() results back to correct time order
```

### 6.5 Denton Benchmarking (SA Only)

X13's `force{type=denton rho=1 lambda=1 mode=ratio}` forces annual totals of SA
series to match original. Our implementation applies Denton as a **post-processing
step** after X11 decomposition:

1. **Primary:** `tempdisagg::td()` with `method="denton-cholette"` and `conversion="sum"`
2. **Fallback** (if tempdisagg not installed): proportional distribution by year

**Difference from X13:** X13 applies Denton internally during X11. Our approach applies it
after. Small differences in benchmarked values may occur, particularly at year boundaries.

### 6.6 Backcast Time-Reversal

**Why we reverse the series:** RJDemetra's RegARIMA model only supports forward
forecasting — there is no built-in backcast or `maxback` equivalent. To produce
backcasts, we reverse the series in time (so the earliest observation becomes the
last), forecast forward on that reversed series, then reverse the forecasted
values back to get values that extend the series into the past.

**X13 behaviour:** `forecast{maxback=N}` generates backcasts from the same model
that was fit forward. Calendar effects are part of the forward model.

**Our approach:** Time-reversal — reverse the series, fit a new model, forecast
forward on the reversed series, reverse results. Calendar effects and
user-defined outliers are **disabled** for the reversed series
(`disable_calendar = TRUE`).

**Why calendar is disabled on reversed series:**
- Easter occurs at a specific calendar date — reversed time destroys this relationship
- Trading day regressors are calendar-based and meaningless in reverse time
- User-defined outlier dates refer to specific calendar events and are equally meaningless
- ARIMA dynamics are symmetric under time reversal and still capture the series' stochastic properties

**Impact:** For series with strong calendar patterns (e.g., monthly retail with
heavy Easter effects), backcasts may differ from X13's maxback because the calendar
signal is not captured in the reversed model. For series with weak/no calendar effects
(annual, most quarterly), the difference is negligible.
