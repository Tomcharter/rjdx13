devtools::load_all("C:/Users/tomch/Coding Projects/R_Projects/RJdemetra/release")

ts_data <- ts(100 + 0.5*(1:120) + 10*sin(2*pi*(1:120)/12) + rnorm(120,0,3),
              start=c(2010,1), frequency=12)

config <- get_default_config(12)
cat("Config transform:", config$transform, "\n")
cat("Config arima_auto:", config$arima_auto, "\n")

transform_applied <- resolve_transform(config$transform, ts_data)
cat("Transform applied:", transform_applied, "\n")

spec <- tryCatch(
  create_rjdemetra_spec(config, ts_data, transform_fun = transform_applied),
  error = function(e) {
    cat("SPEC ERROR:", e$message, "\n")
    cat("SPEC TRACEBACK:\n")
    traceback()
    NULL
  }
)
cat("Spec result:", class(spec), "\n")
