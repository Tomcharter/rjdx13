# FORECASTING default spec file for ANNUAL series using the (0,2,2) model.
# 2 things may change - the file and the maxlead
series{
        title="FORECASTING default spec file for ANNUAL series"
        file=@@@data file@@@
        format="datevalue"
        period=1
        decimals=2
        }     
transform{function=auto}
arima{model=(0,2,2)}
check{print=all}
forecast{maxlead=@@@forecast_maxlead=3@@@
         save=fct}  
outlier{types=(ao ls)}
estimate{
         maxiter=5000
         }