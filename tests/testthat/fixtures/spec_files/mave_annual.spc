# FORECASTING and BACKCASTING default spec file for ANNUAL series with the (0,2,2) model.
series{
        title="FORECASTING and BACKCASTING default spec file for ANNUAL series"
        format="datevalue"
        period=1
        decimals=2
        }     
transform{function=auto}
arima{model=(0,2,2)}
check{print=all}
forecast{maxlead=@@@forecast_maxlead=3@@@
         maxback=@@@forecast_maxback=3@@@
         save=(fct bct)}  
outlier{types=(ao ls)}
estimate{
         maxiter=5000
         }