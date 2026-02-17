# FORECASTING and BACKCASTING default spec file for MONTHLY series using AUTOMDL.
series{
        title="FORECASTING and BACKCASTING default spec file for MONTHLY series"
        format="datevalue"
        period=12
        decimals=2
        }     
transform{function=auto}
regression{aictest=(easter, td)
        }     
check{print=all}
automdl{
        maxdiff=(2 1)
        maxorder=(2 2)
        }     
outlier{types=(ao ls)}
forecast{maxlead=@@@forecast_maxlead=12@@@
         maxback=@@@forecast_maxback=12@@@
         save=(fct bct)}  
estimate{
         maxiter=5000
         }