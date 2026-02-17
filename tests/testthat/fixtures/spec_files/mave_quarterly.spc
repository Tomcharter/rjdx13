# FORECASTING and BACKCASTING default spec file for QUARTERLY series using AUTOMDL.
series{
        title="FORECASTING and BACKCASTING default spec file for QUARTERLY series"
        format="datevalue"
        period=4
        decimals=2
        }     
transform{function=auto}
regression{aictest=(easter)
        }     
check{print=all}
automdl{maxdiff=(2 1)
maxorder=(2 2)
        }     
outlier{types=(ao ls)}
forecast{maxlead=@@@forecast_maxlead=4@@@
         maxback=@@@forecast_maxback=4@@@
         save=(fct bct)}  
estimate{
         maxiter=5000
         }