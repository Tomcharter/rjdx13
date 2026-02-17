# FORECASTING default spec file for QUARTERLY series using AUTOMDL.
# 2 things may change - the file and the maxlead
series{
        title="FORECASTING default spec file for QUARTERLY series"
        file=@@@data file@@@
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
         save=fct}  
estimate{
         maxiter=5000
         }