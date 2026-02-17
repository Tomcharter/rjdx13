# FORECASTING default spec file for QUARTERLY series using AUTOMDL.
# 2 things may change - the file and the maxlead
series{
        title="BACKCASTING default spec file for QUARTERLY series"
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
forecast{
		maxback=4
        save=bct}  
estimate{
         maxiter=5000
         }