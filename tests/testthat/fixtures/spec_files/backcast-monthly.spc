# FORECASTING default spec file for MONTHLY series using AUTOMDL
# 2 things may change - the file and the maxlead
series{
        title="BACKCASTING default spec file for MONTHLY series"
        format="datevalue"
        period=12
        decimals=2
        }     
transform{function=auto}
regression{aictest=(easter, td)
        }     
check{print=all}
automdl{maxdiff=(2 1)
maxorder=(2 2)
        }     
outlier{types=(ao ls)}
forecast{
		maxback=12
        save=bct
		}  
estimate{
         maxiter=5000
         }