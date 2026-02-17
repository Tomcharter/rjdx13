# SPLINING Default FORECASTING & BACKCASTING spec file for ANNUAL series with the (0,2,2) model.
# 2 things may change: the file and the maxlead value

series{
        title="BACKCASTING default spec file for ANNUAL series"
        format="datevalue"
        period=1
        decimals=2
        }

transform{function=auto}

arima{model=(0,2,2)}

check{ print=all}

forecast{
         maxback=3
         save=(bct)
}

outlier{types=(ao ls)}

estimate{
         maxiter=5000
         }
