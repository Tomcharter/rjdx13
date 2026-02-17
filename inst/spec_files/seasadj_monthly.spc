# Monthly seasonal adjustment spec file
# 2 things may change - the file and the maxlead
series{
        title="Monthly Seasonal Adjustment spec file with default settings"
        format="datevalue"
        period=12
        decimals=2
        }     
transform{function=auto}
pickmdl{
method=first
mode=both
                 }     
regression{aictest=(easter, td)}
estimate{
         maxiter=5000
         }
outlier{types=(ao ls)}
forecast{maxlead=12
  save=(fct)}  
force{type=denton
    rho=1
    lambda=1
   round=no
   usefcst=no
   mode=ratio
   save=saa}
x11{appendfcst=yes
save=(d11 d10 d12 d13)}