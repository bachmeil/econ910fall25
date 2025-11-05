library(tstools)
inf <- import.fred("inflation.csv")
dip <- import.fred("dip.csv")
ffr <- import.fred("ffr.csv")

# Estimate the RF VAR equations
fit.dip <- tsreg(dip, 
                 lags(dip %~% inf %~% ffr, 1:12))
fit.inf <- tsreg(inf,  
                 lags(dip %~% inf %~% ffr, 1:12))
fit.ffr <- tsreg(ffr,  
                 lags(dip %~% inf %~% ffr, 1:12))
# Get the residuals
e.dip <- fit.dip$resids
e.inf <- fit.inf$resids
e.ffr <- fit.ffr$resids
# Form the covariance matrix
residuals.matrix <- e.ffr %~% e.dip %~% e.inf
v <- cov(residuals.matrix)
v
# Take the choleski
t(chol(v))

# Change the ordering
residuals.matrix2 <- e.ffr %~% e.inf %~% e.dip
v2 <- cov(residuals.matrix2)
v2
# Take the choleski
t(chol(v2))

library(vars)
ds <- dip %~% inf %~% ffr
varfit <- VAR(ds, ic="SC")
fevd(varfit, n.ahead=12)

u <- import.fred("unrate.csv")
eq1 <- tsreg(u, lags(u %~% inf, 1))
eq2 <- tsreg(inf, lags(u %~% inf, 1))
e1 <- eq1$resids
e2 <- eq2$resids
cov(e1 %~% e2)
