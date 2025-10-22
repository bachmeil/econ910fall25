library(tstools)
inf <- import.fred("inflation.csv")
u <- import.fred("unrate.csv")
library(vars)

data <- inf %~% u
data
VARselect(data, lag.max=13)
varfit <- VAR(data, p=13)
varfit
varfit <- VAR(data, lag.max=13, ic="SC")
varfit
predict(varfit, h=12)
predictions(varfit, "inf", n=12)
getVarForecast(varfit, "inf", n=1)
getVarForecast(varfit, "inf", n=6)
getVarForecast(varfit, "inf", n=12)
getVarForecasts(varfit, "inf", n=1:12)
predict(varfit, h=12)$u

varfit
F <- formF(varfit)
dim(formF(varfit))
start <- matrix(c(4.2, 3.0, rep(0.0, 25)), ncol=1)
start
m <- matPower(F, 12)
m %*% start
