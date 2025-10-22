library(tstools)
inf <- import.fred("inflation.csv")
u <- import.fred("unrate.csv")
fit.inf <- tsreg(inf, lags(u %~% inf,1))
fit.inf
fit.u <- tsreg(u, lags(u %~% inf,1))
fit.u
0.09-0.01*4.3+0.99*2.94
0.14+0.97*4.3+0.01*2.94
0.09-0.01*4.34+0.99*2.96
