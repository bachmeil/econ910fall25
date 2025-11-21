## This example shows the estimation of impulse response functions in a linear
## model using local projections

library(tstools)
inf <- import.fred("inflation.csv")
u <- import.fred("unrate.csv")

## We'll use timing restrictions to estimate the impact effect
fit.inf <- tsreg(inf, u %~% lags(inf %~% u, 1))
fit.inf
## So an economic activity shock causes inflation to fall by 0.08 on impact
## The unemployment rate is assumed to rise by 1 on impact

## 1-step IRF
## First, apply the definition of the IRF
## Forecast with the shock - Forecast without the shock

`h=1 LP fit` <- tsreg(inf, lags(inf %~% u, 1))
`h=1 LP fit`
`inf(T+1|shock)` <- 0.15442 + 0.99713*(-0.08) - 0.02443*1
`inf(T+1|shock)`
`inf(T+1|no shock)` <- 0.15442 + 0.99713*0 - 0.02443*0
`inf(T+1|no shock)`
`inf irf h=1` <- `inf(T+1|shock)` - `inf(T+1|no shock)`
`inf irf h=1`
## But for a linear model, there's no reason to calculate both forecasts
## because the only difference in forecast comes from the slope coefficients
## This is the same thing as above, but in one calculation
0.99713*(-0.08) - 0.02443*1

`h=2 LP fit` <- tsreg(inf, lags(inf %~% u, 2))
`h=2 LP fit`
`inf irf h=2` <- 0.99049*(-0.08) - 0.04587*1
`inf irf h=2`

`h=3 LP fit` <- tsreg(inf, lags(inf %~% u, 3))
`h=3 LP fit`
`inf irf h=3` <- 0.98047*(-0.08) - 0.02443*1
`inf irf h=3`
