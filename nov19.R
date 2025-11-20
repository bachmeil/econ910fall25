theta <- function(gamma, dbar, y1) {
  den <- 1 + exp(-gamma * (y1-dbar))
  return(1/den)
}
theta(1, 0, 0.4)
theta(2, 0, 0.4)
theta(10, 0, 0.4)
theta(1000, 0, 0.4)
theta(1, 0, -0.4)
theta(2, 0, -0.4)
theta(10, 0, -0.4)
theta(1000, 0, -0.4)

library(tstools)
u <- import.fred("unrate.csv")
theta <- u > 6
theta

fit.linear <- tsreg(u, lags(u, 1))
fit.linear
fit.thr <- tsreg(u, theta %~% lags(u %~% theta*u, 1))
fit.thr
# Forecast u(T+1) given u(T) = 4
3.276 + 0.06*4
# Forecast u(T+1) given u(T) = 8
3.276 + 0.06*8 + 0.05*8
fit.thr2 <- tsreg(u, theta %~% lags(u %~% theta*u, 2))
fit.thr2

