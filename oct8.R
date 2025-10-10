library(tstools)
dgdp <- import.fred("dgdp.csv")
fit <- tsreg(dgdp, lags(dgdp, 1:2))
fit

yhat <- fit$fitted
yhat

# Draws from the pick distribution
# Rademacher distribution
z <- sample(c(-1,1), length(yhat),
            replace=TRUE)
z
mean(z)

# Bootstrapped samples of y
ysim <- yhat + z
plot(ysim)
plot(dgdp)
# Fixed-regressor design wild bootstrap
tsreg(ysim, lags(dgdp, 1:2))

set.seed(400)
simoutput <- replicate(1000, {
  z <- sample(c(-1,1), length(yhat),
              replace=TRUE)
  ysim <- yhat + z
  fit <- tsreg(ysim, lags(dgdp, 1:2))
  fit$coefficients[2] - fit$coefficients[3]
})
plot(density(simoutput))
1.96*sd(simoutput)

inf <- import.fred("inflation.csv")
u <- import.fred("unrate.csv")
plot(inf)
plot(u)
plot(u %~% inf, plot.type="single")

fit.inf <- tsreg(inf, lags(u %~% inf,1))
fit.inf
fit.u <- tsreg(u, lags(u %~% inf,1))
fit.u
last(u)
last(inf)
# inf(T+1) = 0.09 - 0.01*u(T) + 0.99*inf(T)
0.09 - 0.01*4.3 + 0.99*2.94
# inf forecast = 2.96%
