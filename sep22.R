library(tstools)
F <- matrix(1:4, nrow=2)
F
F %*% F %*% F %*% F
matPower(F, 4)
F^4

# Initial state vector
# Represents the values after the shock
# Make it a matrix to be safe
init <- matrix(c(1, 0, 0))
init
F <- matrix(c(0.63, 1, 0, 0.28, 0, 1, -0.14, 0, 0),
            nrow=3)
F
Z <- F %*% init
Z
Z <- F %*% Z
Z
Z <- F %*% Z
Z
# Rough approximation
0.63 + 0.28 - 0.14
0.77^2
matPower(F, 3) %*% init

Z <- matrix(c(1, 0, 0))
hl <- 0
repeat {
  if (Z[1] < 0.5) { print(hl); break }
  hl <- hl + 1
  Z <- F %*% Z
}

# Compute IRF half-life confidence interval
ysim <- arima.sim(model=list(ar=0.9), n=100)
ysim
fit <- tsreg(ysim, lags(ysim, 1))
fit
set.seed(200)
simbetas <- replicate(1000, {
  ysim <- arima.sim(model=list(ar=0.9), n=100)
  fit <- tsreg(ysim, lags(ysim, 1))
  coef(fit)[2]  
})
mean(simbetas)
# We want the empirical distribution
quantile(simbetas, probs=c(0.025, 0.975))
# Now do the half-life of the IRF
set.seed(200)
simhalflife <- replicate(1000, {
  ysim <- arima.sim(model=list(ar=0.9), n=100)
  fit <- tsreg(ysim, lags(ysim, 1))
  log(0.5)/log(coef(fit)[2])
})
quantile(simhalflife, probs=c(0.025, 0.975))


# Bias of AR(1) models
ysim <- arima.sim(model=list(ar=0.2), n=100)
fit <- tsreg(ysim, lags(ysim,1))
fit$coef[2]
# Conclude nothing about bias
set.seed(200)
simoutput <- replicate(1000, {
  ysim <- arima.sim(model=list(ar=0.2), n=100)
  fit <- tsreg(ysim, lags(ysim,1))
  fit$coef[2]
})
mean(simoutput)
median(simoutput)
plot(density(simoutput))

set.seed(200)
simoutput <- replicate(1000, {
  ysim <- arima.sim(model=list(ar=0.9), n=100)
  fit <- tsreg(ysim, lags(ysim,1))
  fit$coef[2]
})
mean(simoutput)
median(simoutput)

set.seed(200)
simoutput <- replicate(1000, {
  ysim <- arima.sim(model=list(ar=0.98), n=40)
  fit <- tsreg(ysim, lags(ysim,1))
  fit$coef[2]
})
mean(simoutput)
median(simoutput)

set.seed(200)
simoutput <- replicate(1000, {
  ysim <- arima.sim(model=list(ar=0.98), n=4000)
  fit <- tsreg(ysim, lags(ysim,1))
  fit$coef[2]
})
mean(simoutput)
median(simoutput)
