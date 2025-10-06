# Parametric bootstrap
library(tstools)
dgdp <- import.fred("dgdp.csv")
fit <- tsreg(dgdp, lags(dgdp, 1:2))
fit
names(fit)
length(fit$residuals)

calculate.next <- function(current.calculation,
                           next.element) {
  lastvals <- last(current.calculation, 2)
  nextval <- 0.81 + sum(lastvals * c(-0.29, 1.03)) +
    next.element
  return(c(current.calculation, nextval))
}
set.seed(400)
errors <- rnorm(308, sd=1.52)
ysim <- ts(Reduce(calculate.next, errors,
               init=c(0, 0)))
ysim
tsreg(ysim, lags(ysim, 1:2))

set.seed(400)
h0 <- replicate(1000, {
  errors <- rnorm(308, sd=1.52)
  ysim <- ts(Reduce(calculate.next, errors,
                    init=c(0, 0)))
  fit <- tsreg(ysim, lags(ysim, 1:2))
  fit$coefficients[2] - fit$coefficients[3]
})
plot(density(h0))
mean(h0 < 0)
sd(h0)
0.1074709*1.96
mean(h0 < 0.211)

# IID bootstrap
fit <- tsreg(dgdp, lags(dgdp, 1:2))
set.seed(400)
h0 <- replicate(1000, {
  errors <- sample(residuals(fit), 308, 
                   replace=TRUE)
  ysim <- ts(Reduce(calculate.next, errors,
                    init=c(0,0)))
  fit <- tsreg(ysim, lags(ysim,1:2))
  fit$coefficients[2] - fit$coefficients[3]
})
plot(density(h0))
1.96*sd(h0)

as.matrix(lags(dgdp, 0:2))

# Pairs bootstrap
yx <- as.matrix(lags(dgdp, 0:2))
# Sample observation numbers
index <- sample(1:308, 308, replace=TRUE)
index
yxsim <- ts(yx[index,])

set.seed(400)
h0 <- replicate(1000, {
  yx <- as.matrix(lags(dgdp, 0:2))
  index <- sample(1:308, 308, replace=TRUE)
  yxsim <- ts(yx[index,])
  fit <- tsreg(yxsim[,1], yxsim[,2:3])
  fit$coefficients[2] - fit$coefficients[3]
})
plot(density(h0))
sd(h0)*1.96
