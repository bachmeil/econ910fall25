# Generate data
# True beta = 2.5
# H0: beta = 1.0
set.seed(445)
x <- rnorm(10000)
e <- rnorm(10000)
y <- 2.5*x + e
fit <- lm(y ~ x)
summary(fit)
plot(x, y)
summary(fit)$sigma

# Estimate is 2.51
# Is this "plausible" if beta = 1?
# How much does e cause betahat to 
# fluctuate?
set.seed(400)
bootcoef <- replicate(1000, {
  esim <- rnorm(10000, sd=15.986)
  ysim <- -0.1875 + 1.0*x + esim
  # For this new sample, get betahat
  lm(ysim ~ x)$coefficients[2]
})
sd(bootcoef)
1.96*sd(bootcoef)
quantile(bootcoef, probs=c(0.025, 0.975))

library(tstools)
calculate.next <- function(current.calculation,
                           next.element) {
  nextval <- last(current.calculation) +
    next.element
  return(c(current.calculation, nextval))
}
output <- calculate.next(1.5, 2.5)
output
output <- calculate.next(output, 3.5)
output
output <- calculate.next(output, 4.5)
output

input <- c(2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5)
output <- Reduce(calculate.next, input, init=1.5)
output


# Generate AR(2) data
# y(t) = 0.5 + 0.5 y(t-1) + 0.2 y(t-2) + e
calculate.next <- function(current.calculation,
                           next.element) {
  lastvals <- last(current.calculation, 2)
  nextval <- 0.5 + sum(lastvals * c(0.2, 0.5)) +
    next.element
  return(c(current.calculation, nextval))
}
set.seed(400)
errors <- rnorm(12)
Reduce(calculate.next, errors, init=c(0.0, 0.0))
errors
