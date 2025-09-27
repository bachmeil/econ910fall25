set.seed(200)
simoutput <- replicate(10000, {
  y <- rnorm(100)
  x <- rnorm(100)
  d <- x
  d[1:50] <- 0
  fit <- lm(y~x+d)
  # Coefficients and variances, etc.
  coefmat <- summary(fit)$coefficients
  # Get the t-stat on the structural break dummy
  coefmat["d", "t value"]
})
plot(density(simoutput))
plot(density(abs(simoutput)))
mean(abs(simoutput) > 1.96)

# Question: Multiple hypothesis tests
# How is the size affected?