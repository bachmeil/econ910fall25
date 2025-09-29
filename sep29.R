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
set.seed(200)
simoutput <- replicate(10000, {
  y <- rnorm(100)
  x <- rnorm(100)
  d1 <- x
  d1[1:50] <- 0
  fit1 <- lm(y~x+d1)
  # Coefficients and variances, etc.
  coefmat1 <- summary(fit1)$coefficients
 
  y <- rnorm(100)
  x <- rnorm(100)
  d2 <- x
  d2[1:25] <- 0
  fit2 <- lm(y~x+d2)
  # Coefficients and variances, etc.
  coefmat2 <- summary(fit2)$coefficients
  
  y <- rnorm(100)
  x <- rnorm(100)
  d3 <- x
  d3[1:75] <- 0
  fit3 <- lm(y~x+d3)
  # Coefficients and variances, etc.
  coefmat3 <- summary(fit3)$coefficients
  
  (abs(coefmat1["d1", "t value"]) > 1.96) | 
    (abs(coefmat2["d2", "t value"]) > 1.96) |
    (abs(coefmat3["d3", "t value"]) > 1.96)
})
mean(simoutput)

# Let's get the size under control by raising the 
# critical value
set.seed(200)
simoutput <- replicate(10000, {
  y <- rnorm(100)
  x <- rnorm(100)
  d1 <- x
  d1[1:50] <- 0
  fit1 <- lm(y~x+d1)
  # Coefficients and variances, etc.
  coefmat1 <- summary(fit1)$coefficients
  
  y <- rnorm(100)
  x <- rnorm(100)
  d2 <- x
  d2[1:25] <- 0
  fit2 <- lm(y~x+d2)
  # Coefficients and variances, etc.
  coefmat2 <- summary(fit2)$coefficients
  
  y <- rnorm(100)
  x <- rnorm(100)
  d3 <- x
  d3[1:75] <- 0
  fit3 <- lm(y~x+d3)
  # Coefficients and variances, etc.
  coefmat3 <- summary(fit3)$coefficients
  
  (abs(coefmat1["d1", "t value"]) > 2.4) | 
    (abs(coefmat2["d2", "t value"]) > 2.4) |
    (abs(coefmat3["d3", "t value"]) > 2.4)
})
mean(simoutput)
