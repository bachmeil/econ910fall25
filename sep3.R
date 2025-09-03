# Take one draw of each
# x and y have 1000 observations each
# normal is a standard distribution
# Functions available for taking draws
x <- rnorm(1000)
y <- rnorm(1000)
plot(density(x))
var(x + y)

# But that's just one sample
# We need many samples and then take the average
# of var(x + y)
simoutput <- replicate(100000, {
  x <- rnorm(1000)
  y <- rnorm(1000)
  var(x + y)
})
plot(density(simoutput))
mean(simoutput)

# Reproducible
# Set seed
rnorm(3)
rnorm(3)
set.seed(100)
rnorm(3)
set.seed(100)
rnorm(3)
get.seed()
.Random.seed
set.seed(200)

# var(cos(x)^2 + cos(y)^3)
simoutput <- replicate(100000, {
  x <- rnorm(1000)
  y <- rnorm(1000)
  var(cos(x)^2 + cos(y)^3)
})
plot(density(simoutput))
mean(simoutput)
dnorm(3.0)
pnorm(3.0)
rt(100, 2)

v <- c(1.5, 2.5, 3.5, 4.5, 5.5)
v
length <- 0
for (val in v) {
  length <- length+1
}
length

v.cumsum <- numeric(length(v))
index <- 0
for (val in v) {
  index <- index+1
  if (index == 1) {
    v.cumsum[index] <- val
  } else {
    v.cumsum[index] <- val+v.cumsum[index-1]
  }
}

# Calculate cumulative sum recursively
library(tstools)
vector.cumsum <- function(v, result=0) {
  cat("v: ", v, "\n")
  cat("result: ", result, "\n")
  if (length(v) > 0) {
    next.value <- last(result) + v[1]
    cat("next.value: ", next.value, "\n")
    return(vector.cumsum(v[-1],
                         c(result, next.value)))
  } else {
    return(result)
  }
}
vector.cumsum(v)[-1]