library(tstools)
dnorm(0)
dnorm(1)
dnorm(1)/dnorm(0)
curve(dnorm, -4, 4, ylab="", xlab="")

# Monte Carlo Integration
# 100 observations
# Statistic: Variance of half of standard
# normal random variable
set.seed(200)
output <- replicate(1000, {
  y <- rnorm(100)
  var(y/2)
})
mean(output)
output
plot(density(output))
# Any statistic can be calculated this way
# As long as you can take draws from the joint
# distribution of all variables

set.seed(200)
y <- rnorm(100000)
var(y)
# Now do the rejection
# The above is the proposal density
index.wrong <- which(abs(y) > 1)
index.right <- which(abs(y) <= 1)
# Randomly choose 25% of index.wrong
# to keep
length(index.right)
length(index.wrong)
k <- length(index.wrong)
k
keep <- sample(c(TRUE, FALSE), size=k,
               prob=c(0.25, 0.75),
               replace=TRUE)
mean(keep)
# Construct the final set of elements
index <- c(index.right, index.wrong[keep])
length(index)
y.final <- y[index]
var(y.final)
var(y)

library(bayesm)
dgdp <- import.fred("dgdp.csv")
vars <- cbind(1, lags(dgdp, 0:2))
Y <- vars[,2]
X <- vars[,-2]
draws.posterior <- runireg(
  Data = list(y=Y, X=X),
  Mcmc = list(R=100000, nprint=10000)))

names(draws.posterior)
b.sim <- draws.posterior$betadraw
dim(b.sim)
for (col in 1:3) {
  cat("mean: ", mean(b.sim[,col]), " sd: ",
      sd(b.sim[,col], "\n"))
}
mean(b.sim[,2])
sd(b.sim[,2])
summary(tsreg(dgdp, lags(dgdp,1:2)))

A.prior <- diag(3)
A.prior[1,1] <- 0.0000001
A.prior[2,2] <- 1000
A.prior[3,3] <- 1000
A.prior
prior.informative <- list(betabar=c(0,0,0),
                          A = A.prior)
draws.informative <- runireg(
  Data=list(y=Y, X=X), Prior=prior.informative,
  Mcmc=list(R=100000, nprint=10000)
)
b.sim <- draws.informative$betadraw
mean(b.sim[,2])
sd(b.sim[,2])
mean(b.sim[,3])

library(bsvars)
inf <- import.fred("inflation.csv")
u <- import.fred("unrate.csv")
spec <- specify_bsvar$new(data = inf %~% u, p=2)
draws.var <- estimate(spec, 100000)
draws.var$posterior
slopes <- draws.var$posterior$A
slopes[,,1]
slopes[,,2]
slopes[,,100000]
slopes[,,100001]
