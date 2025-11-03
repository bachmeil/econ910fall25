library(tstools)
inf <- import.fred("inflation.csv")
dip <- import.fred("dip.csv")
ffr <- import.fred("ffr.csv")

# Estimate the SVAR equations
fit.dip <- tsreg(dip, 
                 lags(dip %~% inf %~% ffr, 1))
fit.inf <- tsreg(inf, dip %~% 
                   lags(dip %~% inf %~% ffr, 1))
fit.ffr <- tsreg(ffr, dip %~% inf %~% 
                   lags(dip %~% inf %~% ffr, 1))

D <- matrix(NA, nrow=3, ncol=3)
D
D[1,] <- c(0,0,0)
D
fit.inf
D[2,] <- c(0.026, 0, 0)
D
fit.ffr
D[3,] <- c(0.061, 0.091, 0)
D
# Now calculate A
A <- diag(3) - D
A
# Our shock
E0 <- matrix(c(0,0,1))
E0
# Impact effect
solve(A) %*% E0
solve(A)
# Initial effect on inflation and output is zero
# Effect on FFR is one
# One-step IRF
# Now we need B
B <- matrix(NA, nrow=3, ncol=3)
B
B[1,] <- c(0.0949, -0.071, 0.000)
B[2,]  <- c(-0.011, 0.994, 0.001)
B[3,] <- c(-0.043, -0.060, 0.974)
B
solve(A) %*% B %*% solve(A) %*% E0
solve(A) %*% B %*% solve(A) %*% B %*% solve(A) %*% E0

# Local projections
# From before
IRF0 <- solve(A) %*% E0
IRF0
# Now calculate one-step IRF
dip.1 <- tsreg(dip, lags(dip %~% inf %~% ffr, 1))
dip.1
0.949*0 - 0.071*0 - 0.000*1
inf.1 <- tsreg(inf, lags(dip %~% inf %~% ffr, 1))
inf.1
0.014*0 + 0.992*0 + 0.001*1

inf.6 <- tsreg(inf, lags(dip %~% inf %~% ffr, 6))
inf.6
0.064*0 + 0.891*0 + 0.014*1

# Use the vars package
library(vars)
ds <- dip %~% inf %~% ffr
varfit <- VAR(ds, lag.max=12, ic="AIC")
varfit

# Default is a recursive VAR
irf(varfit, impulse="ffr", response="inf", boot=FALSE)
irf(varfit, impulse="ffr", response="dip", boot=FALSE)
irf(varfit, impulse="ffr", response="ffr", boot=FALSE)
plot(irf(varfit, impulse="ffr", response="ffr", 
    boot=FALSE))
plot(irf(varfit, impulse="inf", response="ffr", 
         boot=FALSE, n.ahead=25))
plot(irf(varfit, impulse="inf", response="ffr", 
         n.ahead=25))

x <- matrix(c(1, 2.2, 4.4, 0, 1, 5.5, 0, 0, 1),
            ncol=3)
x
y <- diag(3)
y
diag(y) <- c(1.2, 2.4, 3.5)
y
# If you don't know x or y, but know M
M <- x %*% y %*% t(y) %*% t(x)
M
# Can we recover x and y if all we know is M
P <- t(chol(M))
P
P %*% t(P)

D <- diag(3)
D
diag(D) <- diag(P)
D
y
P %*% solve(D)
x


# Estimate the RF VAR equations
fit.dip <- tsreg(dip, 
                 lags(dip %~% inf %~% ffr, 1:12))
fit.inf <- tsreg(inf,  
                 lags(dip %~% inf %~% ffr, 1:12))
fit.ffr <- tsreg(ffr,  
                 lags(dip %~% inf %~% ffr, 1:12))
# Get the residuals
e.dip <- fit.dip$resids
e.inf <- fit.inf$resids
e.ffr <- fit.ffr$resids
# Form the covariance matrix
residuals.matrix <- e.dip %~% e.inf %~% e.ffr
v <- cov(residuals.matrix)
v
# Take the choleski
t(chol(v))
residuals.matrix
residuals.matrix2 <- e.ffr %~% e.inf %~% e.dip
v2 <- cov(residuals.matrix2)
t(chol(v2))
