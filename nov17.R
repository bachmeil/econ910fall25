library(tstools)
inf <- import.fred("inflation.csv")
u <- import.fred("unrate.csv")

library(vars)
# Estimate the RF VAR
rfvar <- VAR(u %~% inf, p=2)
F <- formF(rfvar, constant=FALSE)
F

# Impact effect
Z0 <- matrix(c(1, -0.13, 0, 0), ncol=1)
Z0
# One-step IRF
t1 <- F %*% Z0
t1[2]
# One-step ahead inflation IRF
# Two-step IRF
t2 <- matPower(F, 2) %*% Z0
t2[2]
matPower(F, 600) %*% Z0

# Now do inference
# How much do they change in a different
# sample?

# For convenience
eq.u <- tsreg(u, lags(u %~% inf, 1:2))
eq.inf <- tsreg(inf, lags(u %~% inf, 1:2))

# Do the wild bootstrap
set.seed(200)
boot.inf <- replicate(1000, {
  z <- sample(c(-1,1),
              size=length(eq.u$resids),
              replace=TRUE)
  u.sim <- eq.u$fitted +
    z*eq.u$resids
  inf.sim <- eq.inf$fitted +
    z*eq.inf$resids
  
  # Estimate RF VAR using the simulated
  # observations
  rfvar <- VAR(u.sim %~% inf.sim, p=2)
  F <- formF(rfvar, constant=FALSE)
  Z1 <- F %*% Z0
  Z2 <- matPower(F, 2) %*% Z0
  return(c(Z1[2], Z2[2]))
})
dim(boot.inf)  
sd(boot.inf[1,])  
quantile(boot.inf[1,], 
         prob=c(0.025,0.975))
1.96*0.04868769
-0.18
quantile(boot.inf[2,], 
         prob=c(0.025,0.975))
quantile(boot.inf[1,], 
         prob=c(0.32,0.68))
quantile(boot.inf[1,], 
         prob=c(0.05,0.95))

# h=1
lp.irf.1 <- tsreg(inf,
                  lags(u %~% inf, 1:3))
# h=2
lp.irf.2 <- tsreg(inf,
                  lags(u %~% inf, 2:4))
lp.irf.1
# h=1 point estimate
0.0002394*1.0+1.37*(-0.13)

library(msm)
b <- lp.irf.1$coefficients
v <- summary(lp.irf.1)$cov.unscaled
se1 <- deltamethod(~ 1.0*x2 - 0.13*x5,b,v)
se1
# 95% confidence band
-0.1778 + 1.96*0.08
-0.1778 - 1.96*0.08
