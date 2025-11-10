objfun <- function(par) {
  c <- par[1]
  vu <- par[2]
  vi <- par[3]
  return( (0.169-vu)^2 + 
            (0.198-c^2*vu-vi)^2 +
            (-0.03-c*vu)^2 )
}
objfun(c(-0.1, 1, 1))
optim(c(-0.1, 1, 1), objfun)

eq2 <- tsreg(inf, u %~% lags(u %~% inf, 1))
eq2

objfun.nlslv <- function(par) {
  c <- par[1]
  vu <- par[2]
  vi <- par[3]
  return(c(0.169-vu, 0.198-c^2*vu-vi,
           -0.03-c*vu))  
}
library(nleqslv)
nleqslv(c(-0.1, 1, 1), objfun.nlslv)

objfun.gmm <- function(par, data) {
  c <- par[1]
  vu <- par[2]
  vi <- par[3]
  dev1 <- data[,"res1"]^2 - vu
  dev2 <- data[,"res2"]^2 - c^2*vu - vi
  dev3 <- data[,"res1"]*data[,"res2"] - c*vu
  return(cbind(dev1, dev2, dev3))
}

rf.errors <- e1 %~% e2
colnames(rf.errors) <- c("res1", "res2")
objfun.gmm(c(-0.1, 1, 1), rf.errors)
library(gmm)
gmm(objfun.gmm, rf.errors, t0=c(-0.1, 1, 1))

# Overidentified b = 0 AND vu = vi
objfun.gmm <- function(par, data) {
  c <- par[1]
  v <- par[2]
  dev1 <- data[,"res1"]^2 - v
  dev2 <- data[,"res2"]^2 - c^2*v - v
  dev3 <- data[,"res1"]*data[,"res2"] - c*v
  return(cbind(dev1, dev2, dev3))
}
gmm(objfun.gmm, rf.errors, t0=c(-0.1, 1))
