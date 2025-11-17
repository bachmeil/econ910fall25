library(tstools)
inf <- import.fred("inflation.csv")
u <- import.fred("unrate.csv")

eq1.t1 <- tsreg(u, lags(u %~% inf, 1),
                end=c(1983,12))
eq2.t1 <- tsreg(inf, lags(u %~% inf, 1),
             end=c(1983,12))
e1.t1 <- eq1.t1$resids
e2.t1 <- eq2.t1$resids
cov(e1.t1 %~% e2.t1)

eq1.t2 <- tsreg(u, lags(u %~% inf, 1),
                start=c(1984,1))
eq2.t2 <- tsreg(inf, lags(u %~% inf, 1),
                start=c(1984,1))
e1.t2 <- eq1.t2$resids
e2.t2 <- eq2.t2$resids
cov(e1.t2 %~% e2.t2)
plot(e1.t1, e2.t1)
plot(e1.t2, e2.t2)

objfun.nlslv <- function(par) {
  b <- par[1]
  c <- par[2]
  vu.t1 <- par[3]
  vi.t1 <- par[4]
  vu.t2 <- par[5]
  vi.t2 <- par[6]
  return(c(0.06-vu.t1-b^2*vi.t1, 
           0.25-c^2*vu.t1-vi.t1,
           -0.02-c*vu.t1-b*vi.t1,
           0.26-vu.t2-b^2*vi.t2, 
           0.15-c^2*vu.t2-vi.t2,
           -0.04-c*vu.t2-b*vi.t2))  
}
library(nleqslv)
nleqslv(c(0.1, -0.1, 1, 1, 1, 0.5), 
        objfun.nlslv)


eq1 <- tsreg(u, lags(u %~% inf, 1))
e1 <- eq1$resids

# Wild bootstrap
#z <- sample(c(-1,1), size=length())

# Local projections
eq1.1 <- tsreg(u, lags(u %~% inf, 1))
eq2.1 <- tsreg(inf, lags(u %~% inf, 1))
summary(eq1.1)
