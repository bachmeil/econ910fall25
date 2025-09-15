library(hpfilter)
library(tstools)
data(GDPEU)
# Downside: Requires a data frame
trend <- hp2(GDPEU, 1600)
gdp <- ts(GDPEU$gdp, frequency=4, start=c(1995,1))
trend.hp <- ts(trend$gdp, frequency=4, start=c(1995,1))
plot(trend.hp)
plot(trend.hp %~% gdp, plot.type="single", lty=c(2,1))

trend <- hp2(GDPEU, 100)
trend.100 <- ts(trend$gdp, frequency=4, start=c(1995,1))
plot(trend.100 %~% gdp, plot.type="single", lty=c(2,1))

trend <- hp2(GDPEU, 10)
trend.10 <- ts(trend$gdp, frequency=4, start=c(1995,1))
plot(trend.10 %~% gdp, plot.type="single", lty=c(2,1))

trend <- hp2(GDPEU, 160000)
trend.160000 <- ts(trend$gdp, frequency=4, start=c(1995,1))
plot(trend.160000 %~% gdp, plot.type="single", lty=c(2,1))

# Use the H filter
fit <- tsreg(gdp, lags(gdp, 8:11))
trend.h <- fit$fitted
plot(trend.h)
cyclical.h <- fit$resids
plot(cyclical.h)
plot(trend.h %~% gdp, plot.type="single", lty=c(2,1))

2 + 2.9 + 0.5*0.9 + 0.5*(-2.2)
2 + 2.7 + 0.5*0.7 + 0.5*(-1.4)
2 + 2.9 + 0.5*0.9 + 0.5*(-1.0)
2 + 2.8 + 0.5*0.8 + 0.5*(-0.2)
# Increase of 4.25 to 5.1

r.calc <- function(inf, u) {
  return(2+inf+0.5*(inf-2)+0.5*(-2*(u-5.5)))
}
r.calc(2.9, 6.6)
r.calc(2.7, 6.2)
r.calc(2.9, 6.0)
r.calc(2.8, 5.6)


