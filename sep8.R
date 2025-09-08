library(tstools)

# Load the data
dgdp <- import.fred("dgdp.csv")
plot(dgdp)

# Estimate AR model
fit <- tsreg(dgdp, lags(dgdp,1))
fit

# Limit the start and end of the data
fit <- tsreg(dgdp, lags(dgdp,1), start=c(1950,1),
             end=c(2019,4))
fit

# Choose lag length
fit.1 <- tsreg(dgdp, lags(dgdp,1), start=c(1950,1),
             end=c(2019,4))
fit.2 <- tsreg(dgdp, lags(dgdp,1:2), start=c(1950,1),
             end=c(2019,4))
fit.3 <- tsreg(dgdp, lags(dgdp,1:3), start=c(1950,1),
             end=c(2019,4))
AIC(fit.1)
AIC(fit.2)
AIC(fit.3)
fit <- tsreg(dgdp, lags(dgdp, c(1,3)), start=c(1950,1),
             end=c(2019,4))
BIC(fit.1)
BIC(fit.2)
BIC(fit.3)
window(dgdp, c(2019,2), c(2019,4))
fit.3

# Calculate the forecast for 2020Q1
0.87 + 1.18*3.35 - 0.26*2.80 - 0.19*2.24
# Forecast was 3.67
# Now compare with actual
tsobs(dgdp, c(2020,1))
# Actual was 1.28
# Shock
1.28 - 3.67
# Not a "GDP shock" -> Not exogenous
# This could be interpreted as a pandemic shock
# Measuring effect of pandemic in units of GDP growth

# ARMA(1,1)
arma11 <- arima(dgdp, order=c(1,0,1))
predict(arma11, 1)