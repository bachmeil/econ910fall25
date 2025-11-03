library(tstools)
inf <- import.fred("inflation.csv")
dip <- import.fred("dip.csv")
ffr <- import.fred("ffr.csv")

# Reduced form estimation
fit.dip <- tsreg(dip, 
                 lags(dip %~% inf %~% ffr, 1))
fit.inf <- tsreg(inf, dip %~% 
                   lags(dip %~% inf %~% ffr, 1))
fit.ffr <- tsreg(ffr, dip %~% inf %~% 
                   lags(dip %~% inf %~% ffr, 1))
shock.mon <- fit.ffr$resids
plot(shock.mon)
shock.inf <- fit.inf$resids
plot(shock.inf)
shock.output <- fit.dip$resids
plot(shock.output)
# Calculate IRFs for monetary policy shock
# Romer and Romer approach
irf.dip <- tsreg(dip, lags(shock.mon, 0:6))
irf.dip
# Plot the IRF
irf.values <- coefficients(irf.dip)[-1]
plot(ts(irf.values), lwd=1.8)
# Cumulative - effect on output, not growth
plot(ts(cumsum(irf.values)), lwd=1.8)
irf.inf <- tsreg(inf, lags(shock.mon, 0:6))
irf.inf
irf.values <- coefficients(irf.inf)[-1]
plot(ts(irf.values), lwd=1.8)
# Own impulse response function
irf.ffr <- tsreg(ffr, lags(shock.mon, 0:6))
irf.ffr





