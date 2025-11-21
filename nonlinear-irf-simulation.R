## Let's work with the nonlinear model from before. We allow both equations to be nonlinear.
## Reuse the impact effects from the linear example
library(tstools)
inf <- import.fred("inflation.csv")
u <- import.fred("unrate.csv")

u.high <- u > 6
interaction.u <- u.high * u
interaction.inf <- u.high * inf
var.u <- tsreg(u, lags(u.high %~% inf %~% u %~% interaction.u %~% interaction.inf, 1))
var.u
var.inf <- tsreg(inf, lags(u.high %~% inf %~% u %~% interaction.u %~% interaction.inf, 1))
var.inf
## Confirm that they cover the same time period
var.inf$start_date
var.inf$end_date
var.u$start_date
var.u$end_date
## Now we need to draw an initial value and block of future residuals
## Compute the forecast of inf(T+2) with and without the shock
## And take the average difference of the two across many draws
set.seed(200)
simoutput <- replicate(10000, {
    ## Take a draw of one initial value for u and inf
    ## The draw of u determines the regime
    z <- sample(1:length(u), size=1)
    init.u <- u[z]
    init.inf <- inf[z]

    ## Take a draw from the residuals
    z <- sample(1:length(var.u$resids), size=1)
    e.u <- var.u$resids[z]
    e.inf <- var.inf$resids[z]

    ## We need to define these variables below
    `u(T+1|shock)` <- `u(T+1|no shock)` <- `u(T+2|shock)` <- `u(T+2|no shock)` <- NULL
    `inf(T+1|shock)` <- `inf(T+1|no shock)` <- `inf(T+2|shock)` <- `inf(T+2|no shock)` <- NULL
    
    ## Compute the next values depending on the regime
    if (init.u > 6) {
        `u(T+1|shock)` <- 0.121747 + 0.293068 + 0.001899*(init.inf-0.08) + 0.977245*(init.u+1) - 0.046044*(init.u+1) + 0.016159*(init.inf-0.08) + e.u
        `inf(T+1|shock)` <- -0.25143 + 0.29196 + 1.00069*(init.inf-0.08) + 0.05769*(init.u+1) - 0.05708*(init.u+1) - 0.03004*(init.inf-0.08) + e.inf
        `u(T+1|no shock)` <- 0.121747 + 0.293068 + 0.001899*init.inf + 0.977245*init.u - 0.046044*init.u + 0.016159*init.inf + e.u
        `inf(T+1|no shock)` <- -0.25143 + 0.29196 + 1.00069*init.inf + 0.05769*init.u - 0.05708*init.u - 0.03004*init.inf + e.inf
    } else {
        `u(T+1|shock)` <- 0.121747 + 0.001899*(init.inf-0.08) + 0.977245*(init.u+1) + e.u
        `inf(T+1|shock)` <- -0.25143 + 1.00069*(init.inf-0.08) + 0.05769*(init.u+1) + e.inf
        `u(T+1|no shock)` <- 0.121747 + 0.001899*init.inf + 0.977245*init.u + e.u
        `inf(T+1|no shock)` <- -0.25143 + 1.00069*init.inf + 0.05769*init.u + e.inf
    }
    
    ## Now we have to consider the possibility that we changed regimes. So the time T+2
    ## forecasts need to be computed based on the regime at time T+1. Moreover, we have
    ## to consider that the shock and no shock cases might have different regimes for
    ## forecasting T+2.
    
    ## Take another draw from the residuals
    z <- sample(1:length(var.u$resids), size=1)
    e.u <- var.u$resids[z]
    e.inf <- var.inf$resids[z]

    if (`u(T+1|shock)` > 6) {
        `u(T+2|shock)` <- 0.121747 + 0.293068 + 0.001899*`inf(T+1|shock)` + 0.977245*`u(T+1|shock)` - 0.046044*`u(T+1|shock)` + 0.016159*`inf(T+1|shock)` + e.u
        `inf(T+2|shock)` <- -0.25143 + 0.29196 + 1.00069*`inf(T+1|shock)` + 0.05769*`u(T+1|shock)` - 0.05708*`u(T+1|shock)` - 0.03004*`inf(T+1|shock)` + e.inf
    } else {
        `u(T+2|shock)` <- 0.121747 + 0.001899*`inf(T+1|shock)` + 0.977245*`u(T+1|shock)` + e.u
        `inf(T+2|shock)` <- -0.25143 + 1.00069*`inf(T+1|shock)` + 0.05769*`u(T+1|shock)` + e.inf
    }

    if (`u(T+1|no shock)` > 6) {
        `u(T+2|no shock)` <- 0.121747 + 0.293068 + 0.001899*`inf(T+1|no shock)` + 0.977245*`u(T+1|no shock)` - 0.046044*`u(T+1|no shock)` + 0.016159*`inf(T+1|no shock)` + e.u
        `inf(T+2|no shock)` <- -0.25143 + 0.29196 + 1.00069*`inf(T+1|no shock)` + 0.05769*`u(T+1|no shock)` - 0.05708*`u(T+1|no shock)` - 0.03004*`inf(T+1|no shock)` + e.inf
    } else {
        `u(T+2|no shock)` <- 0.121747 + 0.001899*`inf(T+1|no shock)` + 0.977245*`u(T+1|no shock)` + e.u
        `inf(T+2|no shock)` <- -0.25143 + 1.00069*`inf(T+1|no shock)` + 0.05769*`u(T+1|no shock)` + e.inf
    }
    c(`inf(T+2|shock)` - `inf(T+2|no shock)`, `u(T+2|shock)` - `u(T+2|no shock)`)
})
dim(simoutput)
## The mean of row 1 is the 2-step ahead IRF for inflation
## The mean of row 2 is the 2-step ahead IRF for u
## Importantly, you need to specify this is the average over all possible
## initial values
## There were no conditions on the initial values
rowMeans(simoutput)
