## Let's start by establishing why you need to use simulation when forecasting
## multiple steps ahead with a nonlinear model.

## Simple AR(1) model y(t) = 2.5 + 0.5 y(t-1) + e(t)
## Iterate to make a forecast of y(T+2)
## Assume y(T) = 1 and all future residuals are zero
`y(T+1)` <- 2.5 + 0.5*1 + 0
`y(T+1)`
`y(T+2)` <- 2.5 + 0.5*`y(T+1)` + 0
`y(T+2)`

## Now redo it assuming y(T) = 2 and all future residuals are zero
`y(T+1)` <- 2.5 + 0.5*2 + 0
`y(T+1)`
`y(T+2)` <- 2.5 + 0.5*`y(T+1)` + 0
`y(T+2)`

## Now redo it assuming y(T) = 3 and all future residuals are zero
`y(T+1)` <- 2.5 + 0.5*3 + 0
`y(T+1)`
`y(T+2)` <- 2.5 + 0.5*`y(T+1)` + 0
`y(T+2)`

## You can verify that increasing the initial value y(T) by one unit always
## has the same effect on the forecast y(T+2). That's expected because the
## marginal effect is constant in a linear model.

## You can do the same for a change in the residual. The same change in one of
## the residuals has a constant effect on the forecast y(T+2).
## Redo the above for y(T) = 0 and e(T+1) = 0, e(T+1) = 1, and e(T+2) = 2.
`y(T+1)` <- 2.5 + 0.5*0 + 0
`y(T+1)`
`y(T+2)` <- 2.5 + 0.5*`y(T+1)` + 0
`y(T+2)`

`y(T+1)` <- 2.5 + 0.5*0 + 1
`y(T+1)`
`y(T+2)` <- 2.5 + 0.5*`y(T+1)` + 0
`y(T+2)`

`y(T+1)` <- 2.5 + 0.5*0 + 2
`y(T+1)`
`y(T+2)` <- 2.5 + 0.5*`y(T+1)` + 0
`y(T+2)`
## A one unit increase in e(T) causes y(T+2) to rise by 0.5 no matter
## the residuals or initial vaue we use. All as expected in a linear model,
## where marginal effects don't depend on the initial value or the future residuals.

## Now use the threshold model
## y(t) = 1.0 + 0.5 y(t-1) + e(t) if y(t-1) > 0
## y(t) = -1.0 + 0.2 y(t-1) + e(t) if y(t-1) <= 0
## Find y(T+2) for y(T) = -0.5, 0.5, and 1.5 and all future residuals equal to zero
## y(T) = -0.5 case
`y(T+1)` <- -1.0 + 0.2*(-0.5) + 0
`y(T+1)`
`y(T+2)` <- -1.0 + 0.2*`y(T+1)` + 0
`y(T+2)`

## y(T) = 0.5 case
`y(T+1)` <- 1.0 + 0.5*(0.5) + 0
`y(T+1)`
`y(T+2)` <- 1.0 + 0.5*`y(T+1)` + 0
`y(T+2)`

## y(T) = 1.5 case
`y(T+1)` <- 1.0 + 0.5*(1.5) + 0
`y(T+1)`
`y(T+2)` <- 1.0 + 0.5*`y(T+1)` + 0
`y(T+2)`

## Going from y(T) = -0.5 to 0.5 caused y(T+2) to rise from -1.22 to 1.625
## Going from y(T) = 0.5 to 1.5 caused y(T+2) to rise from 1.625 to 1.875
## Marginal effects in the threshold model are not constant
## Of course, we know from prior econometrics courses that nonlinear models
## do not have constant marginal effects. Changes in the initial values or
## the future residuals can cause a change in the marginal effects. One approach
## is to condition on a choice of the initial value and future residuals, but
## it would be hard to do that. The alternative is to average over all possible
## sets of initial values and future residuals. You do that by simulation,
## similar to the simulations in a bootstrap.
