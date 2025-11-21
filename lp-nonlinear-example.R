## We're continuing on from the linear example, but now we have a threshold model
## We allow for different effects if the economic activity shock hits when u > 6
## Reuse the impact effects from the linear example
u.high <- u > 6
interaction.u <- u.high * u
interaction.inf <- u.high * inf
`h=1 LP fit` <- tsreg(inf, lags(u.high %~% inf %~% u %~% interaction.u %~% interaction.inf, 1))
`h=1 LP fit`

## Calculate the effect of a +1 economic activity shock (u rises by one unit) if
## u = 8 when the shock hits.
`inf(T+1|shock)` <- -0.087788 + 0.469276 + 1.027559*(-0.08) + 0.005324*(1) -
    0.048500*(1) - 0.050146*(-0.08)
`inf(T+1|no shock)` <- -0.087788 + 0.469276 + 1.027559*0 + 0.005324*0 -
    0.048500*0 - 0.050146*0
`inf(T+1|shock)` - `inf(T+1|no shock)`

## Now do a simplified calculation
1.027559*(-0.08) + 0.005324*(1) - 0.048500*(1) - 0.050146*(-0.08)

## That's the same answer. The reason is that the model is linear once you specify the
## regime you start in. Therefore, the general procedure is exactly the same whether the
## model is linear or nonlinear, but the model specification changes when you add
## nonlinearity.
