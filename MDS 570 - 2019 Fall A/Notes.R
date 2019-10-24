## GENERAL: 
## Level: The average value in the series.
## Trend:The increasing or decreasing value in the series.
## Seasonality:The repeating short - term cycle in the series.
## Noise:The random variation in the series.
##
## Additive:
##  * Components are added together
##      y(t) = Level + Trend + Seasonality + Noise
##  * Used for datasets where changes over time are consist.
##    The trend is about the same throughout the model.
##    The seasonality has the same frequency and amplitude
##      throughout the data.
##
## RSME - Route Square Mean of Errors
## AIC - Akaike's Information Criterion
## * Model with the minimum value of AIC indicates the
##   better model when considering this metric.
## * Estimate the information that would be lost if a certain
##   model were to be produced with real data.
## * Balances the trade-offs between complexity of a model and
##   how well it fits the data.

## In sample fit = Building a model with only training data and getting the values of 
## the training observe values and fitted values? I think.