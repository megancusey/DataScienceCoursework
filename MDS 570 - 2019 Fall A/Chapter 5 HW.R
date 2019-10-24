library(fma)
library(fpp2)

## Linear Regression

## Chapter 5, Exercise 5

## The data set fancy concerns the monthly sales 
## figures of a shop which opened in January 1987 
## and sells gifts, souvenirs, and novelties. The 
## shop is situated on the wharf at a beach resort 
## town in Queensland, Australia. The sales volume 
## varies with the seasonal population of tourists.
## There is a large influx of visitors to the town 
## at Christmas and for the local surfing festival, 
## held every March since 1988. Over time, the shop
## has expanded its premises, range of products, and
## staff.
##

## a.	Produce a time plot of the data and describe 
##    the patterns in the graph. Identify any unusual 
##    or unexpected fluctuations in the time series.

autoplot(fancy)

## The fancy sales data displays seasonal and trend characteristics
## which indicates a linear transformation may be necessary. As described
## in the description of the data, it is noticeable that sales tend to increase
## during the end of the year (Christmas) and a small peak within a few months into
## a new year (March) with an overall increase of sales, likely due to the shop's
## expansion. What is unexplained is that the more recent data after 1991, it shows
## an increase in sales much earlier then December and March no longer seems like
## one of their most popular seasons.

## b.	Explain why it is necessary to take logarithms
##    of these data before fitting a model.

## Attempting to fit a linear trend to data with a lot of volatile
## data points will result in heteroscedasticity residuals. It is
## better to take the logarithm of the data points to lessen the effects trends 
## and seasonality.

## c. Use R to fit a regression model to the 
##    logarithms of these sales data with a linear 
##    trend, seasonal dummies and a âsurfing festivalâ
##    dummy variable.

## transform sales data with log of sales
log.fancy <- log(fancy)

## create dummy variable for surfing festival
dummy.fest = rep(0, length(fancy))
dummy.fest[seq_along(dummy.fest) %% 12 == 3] <- 1

## March 1987 didn't have a festival
dummy.fest[3] <- 0

## create dummy fest as a time series object
dummy.fest <- ts(dummy.fest, freq = 12, start = c(1987, 1))

## combine data
model_data <- data.frame(log.fancy, dummy.fest)

## create regression line
fit.fancy <- tslm(fancy ~ trend + season + dummy.fest, data = model_data)

## Plot regression model fitted vs data
autoplot(fancy, series = "Data") +
    autolayer(fitted(fit.fancy), series = "Fitted") +
    xlab("Year") + ylab("Sales") +
    ggtitle("Fancy Sales/Year")


##  d. Plot the residuals against time and against the fitted values.
##     Do these plots reveal any problems with the model?

autoplot(fit.fancy$residuals)
## The residuals are supposed to have a mean of 0 or else the 
## the model may be bias. In this plot, the residuals contrast
## from 0 a lot which is concerning to me.

##  e. Do boxplots of the residuals for each month. Does this reveal 
##     any problems with the model?

boxplot(resid(fit.fancy) ~ cycle(resid(fit.fancy)))

## The residuals for the end of the year - beginning of the year 
## (November - January) show a higher variance. Also, there are outliers in November and 
## December.

##  f. What do the values of the coefficients tell you about each variable?

summary(fit.fancy)

## There is an average upward trend of 321.83 in sales. On
## average February has sales of 994.19 more than January.
## March has 8427.58 more sales than January (and so forth for each
## season listed). November and December are the months with the
## biggest average increase in sales compared to January.

##  g. What does the Breusch-Godfrey test tell you about your model?

checkresiduals(fit.fancy)

## The Breusch-Godfrey test indicates that there could be some
## auto correlation between residuals which indicates
## there's a pattern we are not leveraging to forecast
## in the data.

##  h. Regardless of your answers to the above questions, use your regression
##     model to predict the monthly sales for 1994, 1995, and 1996.
##     Produce prediction intervals for each of your forecasts.

future.data <- data.frame(dummy.fest = rep(0, 36))
forecast.fancy <- forecast(fit.fancy, newdata = future.data)
plot(forecast.fancy)
summary(forecast.fancy)
##  i. Transform your predictions and intervals to obtain predictions and 
##     intervals for the raw data.

data <- as.data.frame(forecast.fancy)
data <- exp(data$`Point Forecast`)

data

##  j. How could you improve these predictions by modifying the model?
## We could research the data more and try to identify what
## is causing auto correlation in the residuals and try
## to extract that information to use in forecasting.