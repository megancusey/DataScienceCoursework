##rm(list = ls())
kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)

tskings <- ts(kings)
tskings

plot.ts(tskings)

##install.packages("TTR")
library("TTR")

## Use a simple moving average to 
## smooth the data (order 3)
tskings.sma <- SMA(tskings, n=3)
plot.ts(tskings.sma)

## RESULT - still some random fluctuations,
## try again w/ higher order

tskings.sma <- SMA(tskings, n=8)
plot.ts(tskings.sma)

tskings.diff1 <- diff(tskings, differences=1)
plot.ts(tskings.diff1)

##ACF (MA:lag 1 significant)
## CREATE PLOT
acf(tskings.diff1,lag.max=20)
## GET VALUES ONLY
acf(tskings.diff1,lag.max = 20, plot=FALSE)

##PACF (Lag 3 significant)
pacf(tskings.diff1, lag.max=20)
pacf(tskings.diff1, lag.max=20, plot=FALSE)

## BUILD ARIMA MODEL, choose order w/ least parameteres (0,1,1)
tskings.arima <- arima(tskings, order=c(0,1,1))
tskings.arima

## Forecast
library("forecast")
tskings.forecasts <- forecast(tskings.arima, h = 5)
tskings.forecasts

## PLOT FORECASTS
plot(tskings.forecasts)

## Check out residuals
acf(tskings.forecasts$residuals,lag.max=20)
Box.test(tskings.forecasts$residuals, lag=20, type="Ljung-Box")
## No autocorrecaations from lags 1-20
## P-Value in the Ljung-Box test = .9 which concludes
## that there is little evidence for non-zero correlations in
## the forecast errors (good thing)


plot.ts(tskings.forecasts$residuals)
##plot(tskings.forecasts$residuals)
