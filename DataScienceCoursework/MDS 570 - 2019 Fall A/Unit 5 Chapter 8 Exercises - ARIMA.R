## Chapter 8 Exercises 1, 2, 3, and 5.
rm(list = ls())
## 1. Figure 8.31 shows the ACFs for 36 random numbers,
## 360 random numbers and 1,000 random numbers.

## a. Explain the differences amoung these figures. Do
## they all indicate that the data are white noise?

## Question - I am a bit confused as to how each of these
## series have 20 lags, but a different # of data. Perhaps
## we are only seeing the first 20 lags for each series.
## I'm pretty confident that series x1 and series x3 are 
## white noise, but series x2 concerns me a bit. If entire
## 360 (I supposed 360-1 to account for differencing) data points were close to the ACF intervals
## I would be concerned that there is a correlation
## between the intervals.

## b. Why are the critical values at differrent distances
## from the mean of zero? Why are the autocorrelations 
## different in each figure when they each refer to white 
## noise?

## For the data to be considered white noise, 95% of the
## data needs to be within the intervals suggested by
## the ACF with is a funtion is +/- 2/sqrt(T) where T
## is the length of the time series. The more data in
## a dataset, the small the ACF intervals are which is 
## why in series x3, the interval is much smaller than x1.

## 2. A classic example of a non-stationary series is the 
## daily closing IBM stock price series (data set ibmclose).
## Use R to plot the daily closing prices for IBM stock
## and the ACF and PACF. Explain how each plot shows the series
## is non-stationary and should be differenced.
library(fma)
imbclose.data <- ibmclose

autoplot(imbclose.data)
## The autoplot shoes signs of trends. In general, between 50 and 
## about 125 theres and upward trend followed by a more drastic
## downward trend until around 275. If the data was detrended
## I suspect we'd see some seasonality also.
ggAcf(imbclose.data)
ggPacf(imbclose.data)
## The ACF plot shows that lags are highly correlated. The PACF 
## first lag has a large spike and the rest not so large. The PACF
## though removes the effects of the first y(t)-lag 1 which is 
## why I believe the other lags do not show significant correlations


## 3. For the following series, find an appropriate Box-Cox
## transformation and order of differencing in order to obtain
## stationary data.

## a. usnetelec
## Box Cox transformation & First Differencing
## indicates stationary - eye-balled and confirmed with the 
## unit root
## test


## a. usnetelec
library(expsmooth)
library(urca)
usnetelec.transformed <- BoxCox(usnetelec, BoxCox.lambda(usnetelec))
autoplot(usnetelec) ## upward trend
autoplot(usnetelec.transformed) ## Still upward trend, no real seasonality

autoplot(diff(usnetelec))
autoplot(diff(usnetelec.transformed))


usnetelec %>% ur.kpss() %>% summary()
## original data, no differencing - indicates not stationary

usnetelec.transformed %>% ur.kpss() %>% summary()
## transformed data, no differencing - indicates not stationary

usnetelec %>% diff() %>% ur.kpss() %>% summary()
## No transformation, original data differenced, not 
## stationary

usnetelec.transformed %>% diff() %>% ur.kpss() %>% summary()
## Box Cox transformation & First Differencing, indicates stationary


## b. usgdp

library(expsmooth)
autoplot(usgdp)
## Check to see if we could just first difference the original data
usgdp %>% diff() %>% ggtsdisplay() 
## looks like not stationary enough
usgdp %>% diff() %>% ur.kpss() %>% summary()
## confirmed we need to continue differencing

usgdp.transformed <- BoxCox.lambda()

BoxCox(usgdp,BoxCox.lambda(usgdp)) %>% diff() %>% ggtsdisplay()
BoxCox(usgdp,BoxCox.lambda(usgdp)) %>% diff() %>%  ur.kpss() %>% summary()
## c. mcopper

autoplot(mcopper)
mcopper %>% log() %>% autoplot()
mcopper %>% log() %>% diff() %>% autoplot()
## this looks relatively stationary
mcopper %>% log() %>% diff() %>% ur.kpss %>% summary()
## unit test indicates the data is now stationary.

## d. enplanements
autoplot(enplanements)
enplanements %>% log() %>% diff(lag=12) %>% autoplot()
## may need more differencing
enplanements %>% log() %>% diff(lag=12) %>% ur.kpss %>% summary()
enplanements %>% log() %>% diff(lag=12) %>% diff() %>% ur.kpss %>% summary()
## 2nd differencing is better.

## e. visitors
autoplot(visitors)

visitors %>% log() %>% diff(lag=12) %>% autoplot()
## doesn't look stationary enough
visitors %>% log() %>% diff(lag=12) %>% diff() %>% autoplot()
## this looks a lot better, more stationary.
visitors %>% log() %>% diff(lag=12) %>% diff() %>% ur.kpss %>% summary()
## unit test indicates that the data are now stationary.

## 5. For your retail data (from Exercise 3 in Section 2.10), find
## the appropriate order of differencing (after transformation if
## necessary) to obtain stationary data.

setwd("C:/Users/cusey/Source/Repos/TimeSeriesProjectts/TimeSeriesProjects/TimeSeriesProjects")

data <- read.csv("retail-csv.csv", skip = 1)

myts <- ts(data[, "A3349873A"],
           frequency = 12, start = c(1982, 4))

autoplot(myts)

myts.transformed <- BoxCox(myts, BoxCox.lambda(myts))

autoplot(myts.transformed)


## First I'll do a data transformation to help smooth the
## trend

## The data looks seasonal so I'll try a seasonal trend

myts.transformed.seasonaldif <- myts.transformed %>% diff(lag=12) %>% ggtsdisplay() 

myts.transformed %>% diff(lag=12) %>% ur.kpss() %>% summary()

## check out the unit test
## Depending on significance level, could be ok. May do another
## differencing step to get it a little more stationary.

myts.transformed %>% diff(lag=12) %>% diff() %>% ur.kpss() %>% summary()




