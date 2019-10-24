## Chapter 8 Exercises 8 & 9
library(expsmooth)
library(forecast)
library(fpp2)
library(urca)

rm(list = ls())
## 8. Consider austa, the total international visitors to
##    Australia (in millions) for the period 1980-2015
##
      autoplot(austa)
##############################

##  a. Use auto.arima() to find an appropriate ARIMA
##     model. What model was selected. Check that the 
##     residuals look like white noise. Plot forecasts
##     for the next 10 periods.
##
    
      austa.auto.arima <- austa %>% auto.arima() 
      austa.auto.arima %>% summary()  
      ## ARIMA(0,1,1) with drift
      ## AIC = -15.24
      ## AICc = -14.46
      
      
      austa.auto.arima %>% forecast(h=10) %>% autoplot()
      
      checkresiduals(austa.auto.arima)
      ## the residuals look close to normal
      ## there is no autocorrelation between the residuals so
      ## they look like white noise.
      
      
##  b. Plot forecasts from an ARIMA(0,1,1) model with no
##     drift and compare these to part a. Remove the MA
##     term and plot again.
      
      austa.arima.0.1.1 <-  austa %>% Arima(order=c(0,1,1), include.constant = FALSE)
      
      austa.arima.0.1.1
      ## AIC = -3.45
      ## AICc = -3.08
      
      ## compared to auto.arima() result with drift, the forecast doesn't
      ## seem as practical since there is no trend in the forecast points.
      
      ## include.constant = TRUE will set include.mean=TRUE if d = 0
      ## and if include.drift=TRUE if d = 1. 
      ## If include.constant = FALSE, both include.mean and include.drift = FALSE
      
      austa.arima.0.1.1 %>% forecast(h=10) %>% autoplot()
      
      austa %>% Arima(order=c(0,1,0), include.constant = FALSE) %>% forecast(h=10) %>% autoplot()
      ## AIC 5.24
      ## AIC 5.36
      ## forecast points are not much different when we remove the moving average
      ## parameter.
      
##  c. Plot forecasts from an ARIMA (2,1,3) model with 
##     drift. Remove the constant and see what happens.
    
      
        austa %>% Arima(order=c(2,1,3), include.constant = TRUE) %>% forecast(h=10) %>% autoplot()
##      AIC -13.44
##      AICc  -9.29
        
        ## The prediction interval appears to be smaller than the
        ## auto.arima() model choice. The forecast points curve
        ## a bit more.
        
        austa %>% Arima(order=c(2,1,3), include.constant = FALSE) %>% forecast(h=10) %>% autoplot()
        
        ## an error occurs stating the data isn't stationary.

##  d. Plot forecasts from an ARIMA(0,0,1) model with
##     drift. Remove the MA term and plot again.
##
        austa %>% Arima(order=c(0,0,1), include.constant = TRUE) %>% forecast(h=10) %>% autoplot()
        ## AICc 108.03
        ## AIC 107.28
        
        austa %>% Arima(order=c(0,0,0), include.constant = TRUE) %>% forecast(h=10) %>% autoplot()
        ## AICc 148.17
        ## AIC 147.8
        
        ## Forecast points eventually = to the mean of the dataset.
        
##  e. Plot forecasts from an ARIMA (0,2,1) model with
##     no constant.

        austa %>% Arima(order=c(0,2,1), include.constant = FALSE) %>% forecast(h=10) %>% autoplot()

## The forecast points have a linear upward trend but the prediction intervals
## get increasingly bigger.
        
        
## 9. For the usgdp series:
      autoplot(usgdp)
      
##  a. If necessary, find a suitable Box-Cox transformation
##     for the data
      
      ## De-trend series and reduce variances
      autoplot(BoxCox(usgdp, BoxCox.lambda(usgdp)))
##
##  b. Fit a suitable ARIMA model to the transformed data
##     using auto.arima();
      
      BoxCox(usgdp, BoxCox.lambda(usgdp)) %>% auto.arima()
##  Model chosen: ARIMA(2,1,0) with drift AICc = -114.94 , AIC = -115.11
##  c. Try some other plausible models by experimenting with 
##     the orders chosen;
##
      BoxCox(usgdp, BoxCox.lambda(usgdp)) %>% diff() %>% ur.kpss() %>% summary()
      BoxCox(usgdp, BoxCox.lambda(usgdp)) %>% diff() %>% arima(order=c(2,1,1)) 
      ## AIC = -109.16
      BoxCox(usgdp, BoxCox.lambda(usgdp)) %>% diff() %>% arima(order=c(3,1,0))
      ## AIC = -69.08
      BoxCox(usgdp, BoxCox.lambda(usgdp)) %>% diff() %>% arima(order=c(1,1,0))
      ## AIC = -62.85
      BoxCox(usgdp, BoxCox.lambda(usgdp)) %>% diff() %>% arima(order=c(1,1,1))
      ## AIC = -107.49
      BoxCox(usgdp, BoxCox.lambda(usgdp)) %>% diff() %>% arima(order=c(2,1,2))
      ## AIC = -108.35

            
##  d. Choose what you think is the best model and check the
##     residual diagnostics;
##
##     According to AIC, ARIMA(2,1,0) is the best model.
      BoxCox(usgdp, BoxCox.lambda(usgdp)) %>% diff() %>% arima(order=c(2,1,1)) %>% checkresiduals()
      ## The model passed the ljung-box test. The historgram doesn't look exactly normal.
      
##  e. Produce forecasts of your fitted model. Do the forecasts
##     look reasonable?
      usgdp %>% auto.arima() %>% forecast(h=10) %>% autoplot()
##    The forecast points look reasonable.
      
##  f. Compare the results with what you would obtain using
##     ets() with no transformation.
      
      usgdp %>% ets %>% forecast(h=10) %>% autoplot()
      
## ARIMA forecast points are higher than the ETS.
      