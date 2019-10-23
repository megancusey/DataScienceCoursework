##Chapter 7: Exercises 8, 9, 10

library(fpp2)

## 8. Recall your retail time series data (from Exercise 3
##    in section 2.10).

    ## Read Data
    retail.data <- read.csv("retail-csv.csv", skip = 1)

    retail.data.ts <- ts(retail.data[, "A3349873A"],
               frequency = 12, start = c(1982, 4))

    autoplot(retail.data.ts)
##    a. Why is multiplicative seasonality necessary for this
##       series?  

        retail.data.ts.additive <- decompose(plastics, type = "additive")
        retail.data.ts.multiplicative <- decompose(plastics, type = "multiplicative")

        autoplot(retail.data.ts.additive)
        autoplot(retail.data.ts.multiplicative)

##      When plotting the original data (retail.data.ts), you can
##      see that the data is pretty inconsistent in trend and seasonality.
##      the difference between the range of data points between 1980-1990 
##      are much smaller than 2010+. In addition, the trend doesn't appear to
##      be consistently upwards. When comparing the decompositions, I 
##      noticed that the multiplicative method had remainders on a lower scale than
##      additive. All of the above considered makes sense to classify the dataset as
##      having multiplicative seasonality.

##    b. Apply Holt-Winters' multiplicative method to the data.
##       Experiment with making the trend damped.

retail.data.ts.holt.multiplative.damped <- hw(retail.data.ts, damped = TRUE, seasonal = "multiplicative")
retail.data.ts.holt.multiplative <- hw(retail.data.ts, damped = FALSE, seasonal = "multiplicative")

autoplot(retail.data.ts) +
    autolayer(retail.data.ts.holt.multiplative.damped, series = "Damped", PI = FALSE) +
    autolayer(retail.data.ts.holt.multiplative, series = "Not Damped", PI = FALSE) +
    xlab("Year") +
    ylab("Retail Sales") +
    ggtitle("Retail Sales Damped VS Not Damped")
##    c. Compare the RMSE of the one-step forecasts from the two
##       methods. Which do you prefer?
accuracy(retail.data.ts.holt.multiplative.damped)
## RMSE: 13.30494
accuracy(retail.data.ts.holt.multiplative)
## RMSE: 13.29378
## The difference between root mean squared error is minimal between the damped VS not damped multiplicative
## methods. Since the method that does not apply the damped parameter has a lower RMSE, it is the preferred model.

##    d. Check that the residuals from the best method look like
##       white noise.
checkresiduals(retail.data.ts.holt.multiplative)
## according to the autocorrelation graph from the checkresiduals function, the residuals
## are correlated which means that the residuals are not just white noise.

##    e. Now find the test set RMSE while training the model to the
##       end of 2010. Can you beat the seasonal naive approach from 
##       Exercise 8 in Section 3.7?

## Split data into test/train sets
retail.data.train <- window(retail.data.ts, end = c(2010, 12))
retail.data.test <- window(retail.data.ts, start = 2011)

## Check Test/Train Sets
autoplot(retail.data.ts) +
    autolayer(retail.data.train, series = "Training") +
    autolayer(retail.data.test, series = "Test")

## Seasonal Naive
retail.data.seasonal.naive <- snaive(retail.data.train)
accuracy(retail.data.seasonal.naive, retail.data.test)
##RMSE 71.44309 on test set

## HOLT NOT DAMPED
retail.data.ts.holt.multiplative.2 <- hw(retail.data.train, damped = FALSE, seasonal = "multiplicative")
accuracy(retail.data.ts.holt.multiplative.2, retail.data.test)
## 70.116586

## HOLT DAMPED
retail.data.ts.holt.multiplative.damped.2 <- hw(retail.data.train, damped = TRUE, seasonal = "multiplicative")
accuracy(retail.data.ts.holt.multiplative.damped.2, retail.data.test)
## 81.946499

## Holt-Winter Multiplicative (no damping parameter) produces the best accuracy results when
## splitting the data set into test/train and comparing RSME. Seasonal Naive does appear
## to do better than the Holt-Winter Multiplactive Method with a damping parameter.

## 9. For the same retail data, try an STL decomposition applied to the 
##    Box-Cox transformed series, followed by ETS on the seasonally adjusted
##    data. How does that compare with your best previous forecasts on the 
##    test set?

retail.data.transformed.boxcox.train.stl <- retail.data.train %>%
    stlm(
         s.window = 13,
         robust = TRUE,
         method = "ets",
         lambda = BoxCox.lambda(retail.data.train),
         biasadj = TRUE
     ) %>%
     forecast(
              h = 36,
              lambda = BoxCox.lambda(retail.data.train),
              biasadj=TRUE
     )

autoplot(retail.data.transformed.boxcox.train.stl)
accuracy(retail.data.transformed.boxcox.train.stl,retail.data.test)

## Applying the adjust bias reduces the RMSE. Without bias adjustment the
## RMSE is 98.XX. With bias adjustment, the RMSE is 96.615122. However,
## The ETS applying STL Decomposition with Box Cox transformation does not
## provide a lower RMSE then Holt Winters Multiplative method.

rm(list = ls())

## 10. For this exercise use data set ukcars, the quarterly UK passenger
##     vehicle production data from 1977Q1-2005Q1.
library(expsmooth)

ukcars.original.ts <- ukcars

##     a. Plot the data and describe the main features of the series.
        autoplot(ukcars.original.ts)

##      The data appear to have an inconsistent trend which is a bit odd for 
##      car production data. Near 1985, it appears to be around 300,000-350,000 
##      cars in production then taking a nose dive right before 1980. I would assume
##      car production would almost always have an upward trend as populations increase
##      but perhaps it was an economy crash (perhaps a predictor variable's influence
##      thats on a trend-cycle?) or something that needs to be accounted for.
##      The data also seems to have a bit of seasonality to it.

##     b. Decompose the series using STL and obtain the seasonally  
##        adjusted data.

        ukcars.stl <- mstl(ukcars.original.ts)
        autoplot(ukcars.stl)
        ukcars.stl.seasadj <- seasadj(ukcars.stl)
        ukcars.stl.seasonal <- seasonal(ukcars.stl)
        ukcars.stl.trendcycle <- trendcycle(ukcars.stl)

        autoplot(ukcars.stl.seasadj)

        autoplot(ukcars.original.ts) +
            autolayer(ukcars.stl.seasadj, series = "Seasonal Adjusted") +
            autolayer(ukcars.stl.seasonal, series = "Seasonal") +
            autolayer(ukcars.stl.trendcycle, series = "Trend-Cycle")

##     c. Forecast the next two years of the series using an additive
##        damped trend method applied to the seasonally adjusted data.
##        (This can be done in one step using stlf() with arguments
##        etsmodel="AAN", damped=TRUE.)

        ukcars.stl.seasadj.forecast.AAN.damped <- stlf(ukcars.stl.seasadj, h = 8, damped = TRUE)
        autoplot(ukcars.stl.seasadj.forecast.AAN.damped)

##     d. Forecast the next two years of the series using Holt's linear
##        method applied to the seasonally adjusted data (as before but 
##        with damped=FALSE).
        ukcars.stl.seasadj.forecast.hw <- holt(ukcars.stl.seasadj, damped = FALSE, h = 8)
        autoplot(ukcars.stl.seasadj.forecast.hw)
##     e. Now user ets() to choose a seasonal model for the data.
        ukcars.original.ets.forecast <- forecast(ets(ukcars.original.ts), h=8)
        autoplot(ukcars.original.ets.forecast)
##     f. Compare the RMSE of the ETS model with the RMSE of the models
##        you obtained using STL decompositions. Which gives the better
##        in-sample fits?

        accuracy(ukcars.original.ets.forecast)
        ##ETS RMSE: 25.23244

        accuracy(ukcars.stl.seasadj.forecast.AAN.damped)
        ##AAN Damped, Seasonally Adjusted Data RMSE: 22.78728

        accuracy(ukcars.stl.seasadj.forecast.hw)
        ##Holt-Winter Seasonally Adjusted Data RMSE: 23.295

##      The STL Decomposed, seasonally adjusted, AAN damped forecast gives
##      the best in sample fits.

##     g. Compare the forecasts from the three approaches? Which seems most
##        reasonable?
##      I think the AAN, damped method makes sense with the seasonally adjusted data.
##      Reviewing the STL decomposed data, the seasonal component appears pretty consistent and small.
##      Removing that component from the data helps simplify the model. Applying the damped parameter
##      helps because the observed values have a smaller range in more recent years than what occurred 
##      in more historical data.

##     h. Check the residuals of your preferred model.
       checkresiduals(ukcars.stl.seasadj.forecast.AAN.damped)
##      It appears there is still rooms for improvement on this model.
##      There is some autocorrelation and the residuals are not 
##      normally distributed.