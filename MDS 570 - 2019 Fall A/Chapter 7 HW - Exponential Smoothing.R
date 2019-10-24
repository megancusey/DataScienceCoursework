library(fma)
library(fpp2)

books.original <- books

## Exercise 5: Data set books contains the daily sales of paperback and hardcover books at the same store.
## The task is to forecast the next four days' sales for paperback and hardcover books.

## a. Plot the series and discuss the main features of the data.
autoplot(books.original) +
    xlab("Day") +
    ylab("Book Sales") +
    ggtitle("Daily Book Sales for Paperback and Hardcover Books")

# Both paperback and hardcover book sales appear to have an upward trend.
# The data represents daily sales, it's not clear which days have the spike
# and which days result in a "valley", but I would guess that there might
# be some seasonal relationship between the spikes in sales. 

## b. Use the ses() function to forecast each series, plot the forecasts.
books.ses.paperback = ses(books.original[, 'Paperback'], h = 5)
books.ses.hardcover = ses(books.original[, 'Hardcover'], h = 5)

autoplot(books.ses.paperback) +
    autolayer(fitted(books.ses.paperback), series = "Paperback Fitted")

autoplot(books.ses.hardcover) +
    autolayer(fitted(books.ses.hardcover), series = "Hardcover Fitted Data")

# SES isn't optimal for data that has trend/seasonality tendenancies.

## c. Compute the RMSE values for the training data in each case.

books.ses.paperback.rsme <- round(accuracy(books.ses.paperback), 2)
## Paperback RSME = 33.64

books.ses.hardcover.rsme <- round(accuracy(books.ses.hardcover), 2)
## Hardcover RSME = 31.93

## Exercise 6:

## a. Now apply Holt's linear method to the paperback and hardback series and compute four-day
##    forecasts in each case.

books.holt.paperback <- holt(books.original[, 'Paperback'], h = 4)
books.holt.hardcover <- holt(books.original[, 'Hardcover'], h = 4)

autoplot(books.original[, 'Paperback']) +
        autolayer(books.holt.paperback, series = "Holt's Method", PI = FALSE)

autoplot(books.original[, 'Hardcover']) +
        autolayer(books.holt.hardcover, series = "Holt's Method", PI = FALSE)

## b. Compare the RMSE measures of Holt's method for the two series to those of simple exponential
##    smoothing in the previous question. (Remember that Holt's method is using on more parameter
##    than SES.) Discuss the merits of the two forecasting methods for these data sets.

books.holt.paperback.rsme <- round(accuracy(books.holt.paperback), 2)
## Paperback RSME = 31.14

books.holt.hardcover.rsme <- round(accuracy(books.holt.hardcover), 2)
## Hardcover RSME = 27.19  

# Holt's method reduces the root squared mean error (RSME)
# when compared the simple exponential smoothing (SES). SES
# is better for data that doesn't have a clear trend or seasonality.
# The books data set certainly has an upward trend which explains why
# Holt's method does a better job at reducing RSME. Holt's method includes
# an algorithm for trend and a trend parameter.   

## c. Compare the forecasts for the two series using both methods. Which do you think is best?
# The SES forecast for both series are flat and doesn't appear
# to reflect the general nature of the data. For instance, the last known data
# point for the Paperback series looks to be close to 250 in sales. 
# The next day forcast is closer to 200, but historically, it doesn't seem like there is ever
# that big of a drop in sales between a day. Since SES isn't optimal for seasonal data,
# the forecast isn't efficient in using this information to make its forecast. Holt's linear method
# only considers trend, instead of seasonlity, but the forecast looks like it makes more sense compared to SES.
# The forecast isn't a straight line and reflects the general trend for the two data series.

## d. Calculate a 95% prediction interval for the first forecast for each series, using the RMSE 
##    values and assuming normal errors. Compare your intervals with those produced using ses 
##    and holt.

s <- sqrt(books.holt.paperback$model$mse)

high <- books.holt.paperback$mean[1] + 1.96 * s
low <- books.holt.paperback$mean[1] - 1.96 * s

books.holt.paperback

c(low = low, high = high)
## LOW: 148.44, HIGH: 270.50)
## HOLT: LOW: 143.91, HIGH: 275.0205
s <- sqrt(books.holt.hardcover$model$mse)

high <- books.holt.hardcover$mean[1] + 1.96 * s
low <- books.holt.hardcover$mean[1] - 1.96 * s

books.holt.hardcover

c(low = low, high = high)
## LOW: 196.87 , HIGH: 303.47

## HOLT, LOW: 192.92, HIGH: 307.425

# It appears using HOLT to calculate a 
# prediction interval results in a broader
# range of values while the other method
# provides a more narrow range of values.