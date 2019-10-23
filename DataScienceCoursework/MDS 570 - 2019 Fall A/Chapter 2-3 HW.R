## Chatper 2 Exercise 3

library(fpp2)
## Read Data
data <- read.csv("retail-csv.csv", skip = 1)

## Create time series object
myts <- ts(data[, "A3349873A"],
           frequency = 12, start = c(1982, 4))

## graphing functions
autoplot(myts)
ggseasonplot(myts, year.labels = TRUE, year.labels.left = TRUE)
ggseasonplot(myts, polar = TRUE, year.labels = TRUE, year.labels.left = TRUE)
ggsubseriesplot(myts)

## Chapter 3 Exercise 8
myts.train <- window(myts, end = c(2010, 12))
myts.test <- window(myts, start = 2011)

autoplot(myts) +
    autolayer(myts.train, series = "Training") +
    autolayer(myts.test, series = "Test")

fc <- snaive(myts.train)
accuracy(fc, myts.test)

checkresiduals(fc)