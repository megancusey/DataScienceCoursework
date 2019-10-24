library(fma)
library(fpp2)
##2
## The plastics data set consists of the monthly sales (in thousands)
## of product A for a plastics manufacturer for five years.
## a. Plot the time series of sales of product A. 
##    Can you identify seasonal fluctuations and/or a trend-cycle?
autoplot(plastics)

## The data looks like there is an upward trend but also
## a lot of seasonality. It appears every year there is an
## increase in sales mid-year. The least amount of sales appear to occur
## in the beginning of the new year.

## b. Use a classical multiplicative decomposition to calculate the 
##    trend-cycle and seasonal indices.

plastics %>% decompose(type = "multiplicative") %>%
    autoplot() + xlab("Year") +
    ggtitle("Classical multiplicative decomposition
    of product A ")


decomp_plastics <- decompose(plastics, type = "multiplicative")

## c. Do the results support the graphical interpretation from part a?
## Yes, the classical multiplicative decomposition shows 
## a strong seasonlity and an upward trend as described in a.

## d. Compute and plot the seasonally adjusted data.

autoplot(plastics, series = "Data") +
    autolayer(seasadj(decomp_plastics), series = "Seasonally Adjusted") +
    xlab("Year") + ylab("Monthly Sales")

## e. Change one observation to be an outlier (e.g., add 500 to one 
##    observation), and recompute the seasonally adjusted data.
##    What is the effect of the outlier?

outlier.plastics <- plastics
outlier.plastics[1] <- 1400

decompose_outlier_plastics <- decompose(outlier.plastics, type = "multiplicative")

autoplot(outlier.plastics, series = "Data") +
    autolayer(trendcycle(decompose_outlier_plastics), series = "trend") +
    autolayer(seasadj(decompose_outlier_plastics), series = "seasonally adjusted") +
    xlab("Year") + ylab("Monthly Sales")

## The classical multiplicative decomposition is very sensitive to the outlier.

## f. Does it make any difference if the outlier is near the end 
##    rather than in the middle of the time series?

outlier.middle.plastics <- plastics

outlier.middle.plastics[30] <- 500

decompose.outlier.middle.plastics <- decompose(outlier.middle.plastics, type = "multiplicative")

autoplot(outlier.middle.plastics, series = "Data") +
    autolayer(trendcycle(decompose.outlier.middle.plastics), series = "trend") +
    autolayer(seasadj(decompose.outlier.middle.plastics), series = "seasonally adjusted") +
    xlab("Year") + ylab("Monthly Sales")

outlier.end.plastics <- plastics

outlier.end.plastics[59] <- 2000

decompose.outlier.end.plastics <- decompose(outlier.end.plastics, type = "multiplicative")

autoplot(outlier.end.plastics, series = "Data") +
    autolayer(trendcycle(decompose.outlier.end.plastics), series = "trend") +
    autolayer(seasadj(decompose.outlier.end.plastics), series = "seasonally adjusted") +
    xlab("Year") + ylab("Monthly Sales")

## Regardless of where the outlier is, it appears to have a major effect on the decomposed 
## data.

rm(list = ls())

##4

## a. The civilian labour force had a upward trend across the entire dataset. 
##    The seasonal component is pretty small since the scale ranges from -50 to 50
##    compared to the overall range of the y axis from 7000-9000. It does appear that there
##    is quite a bit of remainder for data between 1991 - 1992 that suggest something might
##    not be accounted for in the data.


## b. There does appear to be a bit of randmonness in 1991/1992
##    that shows a dip in the civilian labor force along with
##    a dip in the seasonally adjusted data.