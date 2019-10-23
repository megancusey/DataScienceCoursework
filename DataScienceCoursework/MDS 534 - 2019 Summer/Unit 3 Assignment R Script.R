##load arules
library(arules)

T100 <- c('M','O','N','K','E','Y')
T200 <- c('D','O','N','K','E','Y')
T300 <- c('M','A','K','E')
T400 <- c('M','U','C','K','Y')
T500 <- c('C','O','O','K','I','E')

data <- list(T100,T200,T300,T400,T500)

data2 <-as(data,"transactions")
inspect(data2)

##Convert transactions to transaction ID Lists
tl <- as(data2,"tidLists")

inspect(tl)

summary(tl)


## Pass in the min support and min confidence

rules <- apriori(tl, parameter = 
                   list(supp=.6, conf =.8))

rules <- sort(rules, by="confidence", decreasing=TRUE)

option(digits=2)

inspect(rules[1:5])

## Remove Data List
rm(list=ls())