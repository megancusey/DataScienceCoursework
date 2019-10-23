exercise3.6 <- c(200,300,400,600,1000)

normalize <- function(x) {
  return ((x-min(x))/(max(x)-min(x)))
}
normalize(exercise3.6)

## Returns
## [1] 0.000 0.125 0.250 0.500 1.000

mean <- mean(exercise3.6)
mean
sd <- sd(exercise3.6, na.rm=FALSE)

zscorenorm <- function(x) {
  return ((x-mean)/sd)
}

zscorenorm(exercise3.6)
##RETURNS
##[1] -0.9486833 -0.6324555 -0.3162278  0.3162278
##[5]  1.5811388

library(DescTools)
meanAD <- MeanAD(exercise3.6, FUN=mean, na.rm=FALSE)

zscorenormMAD <- function(x) {
  return ((x-mean)/meanAD)
}

zscorenormMAD(exercise3.6)
##RETURNS [1] -1.2500000 -0.8333333 -0.4166667  0.4166667
##[5]  2.0833333

rm(list = ls()) #clear entire workspace
