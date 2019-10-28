library(lmtest)

head(ChickEgg)

plot.ts(ChickEgg)

grangertest(chicken ~ egg, order =3, data=ChickEgg)
## P VALUE IS VERY SMALL INDICATING THAT EGGS
## GRANGER CHICKENS AND THE NULL HYPOTHESIS (EGGS
## DO NOT GANGER CHICKENS) CAN BE REJECTED

grangertest(egg ~ chicken, order =3, data=ChickEgg)
## P VALUE IS NOT SMALL INDICATING THAT WE FAIL
## TO REJECT THE NULL HYPOTHESIS (CHICKENS GRANGER
## EGGS)

## CONCLUSION IS THAT EGGS GRANGER CHICKEN

## A GENERIC EXPLAINATION OF GRANGER TEST IS 
## THAT IS CHECKS TO SEE IF LAGGED VALUES OF X
## HELP TO PREDICT Y IN A FORECAST WITH BOTH X AND Y.


## Discussion Post

## The concept of granger causality introduces an interesting 
## concept of attempting to identify a unidirectional causality 
## between two variables. The basis behind the method is to look 
## and see if the lagged values (x) of one variable provides predictive
## power in the other when attempting to forecast both variables (y). 
## If so, the x variable grangers y. If y also grangers x or neither variables 
## granger each other, then the question of "what causes what" remains unanswered.

## The method provides a satisfying answer if a unidirectional relationship
## in the variables are found. However, I believe it's a bit dangerous 
## to rely on the results of the ganger method, knowing what we know about correlation.
## While the chicken and the egg argument kind of makes sense (more eggs = more chickens),
## there could be other reasons that eggs have some sort of correlation with chickens
## within a few lags of each other. For instance, perhaps there was a market shift where 
## more eggs are needed and thus the production of eggs increased and more chickens 
## ( a few lags later b/c they have to grow to produce eggs) were used for harvesting eggs.
## There could be a better variable that predicts the chickens rather than the lags
## of eggs and if you stop looking for other answers after determining the ganger causality, 
## you might be missing a more key component that produces a higher correlation (has a higher
## probability of causality).

rm(list = ls())
