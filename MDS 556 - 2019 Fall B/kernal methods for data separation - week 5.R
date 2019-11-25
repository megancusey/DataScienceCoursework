rm(list = ls())
setwd("C:/Users/cusey/source/repos/DataScienceCoursework/MDS 556 - 2019 Fall B/")

## KERNAL FUNCTIONS:  A function k() that maps pairs (u,v) to numbers when there
## is a phi() mapping (u,v)s to a vector space where k(u,v)= phi(u) %*% phi(v)
## for all u,v.

## %*% is R's notation for dot product or inner product

## EXAMPLE:
u <- c(1,2)
v <- c(3,4)

k <- function(u,v) { ## dot product function of two vector
   u[1]*v[1] + u[2]*v[2] +
    u[1]*u[1]*v[1]*v[1] + u[2]*u[2]*v[2]*v[2] + 
    u[1]*u[2]*v[1]*v[2]
}

## function of a single vector variable that returns a vector containing the original 
## entries plus all products of entries.
phi <- function(x){
  x <- as.numeric(x) 
  c(x,x*x,combn(x,2,FUN=prod))
}

## evaluation of k(,)
print(k(u,v))
## [1] 108
print(phi(u))
# [1] 1 2 1 4 2
print(phi(v))
## [1] 3 4 9 16 12
print(as.numeric(phi(u) %*% phi(v)))
## 108

## phi() = k(,) indicating k(,) as a kernal = transforms to a linear operator so that
## the data can be linearly separated

## APPLY KERNAL TO REAL DATA

load("psub.RData")

dtrain <- subset(psub, ORIGRANDGROUP >= 500)
dtest <- subset(psub, ORIGRANDGROUP < 500)

## BUILD THE BASIC LINEAR REGRESSION MODEL
## Perform Stepwise imporvment - removes variables that do not have a significant
## impact
m1 <- step (
    lm(log(PINCP,base=10)~AGEP+SEX+COW+SCHL, data=dtrain),
    direction='both'
)

rmse <- function(y,f) {
  sqrt(mean((y-f)^2))
}

print(rmse(log(dtest$PINCP, base=10), predict(m1, newdata=dtest)))
## [1] .2752171

## RESULT: RMSE ISN'T THAT LOW EVEN THOUGH THE MODEL SELECTED THE MOST
## SIGNIFICANT VARIABLES

## NEXT STEP: USE AN EXPLICIT KERNAL TRANSFORMATION TO BUILD NEW MODEL VARIABLES
## assign phi again

## Defined our primal kernal function: map a vector to a copy of itself plus
## all squared terms and cross-multiplied terms
phi <- function(x){
  x <- as.numeric(x) 
  c(x,x*x,combn(x,2,FUN=prod))
}

## Define a function similar to our primal kernel, but work on variable names instead
## of values
phiNames <- function(n) {
  c(n,paste(n,n,sep=':'),
    combn(n,2,FUN=function(x) {
      paste(x,collapse=':')
    }))
}

## Convert data to a matrix where all categorical variables are encoded as multiple
## numeric indicators
modelMatrix <- model.matrix(~ 0 + AGEP + SEX + COW+ SCHL, psub)

## Remove problematic characters from matrix column names
colnames(modelMatrix) <- gsub('[^a-zA-Z0-9]+','_',
                              colnames(modelMatrix))

## Apply the primal kernl function to every row of the matrix and transpose results
## so that they're written as row (insted of a list returned by apply())
pM <- t(apply(modelMatrix,1,phi))

vars <- phiNames(colnames(modelMatrix))
vars <- gsub('[^a-zA-Z0-9]+','_',vars)

colnames(pM) <- vars

## Extend names from original matrix to names for compound variables in new matrix
pM<- as.data.frame(pM)

## Add in target variable and test/train split columns to prep for data modeling.
pM$PINCP <- psub$PINCP
pM$ORIGRANDGROUP <- psub$ORIGRANDGROUP
pMtrain <- subset(pM, ORIGRANDGROUP >= 500)
pMtest <- subset(pM, ORIGRANDGROUP < 500)

formulaStr2 <- paste('log(PINCP,base=10)', 
                paste(vars, collapse=' + '),
                sep=' ~ ')

m2 <- lm(as.formula(formulaStr2), data=pMtrain)
coef2 <- summary(m2)$coefficients
interestingVars <- setdiff(rownames(coef2)[coef2[,'Pr(>|t|)']<0.01],
                           '(Intercept)')

## SELECT A SET OF INTERESTING VARIABLES BY BUILDING INITIAL MODEL USING ALL
## THE NEW VARIABLES AND RETAINING AN INTERESTING SUBSET. THIS IS A MANUAL
## PROCESS FOR THE PUROPSE OF QUICKLYING REMOVING USELESS DERVIVED VARIABLES TO
## REDUCE DEGREES OF FREEDOM & OVERFITTING

interestingVars <- union(colnames(modelMatrix), interestingVars)

formulaStr3 <- paste('log(PINCP, base=10)',
                paste(interestingVars, collapse = ' + '),
                sep=' ~ ')

##stepwise regress to get new subset of variables
m3 <- step(lm(as.formula(formulaStr3), data=pMtrain), direction='both')

## Calculate RMSE
print(rmse(log(pMtest$PINCP,base=10), predict(m3, newdata=pMtest)))
## [1] .2735955

## little improvement on RMSE

## Summary on the new variables phi() produced
print(summary(m3))
## THE ONLY VARIABLE PHI PRODUCED WAS AGEP_AGEP (AGEP*AGEP) TO BUILD A 
## NON-MONOTONE RELATION BETWEEN AGE AND LOG INCOME.