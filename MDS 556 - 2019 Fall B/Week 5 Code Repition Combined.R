## USING BAGGING AND RANDOM FORESTS TO REDUCE TRAINING VARIANCE

## USE BAGGING TO HELP REDUCE OVERFITTING, TRAINING VARIANCE (SAMPLES TAKEN FROM THE SAME 
## POPULATION CAN PRODUCE DECISION TREES WITH DIFFERENT STRUCTURES/PREDICTION ACCURACY),
## AND INCREASE PREDICTION ACCURACY.


###################################################################################
## THE FOLLOWING IS AN EXAMPLE OF A DECISION TREE W/O BAGGING
###################################################################################

setwd("C:/Users/cusey/source/repos/DataScienceCoursework/MDS 556 - 2019 Fall B/")
spamD <- read.table("spamD.tsv.txt", header=T, sep="\t")

## SPLIT INTO TEST/TRAIN
spamTrain <- subset(spamD, spamD$rgroup>=10)
spamTest <- subset(spamD, spamD$rgroup<10)

spamVars <- setdiff(colnames(spamD), list("rgroup","spam"))

## USE ALL THE FEATURES AND DO BINARY CLASSIFICATION WHERE TRUE = SPAM DOCUMENTS
spamFormula <- as.formula(paste('spam=="spam"',
                                paste(spamVars,collapse=" + "),sep=" ~ "))

## CALCULATE THE LOG LIKELIHOOD
loglikelihood <- function(y,py) {
  pysmooth <- ifelse(py==0, 1e-12,
                     ifelse(py==1,1-1e-12,py))
  
  sum(y*log(pysmooth) + (1-y)*log(1-pysmooth))
}

##  CALCULATE AND RETURN THE VARIOUS MEASURES ON THE MODEL
## 1. NORMALIZED THE DEVIANCE
## 2. PREDICTION ACCURACY
## 3. F1 (PRODUCT OF PRECISION AND RECALL)

accuracyMeasures <- function(pred, truth, name="model"){
  ## Normalizethe deviance by th number of data points so we can compare the deviance
  ## across the test and training sets
  dev.norm <- -2*loglikelihood(as.numeric(truth),pred)/length(pred)
  ## Convert the class probability estimator into a classifer 
  ## by labeling documents that score greater than .5 as spam.
  ctable <- table(truth=truth,
                  pred=(pred >.5))
  accuracy <- sum(diag(ctable))/sum(ctable)
  precision <- ctable[2,2]/sum(ctable[,2])
  recall <- ctable[2,2]/ sum(ctable[2,])
  f1 <- precision*recall
  data.frame(model=name, accuracy=accuracy, f1=f1, dev.norm)
}

library(rpart)
## Fit decision tree model using rpart library
treemodel <- rpart(spamFormula, spamTrain)

accuracyMeasures(predict(treemodel, newdata=spamTrain), 
                 spamTrain$spam=="spam",
                 name="tree, training")

##            model  accuracy        f1  dev.norm
## 1 tree, training 0.9104514 0.7809002 0.5618654

accuracyMeasures(predict(treemodel, newdata=spamTest), 
                 spamTest$spam=="spam",
                 name="tree, test")
##       model  accuracy        f1  dev.norm
## 1 tree, test 0.8799127 0.7091151 0.6702857

###################################################################################
## RESULT: THE ACCURACY ON THE TEST SET AND F1 SCORES DEGRADE WHEN APPLYING THE  ##
## DECISION TREE MODEL TO THE TEST SET (AS COMPARE TO THE TRAINING SET).         ##
###################################################################################

#####################################################################################
## NEXT, LET US SEE HOW BAGGING IMPACTS THE ACCURACY MEASURES OF THE DECISION TREE ##
#####################################################################################

## Use bootstrap samples that are the same size as the training set, with 100 trees.
ntrain <- dim(spamTrain)[1] ## returns # of rows in spamTrain (4143)
n <- ntrain
ntree <- 100

## BUILD THE BOOTSTRAP SAMPLES BY SAMPLING THE ROW INDICES OF spamTrain WITH REPLACEMENT
## EACH COLUMN OF THE MATRIX SAMPLES REPRESENT THE ROW INDICES INTO spamTrain THAT COMPRISE
## THE BOOTSTRAP SAMPLE.
samples <- sapply(1:ntree, ## loop 1-100
                  FUN = function(iter){
                    ## obtain 100 samples, with replacement
                    sample(1:ntrain, 
                           size=n, 
                           replace=T)
                  })

## TRAIN THE INDIVIDUAL TREES FOR THE SAMPLES CREATED ABOVE AND RETURN THEM IN A LIST
treelist <- lapply(1:ntree,
                   FUN=function(iter){
                     samp <- samples[,iter];
                     rpart(spamFormula, spamTrain[samp,])
                   })

## Use predict.bag that assumes the underlying classifier to return decidion probabilities
## instead of decisions
predict.bag <- function(treelist, newdata){
  preds <- sapply(1:length(treelist),
                  FUN=function(iter) {
                    predict(treelist[[iter]], newdata=newdata)
                  })
  predsums <- rowSums(preds)
  predsums/length(treelist)
}

accuracyMeasures(predict.bag(treelist, newdata=spamTrain),
                 spamTrain$spam=="spam",
                 name="baggin, training")
##              model  accuracy        f1  dev.norm
## 1 baggin, training 0.9239681 0.8119212 0.4659072

accuracyMeasures(predict.bag(treelist, newdata=spamTest),
                 spamTest$spam=="spam",
                 name="bagging, test")
##          model  accuracy        f1  dev.norm
## 1 bagging, test 0.9039301 0.7598101 0.5261388

###################################################################################
## RESULT: THE ACCURACY ON THE TEST SET AND F1 SCORES IMPROVE DRAMATRICALLY WHEN ##
## COMPARED TO THE MODEL THAT PRODUCED ONLY ONE DECISION TREE. THE MODEL CAN BE  ##
## IMPROVED FURTHER BY APPLYING RANDOM FORESTS.                                  ##
###################################################################################

#####################################################################################
## NEXT, LET US SEE HOW RANDOM FOREST IMPACTS THE ACCURACY MEASURES                ##
#####################################################################################

## BAGGING USES RANDOM DATASETS, BUT SAME FEATURES.
## RESULT = INDIVIDUAL TREES ARE MORE LIKELY CORRELATED WITH EACH OTHER.
## IF ONE TREE TENDS TO MAKE MISTAKES, OTHERS ARE MORE LIKELY TO MAKE MISTAKES THERE ALSO
## RANDOM FOREST SEEKS TO DE-CORRELATE THE TREES BY RANDOMIZING THE SET OF VARIABLES
## ALLOWED TO USES.

## RANDOM FOREST APPLIES THE FOLLOWING STEPS:
## 1. GATHERS BOOTSTRAPPED SAMPLE FROM THE TRAINING DATA
## 2. FOR EACH SAMPLE, PRODUCE A DECISION TREE. AT EACH NODE OF THE TREE:
##   A. RANDOMLY DRAW A SUBSET OF VARIABLES FROM THE TOTAL FEATURES AVAILABLE.
##   B. PICK THE BEST VARIABLE AND SPLIT FROM THE SET OF VARIABLES.
##   C. CONTINUE UNTIL THE TREE IS GROWN

library(randomForest)

set.seed(5123512)

fmodel <- randomForest(x=spamTrain[,spamVars], ## independent variables
                       y=spamTrain$spam, ## dependent variable
                       netree = 100, ## default = 500 but want 100 to compare to bagging example
                       nodesize=7, ## specifies that each node of a tree must have a min of 7 features
                       importance=T) ## tells function to save information for calculating variable importance

accuracyMeasures(predict(fmodel,
                         newdata=spamTrain[,spamVars], type='prob')[,'spam'],
                 spamTrain$spam=="spam", name="random forest, train")

##                 model accuracy        f1  dev.norm
## 1 random forest, train 0.989621 0.9737141 0.1420866

accuracyMeasures(predict(fmodel,
                         newdata=spamTest[,spamVars], type='prob')[,'spam'],
                 spamTest$spam=="spam", name="random forest, test")

##                model  accuracy        f1  dev.norm
## 1 random forest, test 0.9563319 0.8897059 0.3019047

###################################################################################
## RESULT: THE ACCURACY OF THE RANDOM FOREST MODEL WAS MUCH BETTER THAN THE      ##
## SINGLE DECISION TREEY AND BAGGED MODELS. THOUGH IF YOU TAKE THE DIFFERENCE OF ##
## THE ACCURACY MEASURES FOR THE TEST AND TRAIN SET OF EACH MODELS, YOU'LL SEE   ##
## THAT THE BAGGED MODEL MINIMIZED THE GENERALIZED ERROR BETWEEN TEST/TRAIN      ##
## WHILE THE RANDOM FOREST MODEL WAS CLOSER TO THE SINGLE DECISION TREE.         ##
###################################################################################

#####################################################################################
## NEXT, LET US EXAMINE RANDOM FOREST, VARIABLE IMPORTANCE                         ##
#####################################################################################

## VARIABLE IMPORTANCE IS CALCULATED DURING RANDOM FOREST TECHNIQUE
## RANDOM FOREST OBSERVES THE IMPACT A VARIABLE HAS ON THE DECISION
## TREE'S ACCURACY. IF THE IMPACT IS LARGE, THEN THE VARIABLE IS CONSIDERED 
## TO BE IMPORTANT. IN ADDITION, THE DECREASE IN NODE PURITY IS ALSO
## MEASURED WHEN A SPLIT IS PERFORMED ON THE VARIABLE.

varImp <- importance(fmodel)
## importance returns a matrix of importance measures.
## the larger the value, the more important the feature is
varImp[1:10,]

varImpPlot(fmodel, type=1)

## knowing the importance of the features can help in feature reduction.

## lets see what happens if we use only the 25 most important features instead
## of all 57.

## sort variables by their importance, measured by accuracy changed
selVars <- names(sort(varImp[,1], decreasing=T))[1:25]

fsel <- randomForest(x=spamTrain[,selVars],
                     y=spamTrain$spam,
                     ntree=100,
                     nodesize=7,
                     importance=T)

accuracyMeasures(predict(fsel,
                         newdata=spamTrain[,selVars],
                         type='prob')[,'spam'],
                 spamTrain$spam=="spam",
                 name="RF small, train")

##           model  accuracy        f1  dev.norm
##1 RF small, train 0.9862419 0.9651595 0.1500728

accuracyMeasures(predict(fsel,
                         newdata=spamTest[,selVars],
                         type='prob')[,'spam'],
                 spamTest$spam=="spam",
                 name="RF small, test")
##            model  accuracy        f1  dev.norm
## 1 RF small, test 0.9519651 0.8793605 0.3035443

###################################################################################
## RESULT: THE SMALLER RANDOM FOREST MODEL PERFORMED JUST AS WELL AS THE RANDOM  ##
## MODEL PRODUCED WITH 57 FEATURES.
###################################################################################

## GENERALIZED ADDITIVE MODELS TO LEARN NON-MONOTONE RELATIONSHIPS
rm(list = ls())

setwd("C:/Users/cusey/source/repos/DataScienceCoursework/MDS 556 - 2019 Fall B/")
spamD <- read.table("spamD.tsv.txt", header=T, sep="\t")

## LOGISTIC AND LINEAR REGRESSION FAIL TO RECONGIZE NON-MONOTONE VARIABLES.
## THAT IS, IF A NUMERIC VARIABLE HAS A HIGH QUANTITY, IT IS GOOD, BUT IF
## AT SOME POINT MORE ACTUALLY RESEMBLES A NEGATIVE CHARACTERISTIC, THESE
## METHODS DON'T CAPTURE THAT INFORMATION

## EXAMPLE: WEIGHT

## GENERALIZED ADDITIVE MODELS (GAMs) are ways to model non-montone responses
## within linear/logistic models.


set.seed(602957)
x <- rnorm(1000)
noise <- rnorm(1000,sd=1.5)

y <- 3*sin(2*x) + cos(.75*x) - 1.5*(x^2) + noise

select <- runif(1000)
frame <- data.frame(y=y,x=x)

train <- frame[select > .1,]
test <- frame[select <= .1,]

## BUILD A LINEAR MODEL
## sin() and cos() ARE NON LINEAR FUNCTIONS SO THERE SHOULDN'T BE A GOOD LINEAR
## FIT BETWEEN X AND Y

lin.model <- lm(y~x, data = train)
summary(lin.model)

## Call:
##   lm(formula = y ~ x, data = train)

## Residuals:
##  Min      1Q  Median      3Q     Max 
## -17.698  -1.774   0.193   2.499   7.529 

## Coefficients:
##  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  -0.8330     0.1161  -7.175 1.51e-12 ***
##   x             0.7395     0.1197   6.180 9.74e-10 ***
##   ---
##     Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## Residual standard error: 3.485 on 899 degrees of freedom
## Multiple R-squared:  0.04075,	Adjusted R-squared:  0.03968 
## F-statistic: 38.19 on 1 and 899 DF,  p-value: 9.737e-10

## calculate the RMSE
resid.lin <- train$y-predict(lin.model)
sqrt(mean(resid.lin^2))
## [1] 3.481091

## A PLOT OF THE OBSERVED VALUES (Y) AND PREDICTED VALUES OF Y(AS X) WITH THE 
## LINE OF BEST FIT INDICATES A SYSTEMATIC OVER/UNDER PREDICTION OF Y
## THIS INDICATES A HETEROSCEDASTIC CHARACTERISTICS OF THE ERRORS (UNEVEN ERRORS
## OF THE DISTRIBUTION), NOT WHITE NOISE

## USE gam() TO MODEL VARIABLES - INDICATE EITHER LINEAR OR NON LINEAR

library(mgcv)
## BUILD THE MODEL SPECIFYING X SHOULD BE TREATED AS NONLINEAR (BY USING S(X))
## W/O S(X) - THE MODEL IS THE SAME AS LM()
## IF THE COEFFICIENT HAS AN EDF OF ABOUT 1, TRY TI REFIT THE VARIABLE AS A
## LINEAR TERM
glin.model <- gam(y~s(x), data=train)

## THE CONVERGED PARAMETER TELLS US IF THE ALGORITHM CONVERGED, ONLY TRUST OUTPUT
## IF THIS IS TRUE
glin.model$converged
## [1] TRUE

summary(glin.model)

## Family: gaussian 
## Link function: identity 

## Formula:
##   y ~ s(x)

## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -0.83467    0.04852   -17.2   <2e-16 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## Approximate significance of smooth terms:
##   edf Ref.df     F p-value    
## s(x) 8.685  8.972 497.4  <2e-16 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## R-sq.(adj) =  0.832   Deviance explained = 83.4%
## GCV =  2.144  Scale est. = 2.121     n = 901

## The family/link function values advise that the model was treated
## with the same distribution assumptions as a standard linear regression.

## The parametric coefficients are the linear terms. This advises if the linear terms
## where significantly different from 0.

## R-sq.(adj) is the adjusted R-Squared.
## Deviance explain is the raw r-squared.

## calculate teh root mean squared error (RMSE)
resid.glin <- train$y-predict(glin.model)
sqrt(mean(resid.glin^2))
## [1] 1.448514

###################################################################################
## RESULT: THE MODEL THAT USES GAM INSTEAD OF THE FIRST MODEL, LINEAR REGRESSION ##
## DOES A MUCH BETTER JOB PREDICTING Y. IF YOU PLOT THE ACTUAL OBSERVATIONS VS   ##
## THE PREDICTED VALUES, THE RESULTS APPEAR HOMOSCEDASTIC, THERE IS NO SYSTETIC  ##
## OVER OR UNDER PREDICTING Y. THE MEAN OF THE RESIDUALS ARE CLOSE TO 0. OVER    ##
## 80% OF THE VARIANCE IS EXPLAINED AND THE RMSE IS REDUCED BY MORE THAN HALF.   ##
###################################################################################

## GAMs  HAVE A HIGHER RISK OF OVERFITTING SINCE THERE IS MORE FLEXIBILITY IN THE MODEL
## CHECK AGAINST THE TEST SET.

actual <- test$y
pred.lin <- predict(lin.model, newdata=test)
pred.glin <- predict(glin.model,newdata=test)
resid.lin <- actual-pred.lin
resid.glin <-actual-pred.glin

sqrt(mean(resid.lin^2))
## RMSE: 2.792653
sqrt(mean(resid.glin^2))
## RMSE: 1.401399

## COMPARE R-SQUARED
cor(actual,pred.lin)^2
## .1543172
cor(actual, pred.glin)^2
## .7828869

## THE TEST SET PERFORMED COMPERABLE TO THE TRAINING SET SO THERE SHOULDN'T BE
## TOO MUCH OVERFITTING

## this plot indicates what s(x) looks like
plot(glin.model)
## extract sx data points
sx <- predict(glin.model, type="terms")
summary(sx)
xframe <- cbind(train, sx=sx[,1])
ggplot(xframe, aes(x=x)) + geom_point(aes(y=y), alpha =.4) + geom_line(aes(y=sx))

###################################################################################
## APPLY GAM ON REAL DATA                                                        ##
## PREDICT NEWBORN BABY'S WEIGHT (DBWT), CONSIDER MOTHER'S WEIGHT (PWGT),        ##
## MOTHER'S PREGNANCY WEIGHT GAIN (WTGAIN), MOTHER'S AGE (MAGER), # OF PRENATAL  ##
## VISITS (UPREVIS)                                                              ##
###################################################################################
rm(list = ls())

library(mgcv)
library(ggplot2)
load("NatalBirthData.rData")

train <- sdata[sdata$ORIGRANDGROUP <= 5,]
test <- sdata[sdata$ORIGRANDGROUP > 5,]
form.lin <- as.formula("DBWT ~ PWGT + WTGAIN + MAGER + UPREVIS")
linmodel <- lm(form.lin,data=train)
summary(linmodel)

## LINEAR MODEL EXPLAINS ABOUT 7% OF THE VARIANCE
## ALL COEFFICIENTS ARE SIGNIFICANTLY DIFFERENT FROM 0

## BUILD GAM W/ SAME VARIABLES
form.glin <- as.formula("DBWT ~ s(PWGT) + s(WTGAIN) + s(MAGER) + s(UPREVIS)")
glinmodel <- gam(form.glin, data=train)

## verify the model  converged
glinmodel$converged
## [1] TRUE

summary(glinmodel)

## ALL VARIABLES ARE SIGNIFANTLY DIFFERENT THAN 0
## EDFs ARE GREATER THAN 1 SUGGESTING THAT THEY ALL
## HAVE A NONLINEAR RELATIONSHIP WITH BIRTH WEIGHT
## THE % OF VARIANCE EXPLAINED IMPROVED, BUT ONLY TO ABOUT
## 10%

## PLOT GAM RESULTS
## get matrix of s(x) functions
terms <- predict(glinmodel, type="terms")
## combine birthweight, convert to data frame
tframe <- cbind(DBWT = train$DBWT, as.data.frame(terms))

## make column names reference friendly s(PWGT) = sPWGT
colnames(tframe) <- gsub('[()]','', colnames(tframe))

## bind in independent variables before spline
pframe <- cbind(tframe, train[,c("PWGT","WTGAIN","MAGER","UPREVIS")])

p1 <- ggplot(pframe, aes(x=PWGT)) +
  geom_point(aes(y=scale(sPWGT, scale=F))) +
  geom_smooth (aes(y=scal(DBWT, scale=F)))

## CHECK GAM MODEL PERFORMANCE ON HOLD OUT DATA
pred.lin <- predict(linmodel, newdata=test)
pred.glin <- predict(glinmodel, newdata=test)

cor(pred.lin, test$DBWT)^2
## [1] 0.0616812
cor(pred.glin, test$DBWT)^2
## [1] 0.08857426

## the R-Squared for the linera and GAM model are similar for both the test and train
## set, no significant over 

###################################################################################
## APPLY GAM ON A LOGISTIC MODEL                                                 ##
###################################################################################

## basic logistic model
form <- as.formula ("DBWT < 2000 ~ PWGT + WTGAIN + MAGER + UPREVIS")
logmod <- glm(form, data=train, family=binomial(link="logit"))

form2 <- as.formula("DBWT<2000~s(PWGT)+s(WTGAIN)+s(MAGER)+s(UPREVIS)")
glogmod <- gam(form2, data=train, family=binomial(link="logit"))

## check to make sure it congered
glogmod$converged
## [1] TRUE

summary(glogmod)
## summary results indicate there's no proof that PWGT has a significant
## effect on baby's weight
## Adjusted r-squared = .0331

## gam() and randomForest() both have to have formula objects using as.formula()

## KERNALS FOR DATA SEPARATION

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

## SUPPORT MACHINE VECTORS TO COMPLICATED DECISION BOUNDARIES
rm(list = ls())
setwd("C:/Users/cusey/source/repos/DataScienceCoursework/MDS 556 - 2019 Fall B/")

## SVM , WITH A phi() BUILDS A MODEL WHERE X IN THE MACHINE DECIDES WHICH CLASS IT IS IN
## IF:

## W %*% PHI(X) + B >= 0 

## USES A KERNAL TO SEPARATE DATA, DATA THAT MAKE A POSITIVE DOT PRODUCT WITH w IS ON 
## ONE SIDE OF THE HYPERPLANE (BELONGING TO A CERTAIN CLASS), DATA W/ A NEGATIVE DOT
## PRODUCT ARE IN THE OTHER CLASS

## DATA IS SEPARABLE IF U>V AND THE SIZE OF SEPARATION IS DETERMINED BY

## (U-V)/SQRT(W %*% W) = MARGIN

## GOAL IS TO MAXIMIZE THE MARGIN

## EXAMPLE:

##install.packages('kernlab')
library('kernlab')
data('spirals')
## use kernlab's spectral clustering routine to identify the two different
## spirals in the example data set
sc <- specc(spirals, centers=2)
## combine the spiral coordinates and the spiral label into a data frame
s <- data.frame(x=spirals[,1], y=spirals[,2],
                class=as.factor(sc))

library("ggplot2")
## plot the spirals with class labels
ggplot(data=s) +
  geom_text(aes(x=x,y=y,label=class, color=class)) +
  coord_fixed() +
  theme_bw() + 
  theme(legend.position = 'none')


## SVM W/ A POOR CHOICE OF KERNAL
set.seed(2335246L)
s$group <-sample.int(100, size=dim(s)[[1]],replace=T)
sTrain <- subset(s, group >10)
sTest <- subset(s, group <= 10)

## build svm vector model using a vanilladot kernal (not a very good kernal)
mSVMV <- ksvm(class~x+y, data=sTrain, kernal='vanilladot')

## predict the test/hold out data
sTest$predSVMV <- predict (mSVMV, newdata=sTest,type='response')

## plot the predictions on top of a grey copy of all the data so that we can see if
## predictions agree w/ original markings
ggplot() + 
  geom_text(data=sTest, aes(x=x,y=y, label=predSVMV), size=12) +
  geom_text(data=s, aes(x=x,y=y, label=class, color=class), alpha=0.7) +
  coord_fixed() + 
  theme_bw() + theme(legend.position="none")


## SVM W/ A GOOD KERNAL

## use the radial or Gaussian Kernel instead

mSVMG <- ksvm(class~x+y, data=sTrain, kernal='rbfdot')
sTest$predSVMG <- predict(mSVMG, newdata=sTest, type="response")
ggplot() + 
  geom_text(data=sTest, aes(x=x,y=y, label=predSVMG), size=12) +
  geom_text(data=s, aes(x=x, y=y, label=class, color=class), alpha=0.7) +
  coord_fixed() +
  theme_bw() + theme(legend.position='none')

## Much better results with a more appropriate kernel

## SVM on real data
rm(list = ls())
spamD <- read.table('spamD.tsv.txt', header=T, sep='\t')

## SPAM WITH LOGISTIC METHOD MODEL
spamTrain <- subset(spamD, spamD$rgroup>=10)
spamTest <- subset(spamD, spamD$rgroup<10)
spamVars <- setdiff(colnames(spamD), list('rgroup','spam'))
spamFormula <- as.formula(paste('spam=="spam"', paste(spamVars, collapse=' + ')
                                , sep=' ~'))
spamModel <- glm(spamFormula, family=binomial(link='logit'),
                 data=spamTrain)
spamTest$pred <- predict(spamModel, newdata=spamTest, type="response")

## CONFUSION MATRIX
print(with(spamTest, table(y=spam, glPred=pred>=.05)))

## this is different than what is in the book?
## y          FALSE TRUE
## non-spam   182   96
## spam         1  179

library('kernlab')
spamFormulaV <- as.formula(paste('spam',
                                 paste(spamVars, collapse=' + '),
                                 sep=' ~ '))

## rbfdot = radial dot or gaussian kernsl (default)
## C=10, the soft margin penalty. This is high, we prefer not to move
## training examples over getting a wider margin.
## prob.model=T, cross = 5 is asking the model to also product class probabilities
## the class.weights paramater is assigning more weight (more expensive mistake) when
## it incorrectly classifies non-spam as spam.
svmM <- ksvm(spamFormulaV, data=spamTrain,
             kernel = "rbfdot", 
             C=10, 
             prob.model=T, 
             cross=5,
             class.weights=c('spam' = 1, 'non-spam'=10)
)
spamTest$svmPred <- predict(svmM, newdata=spamTest, type="response")
print(with(spamTest, table(y=spam,svmPred=svmPred)))
## y          non-spam spam
## non-spam      271    7
## spam           28  152

## definitely assigned less non-spam as spam

print(svmM)
## to compare this models "apples to apples" (since the SVM model was told to prefer
## training accuracy and margin over model simplicity(param: C=10) & precision over recall
## (class.weights param) look at all the spam candidates (159 per confusion matrix)

sameCut <- sort(spamTest$pred)[length(spamTest$pred) -159]
print(with(spamTest, table(y=spam,glPred=pred>sameCut))) 
