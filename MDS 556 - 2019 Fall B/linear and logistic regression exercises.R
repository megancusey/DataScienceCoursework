## LINEAR REGRESSION EXAMPLE:

## About the data: PUMS data use to predice personal 
## income from other demographic variables such as
## age and education from 2011 US Census PUMS data.

## Secondary goal: What is the effect of having a 
## bachelor's degree on income vs having no degree at
## all?

## STEP 1: DATA PREPARATION
## - Limit the data to full time employees between
## - 20 & 50 years old w/ an income between 1K and 250K
## - Divide into a training and test set.

## Q: Most 20 year olds couldn't have a bachelor's degree
##    yet. Could this skew the results?

## PUMS data is loaded into global environment via rData file.

dtrain <- subset(psub, ORIGRANDGROUP >= 500)
dtest <- subset(psub, ORIGRANDGROUP < 500)

## BUILD THE MODEL
model <- lm(log(PINCP, base=10)~AGEP+SEX+COW+SCHL, data=dtrain)

## CREATE COLUMN W/ PREDICTION VALUES AGAINST TEST SET
dtest$predLogPINCP <- predict(model, newdata=dtest)

## CREATE COLUMN W/ PREDICTION VALUES AGAINST TRAIN SET
dtrain$predLogPINCP <- predict(model, newdata=dtrain)

## REVIEW QUALITY OF MODEL AND MODEL PREDICTIONS

library(ggplot2)

## PLOT 1
## PLOT THE TEST DATA, USE THE TEST PREDICTIONS ON THE X AXIS
## AND THE Y AXIS AS THE KNOWN OBSERVATIONS (LOG(PINCP, BASE=10))
## AES() = ASTHETICS
ggplot(data=dtest, aes(x=predLogPINCP, y=log(PINCP,base=10))) +
  geom_point(alpha = .2, color="black") +
  geom_smooth(aes(x=predLogPINCP, y=log(PINCP, base=10)),color="black") +
  geom_line(aes(x=log(PINCP, base=10), y=log(PINCP, base=10)), color="blue", linetype=2) +
  scale_x_continuous(limits=c(4,5)) + 
  scale_y_continuous(limits=c(3.5,5.5))

## PLOT 2
## PLOT THE RESIDUALS AS A F(X) OF PREDICTED LOG INCOMES
ggplot(data=dtest, aes(x=predLogPINCP, y=predLogPINCP-log(PINCP,base=10))) +
  geom_point(alpha=.2, color="black") +
  geom_smooth(aes(x=predLogPINCP, y=predLogPINCP-log(PINCP, base=10)),color="black")

## Goal is that the points will lie close to the line of prediction
## in this case, the points create a wider cloud.
## This means that the input variables do not explain the
## predicted values very closely.
## However, if the smoothing curve lies along the line prediction, then
## the model overpredicts as much as it underpredicts.

## There are no systematic errors, the smoothing curve does not
## vary from the 0 x axis too much. The error is
## not correlated with y = homoscedastic characteristic is true.

## If there was systematic error, heteroscedastic characteristic = true.

## Check R-Squared values.
## where y = Observed Income & f = the Predicted Income
## GOAL: R-Squared (value ranges from 0-1) to be closer to 1 which would mean that 
## the independent variables account for most/all the variation of the
## dependent variable.

## Limitations of r-squared, the more independent variables in the model,
## the higher r-squared, regardless of the value the independent variable provides.
## Check for adjusted r-squared which penalizes the model for inputs.

rsq <- function(y,f) {1-sum((y-f)^2)/sum((y-mean(y))^2)}

## R-Squared of Training Set
rsq(log(dtrain$PINCP,base=10), predict(model, newdata=dtrain))
## [1] 0.3382568

## R-Squared of Test Sest
rsq(log(dtest$PINCP,base=10),predict(model, newdata=dtest))
## [1] 0.2605496

## R-Squared values indicate a low quality model.
## R-Squared would ideally be above .7

## RMSE 
## The measure of the width of the residuals around
## the line of "perfect" prediction. 
## GOAL: Minimize RMSE
## Measurement of how good explanatory variables are.
##where y = observed dependent/prediction variable, f=the prediction values produced by model.
rmse <- function(y,f) {sqrt(mean((y-f)^2))}

## RMSE of the Training Data Set
rmse(log(dtrain$PINCP, base=10), predict(model, newdata=dtrain))
##[1] 0.2651856
## RMSE of the Test Data Set
rmse(log(dtest$PINCP, base=10), predict(model,newdata=dtest))
##[1] 0.2752171

## What is the value of having a bachelor's degree?
## Coefficients measure the value of a independent variable's
## effect on the dependent variable.
coefficients(model)
## For this model, the intercept is 3.97
## The Coefficient for SCHLBachelor's Degree:
## 0.39383406 

## DISCRETE VARIABLES:
## INTERPRETATION: THE MODEL GIVES A .39 BONUS
## TO LOG INCOME FOR HAVING A BACHELOR'S DEGREE VS
## TO NOT HAVING A HIGH SCHOOL DEGREE.

## THE INCOME RATIO BETWEEN SOMEBODY W/ A BACHELOR'S DEGREE
## AND THE EQUIVALENT PERSON (HOLD ALL OTHER INDEPENDENT VARIABLES CONTSTANT)
## IS LOG(10)*.39 OR 2.45 TIME HIGHER

## SCHLREGULAR HIGH SCHOOL DIPLOMA HAS COEFFICIENT OF .1
## WHICH CAN BE INTREPRETED AS: HAVINGA BACHELOR'S DEGREE ADDS
## .39-.1 UNITS TO PREDICTED LOG INCOME. OR LOG(10) OF (.39-.1) = 1.8X HIGHER

## FOR FUN, HOW MUCH VALUE DOES A MASTERS DEGREE ADD ?
## LOG(10)(.47 - .39) = 1.2X

## CONTINUOUS: AGEP HAS A COEFFICIENT OF .0117.
## EVERY 1 YEAR INC IN AGE = .0117 TO LOG INCOME.
##LOG(10)(.0117) = 1.027X GREATER OR 2.7% INCREASE IF ALL OTHER VARIABLES ARE CONSTANT.

## HOW DO THEY GET FROM RATIO TO %?

## ARE THE COEFFICIENTS RELIABLE?
summary(model)

## Call = how the model was constructed, double check correct parameters were used.
## Call:
## lm(formula = log(PINCP, base = 10) ~ AGEP + SEX + COW + SCHL, 
##   data = dtrain)

## RESIDUALS = summary of residual metrics, goal is to
## minimize the error between the observed values and predicted values
## AKA: Residuals
## GOAL: see median near 0 and symmetry in 1st and 3rd Quartile.
## Residuals:
## Min       1Q   Median       3Q      Max 
## -1.29220 -0.14153  0.02458  0.17632  0.62532 

## Coefficients: Example below, top (3) *
## Coefficients:
##             Estimate Std. Error t-value Pr(>|t})
## (Intercept) 3.973283 0.059343   66.954  < 2e-16
## AGEP        0.011717 0.001352   8.666   < 2e-16
## SEXF       -0.93133  0.023405  -3.979  7.80e-05

## COLUMNS OF THE SUMAMRY OF COEFFICIENTS TABLE:
## ESTIMATE: ESTIMATED COEFFICIENT
## STANDARD ERROR: THE UNCERTAINITY OF TEH STIMATE
## T-VALUE: HOW LARGE TO COEFFICIENT IS RELATIVE TO UNCERTAINITY
## PR(>|T|): HOW LIKELY THE RATIO WOULD BE DUE TO CHANCE

## EXAMPLE: SEXF LEARNED THERE IS A -.093 PENALTY TO LOG(10) OF INCOME, FOR
## A FEMALE INDIVIDUAL. THEREFOR, 1-(LOG(10)*.093) = 1.238 (19% LESS)

## THE P VALUE COLUMN ESTIMATES THE PROBABILITY OF SEEING A COEFFICIENT WITH 
## A VALUE AS LARGE AS THE ESTIMATED VALUE IF THE TRUE COEFFICIENT IS 0 (NULL HYPOTHESIS).
## IF THE P VALUE IS LESS THAN .05, THE EFFECT IS SIGNIFICANT AND THE NULL HYPOTHESIS CAN BE REJECTED.
## IF IT IS GREATER THAN .05, THE COEFFICIENT ISN'T TO  BE TRUSTED.
## ONCE THE P VALUE IS LESS THAN .05, FOCUS ON VARIABLES THAT EXPLAIN THE MOST VARIANCE.

## LAST PORTION OF SUMMARY(MODEL) = OVERALL QUALITY
## DEGREES OF FREEDOM = # OF DATA ROWS MINUT # OF COEFFICIENTS FIT
## RESIDUAL STANDARD ERROR = SUM OF THE SQUARE OF RESIDUALS DIVIDED BY THE DEGREES OF FREEDOM.
## MULTIPLE R-SQUARED = R-SQUARED
## ADJUSTED R-SQUARED = R-SQUARED PENALIZED BY DEGREES OF FREEDOM TO # OF TRAINING EXAMPLES.
## F-STATISTIC = MEASURES IF THE LINEAR REGRESSION MODEL PREDICTS OUTCOME BETTER THAN THE CONSTANT MODE (MEAN OF Y)
##  CHECKS IF 2 VARIANCES (THE RESIDUALS FROM CONSTANT MODEL AND VARIANCE OF THE RESIDUALS FROM LINEAR REGRESSION MODEL)
##  ARE SIGNIFICANTLY DIFFERENT. IF P VALUE IS LESS THAN .05, REJECT THE NULL HYPOTHESIS THAT THE CONSTANT MODEL AND THE 
##  LINEAR REGRESSION MODEL ARE NOT SIGIGNIFICANTLY DIFFERENT)

##  Residual standard error: 0.2691 on 578 degrees of freedom
##  Multiple R-squared:  0.3383,	Adjusted R-squared:  0.3199 
##  F-statistic: 18.47 on 16 and 578 DF,  p-value: < 2.2e-16

rm(list = ls())

## LOGISTIC REGRESSION EXAMPLE:

## LOGISTIC VS LINEAR REGRESSION:
## LOGISTIC - CLASSIFICATION PREDICTION
## PREDICTS VALUES BETWEEN 0 and 1 (binary classification or Probability)
## BOTH CAN USE COEFFICIENTS TO PROVIDE ADVICE
## LOGISTIC REGRESSION IS THE LINEAR REGRESSION THAT FINDS THE LOG-ODDS OF
## THE PROBABILITY OF INTEREST.

## Loaded data via .Rdata file
## ABOUT THE DATA: PROVIDE CLASSIFICATION IF A
## PREGNANCY WILL BE HIGH RISK. ONLY USE VARIABLES
## THAT ARE AVAILABLE BEFORE OR DURING LABOR. THE OUTCOME
## WILL BE USED TO DETERMINE IF EMERGENCY EQUIPMENT SHOULD BE
## DELIVERED TO THE ROOM DURING BIRTH FOR AFTER LABOR CARE.

## Subset into test/train
train <- sdata[sdata$ORIGRANDGROUP <=5,]
test <- sdata[sdata$ORIGRANDGROUP >5,]

## BUILD MODEL
complications <- c("ULD_MECO", "ULD_PRECIP","ULD_BREECH")
riskfactors <- c("URF_DIAB","URF_CHYPER","URF_PHYPER","URF_ECLAM")

y <- "atRisk"
x <- c("PWGT", "UPREVIS","CIG_REC","GESTREC3","DPLURAL", complications, riskfactors)

## paste() concatencate after converting to character
fmla <- paste(y, paste(x, collapse = "+"), sep="~")

## FIT LOGISTIC REGRESSION MODEL
print(fmla)
## [1] "atRisk~PWGT+UPREVIS+CIG_REC+GESTREC3+DPLURAL+
## ULD_MECO+ULD_PRECIP+ULD_BREECH+URF_DIAB+URF_CHYPER+
## URF_PHYPER+URF_ECLAM"

## similar to lm() except the family parameter which indicates  the 
## distribute of dependent variable = y. The link function olinkes the output of
## to a linear model. ## this makes it a logistic model, otherwise it would produce a 
## standard linear model.
model <- glm(fmla, data=train, family=binomial(link="logit"))

## MAKE PREDICTIONS
## without the response parameter, the prediction would be the output of the link function, logit(y) 
## instead of the predicted probabilities of y.
train$pred <- predict(model, newdata=train, type="response")
test$pred <- predict(model, newdata=test, type="response")

## CHARACTERIZING PREDICTION QUALITY
## PLOT 1 - plot the distribution of scores for positive and negative instances.
library(ggplot2)
ggplot(train, aes(x=pred, color=atRisk, linetype=atRisk)) +
  geom_density()

## In general, we want the prediction for the TRUE to have a higher probability rate
## (probability on the right) and FALSE predictions with low probability (predictions on the
## left).

## Both TRUE and FALSE predictions are concentrated on the left, indicating the probability
## of a true case scores low.

## The scores for negativies instances dies down quicker than positive instances.
## To use as a classifier, a threshold must be set in order to identify a class as positive or negative.
## Picking a threshold means you are trying to balance the precision of the classifier and the recall.

## PRECISION =  FRACTION OF THE PREDICTED POSITIVES = TRUE
## RECALL = HOW MANY OF THE TRE POSITIVES THE CLASSIFIER FINDS.

## IF THE SCORE DISTRIBUTIONS OF POSITIVE AND NEGATIVE INSTANCES ARE WELL SEPARATED (IN THIS EXAMPLE, IT'S NOT)
## THEN THE THRESHOLD IS IN THE "VALLEY" BETWEEN THE TWO PEAKS.

## SINCE THE TWO DISTRIBUTIONS AREN'T SEPARATED, THE MODEL CAN'T BUILD A CLASSIFIER WITH GOOD
## PRECISION AND RECALL. THE CLASSIFIER CAN IDENTIFY A SUBSET OF SITUATIONS THAT HAVE A HIGHER THAN AVERAGE
## RATE OF AT-RISK BIRTH.

## ENRICHMENT RATE: RATIO OF CLASSIFIER PRECISION TO AVERAGE RATE OF POSITIVES.

## USE TRAINING SET TO PICK A THRESHOLD
## PLOT 2: PLOT ENRICHMENT AND RECALL AS A FUNCTION OF THRESHOLD
## HIGHER THRESHOLD = MORE PRECISE CLASSIFICATION AT THE COST OF MISSING MORE CASES
## LOWER THRESHOLD = THE COST OF MORE FALSE POSITIVES.

## BEST TRADE-OFF BETWEEN PRECISION AND RECALL IS A FUNCTION OF HOW MANY RESOURCES THE HOSPITAL
## HAS AVAILABLE TO ALLOCATE.

##install.packages("ROCR")
##install.packages("grid")

## LOAD ROCR LIBRARY
library(ROCR)
library(grid)
library(ggplot2)
## CREATE ROCR OBJECT TO CALCULATE PRECISION AS A
## FUNCTION OF THRESHOLD

## ROCR PREDICTION OBJECTS
predObj <- prediction(train$pred,train$atRisk)
precObj <- performance(predObj, measure="prec")

## ROCR OBJECT TO CALC RECALL AS A F(X) OF THRESHOLD
recObj <- performance(predObj, measure="rec")

## ROCR OBJECTS ARE WHAT R CALLS S4 OBJECTS, EXTRACT
## SLOTS FROM OBJECTS USING @
precision <-(precObj@y.values)[[1]]
prec.x <-(precObj@x.values)[[1]]
recall <-(recObj@y.values)[[1]]

## BUILD A DATAFRAME W/ THRESHOLDS, PRECISION, AND RECALL
rocFrame <- data.frame(threshold=prec.x, precision=precision, recall=recall)

## THIS FUNCTION PLOTS STACKED PLOTS ON A SINGLE PAGE
nplot <- function(plist){
  n<- length(plist)
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(n,1)))
  vplayout=function(x,y) {viewport(layout.pos.row = x, layout.pos.col = y)}
  for(i in 1:n) {
    print(plist[[i]], vp=vplayout(i,1))
  }
}

## CALC RATE OF AT-RISK BIRTHS IN THE TRAIN SET
pnull <- mean(as.numeric(train$atRisk))

## PLOT ENRICHMENT RATE AS A F(X) OF THRESHOLD
p1 <-ggplot(rocFrame, aes(x=threshold)) +
  geom_line(aes(y=precision/pnull)) +
  coord_cartesian(xlim = c(0,0.05), ylim = c(0,10))

## plot recall as F(X) of Threshold
p2 <- ggplot(rocFrame, aes(x=threshold)) +
  geom_line(aes(y=recall)) + 
  coord_cartesian(xlim=c(0,0.05))

## show both plots
nplot(list(p1,p2))

## Pick threshold based on off plot, say threshold = .02
## Evaluate Model

## create confusion matrix
ctab.test <- table(pred=test$pred > .02, atRisk=test$atRisk)
ctab.test

precision <- ctab.test[2,2]/sum(ctab.test[2,])
precision
## [1] 0.04601349

recall <-ctab.test[2,2]/sum(ctab.test[,2])
recall
## [1] 0.5550239

enrich <- precision/mean(as.numeric(test$atRisk))
enrich
## [1] 2.664159

## RESULTS: LOW-CLASSIFIER, BUT IDENTIFIES 55.5% OF TRUE POSITIVES

## FINDING RELATIONS AND EXTRACTING ADVICE:
coefficients(model)
summary(model)

## CALCULATING DEVIANCE RESIDUALS
## CREATE VECTOR OF PREDICTIONS FOR TRAINING DATA
pred <- predict(model, newdata=train, type="response")

## CREATE A FUNCTION TO RETURN THE LOG LIKELIHOODS FOR EACH DATA POINT.
## Y = TRUE OUTCOME, PY = PREDICTED PROBABILITY

llcomponents <- function(y, py) {
  y*log(py) + (1-y) * log(1-py)
}

edev <- sign(as.numeric(train$atRisk) - pred) *
        sqrt(-2*llcomponents(as.numeric(train$atRisk),pred))

summary(edev)

## CREATE A FUNCTION TO CALC THE LOG LIKELIHOOK OF A DATASET.
## Y = OUTCOME IN NUMERIC FORM, PY PREDICTED PROBABILITY THAT Y =1.
loglikelihood <- function(y,py) {
  sum(y*log(py) + (1-y)*log(1-py))
}

pnull <- mean(as.numeric(train$atRisk))
null.dev <- 2*loglikelihood(as.numeric(train$atRisk),pnull)
pnull
## [1] 0.01920912
null.dev
## [1] -2698.716 why did I get negative here?
model$null.deviance
## [1] 2698.716
pred <- predict(model, newdata=train, type="response")
## Calc deviance of model for training data
resid.dev <- 2*loglikelihood(as.numeric(train$atRisk),pred)
resid.dev
##[1] -2462.992, negative again? but loglikihood function matches text?
model$deviance
##[1] 2462.992

## calc null deviances and residual deviance for test data
testy <- as.numeric(test$atRisk)
testpred<-predict(model,newdata=test,type="response")

pnull.test <-mean(testy)
null.dev.test <- -2*loglikelihood(testy, pnull.test)
resid.dev.test <- -2*loglikelihood(testy,testpred)

pnull.test
## [1] 0.0172713
null.dev.test
## [1] 2110.91
resid.dev.test
## 1947.094
