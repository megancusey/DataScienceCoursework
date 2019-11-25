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
