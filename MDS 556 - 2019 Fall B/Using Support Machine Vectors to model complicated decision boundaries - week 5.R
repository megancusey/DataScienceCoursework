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
 