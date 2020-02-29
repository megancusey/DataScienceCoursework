####################################################################
# Megan Cusey
# MDS 564 - Advanced Data Mining and Analytics
# Date: 02/05/2020
# Week 1 - Test Case: Caravan Insurance Customer Profile Modeling
# Assignment: Read Chapter 7 from Data Mining Applications with R.
# Run code provided and make suggestions to improve the case study.
# Follow CRISP Method.
# 1. Business Understanding
# 2. Data Understanding
# 3. Data Preparation
# 4. Modeling
# 5. Evaluation
# 6. Deployment
####################################################################


# 1. Business Understanding:
# The study frame's its problem around trying to identify potential caravan insurance customers by understanding 
# existing caravan policy holders focusing a marketing strategy for individuals that could be existing customers
# that do not hold a caravan policy or new customers with similiar characteristics.
#
# The nature of the problem leads me to a few assumptions:
# 1. All potential caravan insurance carriers have a need for the policy because either he/she owns a caravan or
#    needs to purchase insurance on behalf of somebody that does.
# 2. Current owners of caravans carry insurance because they have to as a condition of financing, the cost of the 
#    caravan is worth insuring, the caravan serves as a primary residence, he/she can afford to insurance it
#    or a combination of these factors.
# 3. Current owners of caravans do not carry insurance because they are no longer financing or didn't have to finance
#    the caravan, its not worth insuring, they can't afford to insurance it, or a combination of these factors.
# 4. Most owners that want insurance probably already have it, but with a different carrier.

# I would validate some of these assumptions by figuring out the date of purchase for existing caravan insurance policies
# with the initial policy start date (assuming they didn't switch carriers since the caravan purchase).

# I would focus my efforts on defining who are the group(s) of people who are most likely to purchase caravans in
# the near future.


# 2. Data Understanding: 
# Data used in this study describes existing customers that may or may not already have caravan insurance policies.
#

## Set Working Directory
setwd("C:/Users/cusey/source/repos/DataScienceCoursework/MDS 564 - 2020 Spring/")
getwd()

## Load Data
train_data <- read.table(file="Caravan-Train-Set.txt",header=TRUE,sep="\t",na.strings="*",stringsAsFactors=FALSE)
test_data <- read.table(file="Caravan-Test-Set.txt",header=TRUE,sep="\t",na.strings="*",stringsAsFactors=FALSE)

## First 10 rows
head(train_data,10)

## Explore Data
library(Hmisc)

summary(train_data)
str(train_data)
describe(train_data)

correl1a <- as.data.frame(cor(train_data[1:85],train_data[86],method = "spearman"))
correl1b <- cbind(rownames(correl1a),correl1a)
correl1 <- correl1b[order(- abs((correl1b$CARAVAN))), ]
correl2a <- as.data.frame(cor(train_data[1:85],train_data[86],method = "pearson"))
correl2b <- cbind(rownames(correl2a),correl2a)
correl2 <- correl2b[order(- abs((correl2b$CARAVAN))), ]
correl1
correl2
plot(correl1)
plot(correl2)

numeric_variables <- which(sapply(train_data, is.numeric)) #index vector numeric variables

library(ggplot2)
library(purrr)
library(tidyr)

##install.packages("knitr")
library(knitr)
library(ggplot2)
library(plyr)
library(dplyr)
##install.packages("corrplot")
library(corrplot)
library(caret)
library(gridExtra)
library(scales)
library(Rmisc)
library(ggrepel)
##library(randomForest)
library(psych)
##library(xgboost)

## Review distribution of variables - first 10
train_data[numeric_variables[0:10]] %>%
  keep(is.numeric) %>% 
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales="free") +
  geom_histogram() + 
  ylab("Frequency") + 
  xlab("Features") +
  ggtitle("Distribution")

## Review distribution of variables - first 11
train_data[numeric_variables[11:20]] %>%
  keep(is.numeric) %>% 
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales="free") +
  geom_histogram() + 
  ylab("Frequency") + 
  xlab("Features") +
  ggtitle("Distribution")

## Review distribution of variables - first 11
train_data[numeric_variables[21:30]] %>%
  keep(is.numeric) %>% 
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales="free") +
  geom_histogram() + 
  ylab("Frequency") + 
  xlab("Features") +
  ggtitle("Distribution")

train_data[numeric_variables[31:40]] %>%
  keep(is.numeric) %>% 
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales="free") +
  geom_histogram() + 
  ylab("Frequency") + 
  xlab("Features") +
  ggtitle("Distribution")

train_data[numeric_variables[41:50]] %>%
  keep(is.numeric) %>% 
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales="free") +
  geom_histogram() + 
  ylab("Frequency") + 
  xlab("Features") +
  ggtitle("Distribution")

train_data[numeric_variables[51:60]] %>%
  keep(is.numeric) %>% 
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales="free") +
  geom_histogram() + 
  ylab("Frequency") + 
  xlab("Features") +
  ggtitle("Distribution")

train_data[numeric_variables[61:70]] %>%
  keep(is.numeric) %>% 
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales="free") +
  geom_histogram() + 
  ylab("Frequency") + 
  xlab("Features") +
  ggtitle("Distribution")

train_data[numeric_variables[71:80]] %>%
  keep(is.numeric) %>% 
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales="free") +
  geom_histogram() + 
  ylab("Frequency") + 
  xlab("Features") +
  ggtitle("Distribution")

train_data[numeric_variables[81:86]] %>%
  keep(is.numeric) %>% 
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales="free") +
  geom_histogram() + 
  ylab("Frequency") + 
  xlab("Features") +
  ggtitle("Distribution")

##names(numeric_variables)

cor_numVar <- cor(train_data[,numeric_variables], use="pairwise.complete.obs") #correlations of all numeric variables
cor_sorted <- as.matrix(sort(cor_numVar[,'CARAVAN'], decreasing = TRUE))
#select only high corelations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.1)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")

as.matrix(sort(cor(train_data[1:85], train_data[86],method="pearson"),decreasing=TRUE))

cor(train_data[,grep("PPERSAUT",colnames(train_data))], train_data[,grep("APERSAUT",colnames(train_data))],method="pearson")

data_caravan <- subset(train_data,train_data$CARAVAN == 1)

hist(data_caravan$APERSONG) %>%
text(labels=counts)

## 3. Data Preparation:

clean_data_pipeline <- function(data) {
  #The following makes four variables explicitly categorical 
  data$MOSTYPE <- as.factor(data$MOSTYPE) ## Customer Subtype, 41 Levels
  data$MGEMLEEF <- as.factor(data$MGEMLEEF) ## Average Age, 6 Levels
  data$MOSHOOFD <- as.factor(data$MOSHOOFD) ## Customer Main Type, 10 Levels
  data$PWAPART <- as.factor(data$PWAPART) ## Contribution 3rd Party Insur. 9 Levels
  
  ### removing religious variables from dataset
  data <- data[ ,-c(6:9)] 
  
  
  ## Removing the following features for having little variation
  ## in the distribution of data
  data<- data[,-c(grep("ABYSTAND",colnames(data)),
                             grep("APLEZIER",colnames(data)),
                             grep("AFIETS",colnames(data)),
                             grep("AZEILPL",colnames(data)),
                             grep("AINBOED",colnames(data)),
                             grep("AAANHANG",colnames(data)),
                             grep("ALEVEN",colnames(data)),
                             grep("AWAOREG",colnames(data)),
                             grep("APERSONG",colnames(data)),
                             grep("AWERKT",colnames(data)),
                             grep("AGEZONG",colnames(data)),
                             grep("AVRAAUT",colnames(data)),
                             grep("ABESAUT",colnames(data)),
                             grep("AWALAND",colnames(data)),
                             grep("PINBOED",colnames(data)),
                             grep("AMOTSCO",colnames(data)),
                             grep("PPLEZIER",colnames(data)),
                             grep("PBYSTAND",colnames(data)),
                             grep("AWABEDER",colnames(data)),
                             grep("PFIETS",colnames(data)),
                             grep("PAANHANG",colnames(data)),
                             grep("PLEVEN",colnames(data)),
                             grep("PWERKT",colnames(data)),
                             grep("PPERSONG",colnames(data)),
                             grep("PZEILPL",colnames(data)),
                             grep("PBROM",colnames(data)),
                             grep("PTRACTOR",colnames(data)),
                             grep("PGEZONG",colnames(data)),
                             grep("PWAOREG",colnames(data)),
                             grep("PMOTSCO",colnames(data)),
                             grep("PWALAND",colnames(data)),
                             grep("PVRAAUT",colnames(data)),
                             grep("PBESAUT",colnames(data)),
                             grep("PWABEDR",colnames(data))
  )]

  return(data)
}

train_data <- clean_data_pipeline(train_data)

prop.table(table(train_data$MOSTYPE,train_data$CARAVAN),1)

cust.logit <- glm(CARAVAN~. ,data = train_data, family=binomial(link="logit"))
summary(cust.logit)

## 4. Modeling: 

## RP model
library(ROCR)
library(rpart)
library(rpart.plot)
library(caret)
cust.rp<- rpart(CARAVAN~ ., data=train_data)
cust.rp

rpart.plot(cust.rp)

cust.rp$cptable

printcp(cust.rp)


cust.rp$variable.importance
cust4.var.imp<-varImp(cust.rp, UseModel=rpart)
hist(cust4.var.imp$Overall)

cust4a <- cbind(rownames(cust4.var.imp),cust4.var.imp)
cust4 <- cust4a[order(-cust4a$Overall), ]
cust4

test_data <- clean_data_pipeline(test_data)

cust.rp.predict<-predict(cust.rp, type = "matrix", newdata = test_data)
cust.rp.prob.rocr<-prediction(cust.rp.predict,test_data$CARAVAN)
cust.rp.perf<-performance(cust.rp.prob.rocr,"tpr","fpr")
plot(cust.rp.perf,main="ROC curve on RPart",colorize=T)

##Bagging Model###

library(ipred)
cust.ip<-bagging(CARAVAN ~ ., data=train_data, coob=TRUE)
cust.ip.prob<-predict(cust.ip, type="prob", newdata = test_data)
custip4.var.imp<-varImp(cust.ip)
hist(custip4.var.imp$Overall)

custip4a <- cbind(rownames(custip4.var.imp),custip4.var.imp)
custip4 <-custip4a[order(-custip4a$Overall), ]
custip4

cust.ip.prob.rocr<-prediction(cust.ip.prob, test_data$CARAVAN)
cust.ip.perf<-performance(cust.ip.prob.rocr,"tpr","fpr")
plot(cust.ip.perf,main="ROC curve on Bagging",colorize=T)
## SVM ###
library(e1071)
cust.svm <-svm(CARAVAN ~ ., data=train_data, method="C-classification", kernel="radial", cost=10, gamma=0.1, cross=0, fitted=TRUE,probability=TRUE)
cust.svm.feature.weights = t(cust.svm$coefs) %*% cust.svm$SV
cust.svm.feature.weights
custsvmt <- data.frame(t(cust.svm.feature.weights))
custsvma <- cbind(rownames(custsvmt),custsvmt)
custsvm <- custsvma[order(-abs(custsvma$t.cust.svm.feature.weights.)),]
custsvm
hist(cust.svm.feature.weights)

cust.svm.prob<-predict(cust.svm, type="prob", newdata = test_data, probability=TRUE)
cust.svm.prob.rocr<-prediction(cust.svm.prob, test_data$CARAVAN)
cust.svm.perf<-performance(cust.svm.prob.rocr,"tpr","fpr")
plot(cust.svm.perf,main="ROC curve on SVM",colorize=T)
### LR Classification ###
cust.logit<-glm(CARAVAN ~ ., data=train_data, family = binomial(link="logit"))
summary(cust.logit)

custlogit.var.imp<-varImp(cust.logit,useModel=glm)
custlogita <- cbind(rownames(custlogit.var.imp),custlogit.var.imp)
custlogit <- custlogita[order(-custlogita$Overall), ]
custlogit

cust.logit.prob<-predict(cust.logit, type="response", newdata = test_data)
cust.logit.prob.rocr<-prediction(cust.logit.prob, test_data$CARAVAN)
cust.logit.perf<-performance(cust.logit.prob.rocr,"tpr","fpr")
plot(cust.logit.perf,main="ROC curve on LR",colorize=T)
### plotting ROC #####
ppi <- 300
png(filename="ROC curve without religion variables.png", width=6*ppi,height=6*ppi,res=ppi)
plot(cust.rp.perf,col=2,main="ROC curve without religion variables")
legend(0.5,0.5,c('rpart','bagging','svm','logitisc'),2:5)
plot(cust.ip.perf,col=3,add=TRUE)
plot(cust.svm.perf,col=4,add=TRUE)
plot(cust.logit.perf,col=5,add=TRUE)
dev.off()

### plotting Recall ####
cust.rp.perf.cr<-performance(cust.rp.prob.rocr,"rec","rpp")
cust.ip.perf.cr<-performance(cust.ip.prob.rocr,"rec","rpp")
cust.svm.perf.cr<-performance(cust.svm.prob.rocr,"rec","rpp")
cust.logit.perf.cr<-performance(cust.logit.prob.rocr,"rec","rpp")
ppi <- 300
png(filename="Cummulative curve without religion variables.png", width=6*ppi,height=6*ppi,res=ppi)
plot(cust.rp.perf.cr,col=2,main="Cummulative curve without religion variables")
legend(0.5,0.5,c('rpart','bagging','svm','logitisc'),2:5)
plot(cust.ip.perf.cr,col=3,add=TRUE)
plot(cust.svm.perf.cr,col=4,add=TRUE)
plot(cust.logit.perf.cr,col=5,add=TRUE)
dev.off()
#### plotting accuracy of prediction
cust.rp.perf.acc<-performance(cust.rp.prob.rocr,"acc")
cust.ip.perf.acc<-performance(cust.ip.prob.rocr,"acc")
cust.svm.perf.acc<-performance(cust.svm.prob.rocr,"acc")
cust.logit.perf.acc<-performance(cust.logit.prob.rocr,"acc")
ppi <- 300
png(filename="Accuracy vesus Cut-off without religion variables.png", width=6*ppi,height=6*ppi,res=ppi)
plot(cust.rp.perf.acc,col=2,main="Accuracy vesus Cut-off without religion variables")
legend(0.5,0.5,c('rpart','bagging','svm','logitisc'),2:5)
plot(cust.ip.perf.acc,col=3,add=TRUE)
plot(cust.svm.perf.acc,col=4,add=TRUE)
plot(cust.logit.perf.acc,col=5,add=TRUE)
dev.off()

time.rp <- system.time(
  {cust.rp <- rpart(CARAVAN~. , data=train_data)
  cust.rp.pred <- predict(cust.rp,type="matrix",newdata=test_data)
  cust.rp.prob.rocr <- prediction(cust.rp.pred, test_data$CARAVAN)})
time.rp

## 5. Evaluation
# Cross Validation - RP
control <-rpart.control(xval=5)
cust.rp2 <- rpart(CARAVAN~., data=train_data, control = rpart.control(xval = 5)) 
cust.rp2

rpart.plot(cust.rp2)
cust.rp2$cptable
printcp(cust.rp2)
cust.rp2$variable.importance
cust42.var.imp<-varImp(cust.rp2, UseModel=rpart)
hist(cust42.var.imp$Overall)

cust4a2 <- cbind(rownames(cust42.var.imp),cust42.var.imp)
cust42 <- cust4a2[order(-cust4a2$Overall), ]
cust42

cust.rp.predict2<-predict(cust.rp2, type = "matrix", newdata = test_data)
cust.rp.prob.rocr2<-prediction(cust.rp.predict2,test_data$CARAVAN)
cust.rp.perf2<-performance(cust.rp.prob.rocr2,"tpr","fpr")
plot(cust.rp.perf2,main="ROC curve on RPart",colorize=T)

# 6. Deployment
