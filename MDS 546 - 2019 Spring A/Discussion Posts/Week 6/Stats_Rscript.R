library(readxl)
stats_workbook <- read_excel("C:/Users/cusey/Downloads/data_stats.xlsx")
stats_workbook

cor.test(x=stats_workbook$X,y=stats_workbook$Y,conf.level = 0.95)

myLM <- lm(stats_workbook$X ~ stats_workbook$Y)

summary(myLM)

#Chapter 14 Q18
library(readxl)
angry_moods <- read_excel("C:/Users/cusey/Downloads/angry_moods.xls")
angry_moods

#Plot the data (X,Y)
plot(angry_moods$'Anger-Out',angry_moods$'Control-Out')


#Fit a line
myLM <- lm(angry_moods$'Anger-Out' ~ angry_moods$'Control-Out')

#Visual the line
lines(angry_moods$'Anger-Out',myLM$fitted)

#Show our coefficients
myLM$coeff

summary(myLM)

#Chapter 15
#Q28

#Load Data
library(readxl)
data_ADHD <- read_excel("C:/Users/cusey/Downloads/adhd.xls")
data_ADHD

#Box Plot for fun
boxplot(data_ADHD$D0, data_ADHD$D15, data_ADHD$D30, data_ADHD$D60)

xdf <- data.frame(cbind(data_ADHD$D0, data_ADHD$D15, data_ADHD$D30, data_ADHD$D60))
View(xdf)

summary(xdf)

xs <- stack(xdf)
View(xs)

anova1 <- aov(values~ind, data = xs)
anova1
summary(anova1)

#Just for fun even though they are not statistically significant
TukeyHSD(anova1)

#Chapter 15
#Q29
library(readxl)
angry_moods <- read_excel("C:/Users/cusey/Downloads/angry_moods.xls")
angry_moods

boxplot(angry_moods$Anger_Expression ~ angry_moods$Gender*angry_moods$Sports + 
          angry_moods$Gender:angry_moods$Sports, data = angry_moods)

anova <- aov(angry_moods$Anger_Expression ~ angry_moods$Gender*angry_moods$Sports + 
               angry_moods$Gender:angry_moods$Sports, data = angry_moods)
summary(anova)
#Chapter 17
#Q11
library(readxl)
data_diet <- read_excel("C:/Users/cusey/Downloads/diet.xls")
data_diet


#Failed attempts to fix
#data <- data.frame(data_diet)
#rownames(data) <- c("AHA","MED")
#View(data)
#convert data as a table
#dt <- as.table(as.matrix(data_diet))
#View(dt)

chisq <- chisq.test(data_diet)
