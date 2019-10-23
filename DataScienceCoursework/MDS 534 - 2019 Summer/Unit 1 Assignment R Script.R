library(pastecs)
## Load Data
exercise2.4 <- cbind(c(23,23,27,27,39,41,47,49,50,
                     52,54,54,56,57,58,58,60,61), 
                   c(9.5,26.5,7.8,17.8,31.4,25.9,
                     27.4,27.2,31.2,34.6,42.5,28.8,
                     33.4,30.2,34.1,32.9,41.2,35.7))

colnames(exercise2.4) <- c("Age", "%fat")

## Get Mean, Median, Standard Deviation
stat.desc(exercise2)

## Draw Box Plot
boxplot(exercise2.4)

## Draw Scatter Plot
plot(exercise2.4)

## Draw q-q plot
qqnorm(exercise2.4)


## Load Data
tuple1 <- c(22,1,42,10)
tuple2 <- c(20,0,36,8)

##Euclidean Distance
dist(rbind(tuple1,tuple2))

##Manhattan Distance
dist(rbind(tuple1,tuple2), method="manhattan")

##Minkowski Distance
dist(rbind(tuple1,tuple2), method="minkowski",p=3)

##Supremum Norm Distance
dist(rbind(tuple1,tuple2), method="maximum")
