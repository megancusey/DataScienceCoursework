library(stats)

data <- read.csv("Car evaluation dataset.csv")

#plot raw data
plot(data$Rating)

## Creat summary tables
table.ratings <- table(data$Rating)
table.purchaseprice <- table(data$Purchase.Price)
table.maintenancecost <- table(data$Maintenance.Costs)
table.doors <- table(data$Doors)
table.persons <- table(data$Persons)

##Purchase Price
  ## Frequencies
  table.purchaseprice <- sort(table.purchaseprice, decreasing=TRUE)
  table.purchaseprice
  ## Proportions
  round(prop.table(table.purchaseprice),2)*100


barplot(table.purchaseprice[order(table.purchaseprice,decreasing = TRUE)],
        main="Frequencies of Purchase Price in \n car evaluation dataset",
        xlab="Type of Purchase Price",
        ylab="Frequency of Purchase Price")

##Maintenance Cost
  ## Frequencies
  table.maintenancecost <- sort(table.maintenancecost, decreasing=TRUE)
  table.maintenancecost
  ## Proportions
  round(prop.table(table.maintenancecost),2)*100
  
  
  barplot(table.maintenancecost[order(table.maintenancecost,decreasing = TRUE)],
          main="Frequencies of Maintenance Cost in \n car evaluation dataset",
          xlab="Type of Maintenance Cost",
          ylab="Frequency of Maintenance Cost")
  

  
  
## Doors
  ## Frequencies
  table.doors <- sort(table.doors, decreasing=TRUE)
  table.doors
  ## Proportions
  round(prop.table(table.doors),2)*100
  
  
  barplot(table.doors[order(table.doors,decreasing = TRUE)],
          main="Frequencies of Number of Doors in \n car evaluation dataset",
          xlab="Number of Doors",
          ylab="Frequency of Number of Doors")
  
  
## Persons
  ## Frequencies
  table.persons <- sort(table.persons, decreasing=TRUE)
  table.persons
  ## Proportions
  round(prop.table(table.persons),2)*100
  
  
  barplot(table.persons[order(table.persons,decreasing = TRUE)],
          main="Frequencies of Number of Persons in \n car evaluation dataset",
          xlab="Number of Persons",
          ylab="Frequency of Number of Persons")
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ## Proportions
  round(prop.table(table.ratings),2)*100
  ## RATINGS
  barplot(table.ratings[order(table.purchaseprice,decreasing = TRUE)],
          main="Frequencies of Ratings in \n car evaluation dataset",
          xlab="Type of Rating",
          ylab="Frequency of Rating")


rm(list=ls())