getwd()


library(arules)
library(arulesViz)
library(RColorBrewer)

## Define function
function_apriori <- function (file_name, minsup, minconf, unique_id_first_column) {
  
  data <- read.csv(file_name)

  if (unique_id_first_column == 1)
  {
    ## remove first column
    data <- data[ -1]
  }
  
  ## Replace any missing values with 0
  data[is.na(data)] <- 0
  
  ## Turns into T/F Values
  data <- apply(data,2,as.logical)
  
  
  #Create Frequent Itemsets
  help(apriori)
  
  rules <- apriori(data, parameter=list(support=minsup,target="frequent itemsets"))
  inspect(head(sort(rules, by = "support"), 50))
  
  #Create Apriori Rules
  ##rules_6_6c <- apriori(ap7, parameter = list(support = 0.6, confidence = 0.8, target = "rules"))
  ##inspect(head(sort(rules_6_6c, by = "support"), 50))
  
  #Analyze and Visualize Results
  ##inspect(rules_6_6_r[1:10])
  ##itemFrequencyPlot(transactional_data_6_6,topN=20,col=brewer.pal(8,'Pastel2'), main='Relative Item Frequency Plot',type="relative",ylab="Item Frequency (Relative)")
  ##plot(rules_6_6_r[1:10],method="graph")
  ##plot(rules_6_6_r[1:10],method="paracoord",control=list(reorder=TRUE))
  ##plot(rules_6_6_r[1:10],method="matrix",control=list(reorder='support/confidence'))
  
  
  return (inspect)
}

#Reading Data from a file - format is one entry per transaction

file_name <- readline(prompt="Enter File Name & Extension:")
min_sup <- as.numeric(readline(prompt="Enter Minimum Support:"))
min_conf <- as.numeric(readline(prompt="Enter Minimum Confidence:"))
unique_id_first_column <- as.numeric(readline(prompt="Enter a 1 if first column is the unique key:"))



apriori(file_name, min_sup, min_conf, unique_id_first_column)


##rm(list=ls())