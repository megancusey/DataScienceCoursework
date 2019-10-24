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
  fi_rules <- apriori(data, parameter=list(support=minsup,target="frequent itemsets"))
  inspect(sort(fi_rules, by = "support"))
  
  #Create Apriori Rules
  ap_rules <- apriori(data, parameter = list(support = minsup, confidence = minconf, target = "rules"))
  inspect(sort(ap_rules, by = "support"))
  
  return (fi_rules)
}

#Reading Data from a file - format is one entry per transaction

file_name <- readline(prompt="Enter File Name & Extension:")
min_sup <- as.numeric(readline(prompt="Enter Minimum Support:"))
min_conf <- as.numeric(readline(prompt="Enter Minimum Confidence:"))
unique_id_first_column <- as.numeric(readline(prompt="Enter a 1 if first column is the unique key:"))


results <- function_apriori(file_name, min_sup, min_conf, unique_id_first_column)
inspect(sort(results, by = "support"))
write(results, file = "rules2.csv", sep = ",")


rm(list=ls())