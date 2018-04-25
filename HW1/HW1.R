library(rstudioapi)
library(rpart)
rm(list = ls())

# Question 1
#############
#Set working directory to same 
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path))

#Read data
age_data <- read.csv('age_stats315B.csv')
factor_columns <- c('Occup', 'TypeHome', '',)
df[factor_columns] <- lapply(df[factor_columns], factor)
