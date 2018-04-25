library(rstudioapi)
library(rpart)
library(rpart.plot)
rm(list = ls())

# Question 2
#############
#Set working directory to data subdirectory
current_path <- getActiveDocumentContext()$path 
current_directory <- dirname(current_path)
data_path <- paste(current_directory,'/data',sep='')
setwd(data_path)

#Read and type data
house_data <- read.csv('housetype_stats315B.csv')
factor_columns <- c(
  'TypeHome',
  'sex',
  'MarStat',
  'Occup',
  'LiveBA',
  'DualInc',
  'HouseStat',
  'Ethnic',
  'Lang'
)
house_data[factor_columns] <- lapply(house_data[factor_columns], factor)
fit <- rpart(age ~ ., data = house_data, method = 'class')
rpart.plot(fit)
fit

#say something here

#misclassification
y_hat <- predict(fit, house_data)
y_hat_max_p <- apply(y_hat,1,which.max)
y <- house_data$TypeHome
correct <- y == y_hat_max_p
pct_correct <- sum(correct)/length(correct)
pct_correct


