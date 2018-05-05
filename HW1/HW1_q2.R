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
house_data[factor_columns] <- lapply(house_data[factor_columns], as.factor)
fit <- rpart(TypeHome ~ ., data = house_data, method = 'class', control=rpart.control(minbucket = 10, xval = 10, maxsurrogate = 5, usesurrogate = 2, cp=0.0001))

# Find the minimum cross-validation error + one SD
min_error_window <- min(fit$cptable[,"xerror"] + fit$cptable[,"xstd"])
# Find the simplest model with xerror within the min_error_window
best_cp <- first(fit$cptable[which(fit$cptable[,"xerror"] < min_error_window),"CP"])
best_fit <- prune(fit, cp = best_cp)

rpart.plot(best_fit)

#misclassification
y_hat <- predict(best_fit, house_data)
y_hat_max_p <- apply(y_hat,1,which.max)
y <- house_data$TypeHome
correct <- y == y_hat_max_p
pct_correct <- sum(correct)/length(correct)
1-pct_correct

