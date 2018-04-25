library(rstudioapi)
library(rpart)
library(rpart.plot)
rm(list = ls())

# Question 1
#############
#Set working directory to data subdirectory
current_path <- getActiveDocumentContext()$path 
current_directory <- dirname(current_path)
data_path <- paste(current_directory,'/data',sep='')
setwd(data_path)

#Read and type data
age_data <- read.csv('age_stats315B.csv')
factor_columns <- c(
  'Occup',
  'TypeHome',
  'sex',
  'MarStat',
  'Edu',
  'LiveBA',
  'DualInc',
  'HouseStat',
  'Ethnic',
  'Lang'
)
age_data[factor_columns] <- lapply(age_data[factor_columns], factor)
fit <- rpart(age ~ ., data = age_data)
fit
#First split: Marital status
#Strong indicators of you are not having ever been married.
#  2. Living together, not married
#  5. Single, never married
#For the unmarried...
#then the next thing is whether or not you own/rent your own place/live with parents
#there are two more splits, on education (younger: grade 11 or less, older: at least high school)
#                           and occupation (younger: not retired, retired)


# 1. Married
# 3. Divorced or separated
# 4. Widowed
#For the folks who have been married...
#then the next best split is occupation, retired vs. all other occupations
#Then the non-retired folks split on owning home vs. renting & living with family
#Then the folks who own a home, if they have less then half a person under 18 they are young

#a)
fit$frame$nsurrogate
which(fit$frame$nsurrogate != 0) + 1
colnames(age_data)[fit$frame$nsurrogate]
#yes there were surrogate splits used. Surrogates are variables used to classify data points
#that have missing values for a given feature The surrogate variables used were:


#b)
ncols <- dim(age_data)[2]
joe_data <- data.frame(matrix(ncol = ncols, nrow = 0))
                             #1,2,3,4,5,6,7,8,9,10,11,12,13,14
joe_data <- rbind(joe_data, c(3,1,3,1,5,6,9,3,1, 4, 0, 2, 8, 1))
colnames(joe_data) <- colnames(age_data)
joe_data[factor_columns] <- lapply(joe_data[factor_columns], factor)
predict(fit, joe_data)

#result: 2.8, on the upper end of 18 through 24. I'm 29. Gotcha tree!
rpart.plot(fit)
