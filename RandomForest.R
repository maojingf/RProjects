# Random Forest

# clear console, clear workspace
cat("\014")
rm(list = ls())

workDirectory <- c("C:/Users/martin/Desktop/RProjects")
setwd(workDirectory)

# data loading
library(randomForest)
library(rpart)

# data exploration
str(kyphosis)
head(kyphosis)

# data cleaning
# no need, data is pretty good

# model training / evaluation
model <- randomForest(Kyphosis ~., data=kyphosis)
  print(model)
  importance(model)
