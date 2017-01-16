# Logistic Regression

# clear console, clear workspace
cat("\014")
rm(list = ls())

workDirectory <- c("C:/Users/martin/Desktop/RProjects")
setwd(workDirectory)

# data loading
df.train <- read.csv("titanic_train.csv")

# data exploration

  # data head
  head(df.train)

  # missing data
  library(Amelia)
  missmap(df.train, main = "TitanicDataMissMap", col = c("yellow", "black"), legend = F)
  graphics.off()

  # basic stats
  library(ggplot2)
  ggplot(df.train, aes(Survived), alpha = 0.5) + geom_bar()
  graphics.off()
  
  # histogram
  library(ggplot2)
  ggplot(df.train, aes(Age)) + geom_histogram(fill = 'blue', bins = 20, alpha = 0.5)
  graphics.off()

  # box plot
  library(ggplot2)
  pl <- ggplot(df.train, aes(Pclass, Age)) + geom_boxplot(aes(group = Pclass, fill = factor(Pclass), alpha = 0.4))
  pl + scale_y_continuous(breaks = seq(min(0), max(80), by = 2))
  graphics.off()
  
# data cleaning

  # data imputation
  impute_age <- function(age, class) {
    out <- age
    
    for (i in 1:length(age)) {
      
      if (is.na(age[i])) {
        
        if (class[i] == 1) {
          out[i] <- 37
          
        } else if (class[i] == 2) {
          out[i] <- 29
          
        } else {
          out[i] <- 24
        }
      } else {
        out[i] <- age[i]
      }
    }
    
    return(out)
  }
  
  df.train$Age <- impute_age(df.train$Age, df.train$Pclass)
  
  # check missing data again
  library(Amelia)
  missmap(df.train, main = "TitanicDataMissMap", col = c("yellow", "black"), legend = F)
  graphics.off()

# model training
  
  # select columns for trainig
  library(dplyr)
  df.train <- select(df.train, -PassengerId, -Name, -Ticket, -Cabin)
  
  # check data again
  str(df.train)
  
  # encode int as a levels
  df.train$Survived <- factor(df.train$Survived)
  df.train$Pclass <- factor(df.train$Pclass)
  df.train$Parch <- factor(df.train$Parch)
  df.train$SibSp <- factor(df.train$SibSp)
  
  # train model
  log.model <- glm(formula=Survived ~ . , family = binomial(link='logit'), data = df.train)
  summary(log.model)  
  
# model evaluation
  
  # setup evaluation
  library(caTools)
  set.seed(101)
  
  split = sample.split(df.train$Survived, SplitRatio = 0.70)
  final.train = subset(df.train, split == TRUE)
  final.test = subset(df.train, split == FALSE)
  
  final.log.model <- glm(formula=Survived ~ . , family = binomial(link='logit'), data = final.train)
  summary(final.log.model)
  
  # prediction accuracy
  fitted.probabilities <- predict(final.log.model, newdata=final.test, type='response')
  fitted.results <- ifelse(fitted.probabilities > 0.5,1,0)
  misClasificError <- mean(fitted.results != final.test$Survived)
  
  print(paste('Accuracy',1-misClasificError))
  table(final.test$Survived, fitted.probabilities > 0.5)
  
# EOF