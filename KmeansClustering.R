# k-means clustering

# clear console, clear workspace
cat("\014")
rm(list = ls())

workDirectory <- c("C:/Users/martin/Desktop/RProjects")
setwd(workDirectory)

# data loading and cleaning
library(datasets)
head(iris)

library(ggplot2)
ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()

# clustering
set.seed(101)
irisCluster <- kmeans(iris[, 1:4], 3, nstart = 20)
table(irisCluster$cluster, iris$Species)

# cluster visualization
library(cluster)
clusplot(iris, irisCluster$cluster, color=TRUE, shade=TRUE, labels=0,lines=0, )
