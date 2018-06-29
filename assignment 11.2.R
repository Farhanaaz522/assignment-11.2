#Problem
#1. Use the below given data set
#   DataSet
#2. Perform the below given activities:
#a. apply K-means clustering to identify similar recipies
#b. apply K-means clustering to identify similar attributes
#c. how many unique recipies that people order often
#d. what are their typical profiles


#Answers
#reading the dataset
#using wine dataset

library(pvclust)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(ggplot2)

#reading the dataset
data <- read.csv("D:/acadgild/wine.csv")
View(data)
#structure of it
str(data)

# Normalization/scaling
df <- scale(data[,-c(1,2)])

#finding optimal clusters for our data by looking into plot
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

#The data parameter is the numeric dataset to be analyzed, 
#nc is the maximum number of clusters to consider, 
#and seed is a random number seed.

#of our main dataset df
wssplot(df)

library(NbClust)
set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
nc
table(nc$Best.n[1,])

barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")

#The plot above represents the variance within the clusters. 
#It decreases as k increases, but it can be seen a bend (or "elbow") at k = 3. 
#This bend indicates that additional clusters beyond the third have little value.
