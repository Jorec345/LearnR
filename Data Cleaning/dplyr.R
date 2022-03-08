#data cleaning 
#calling the libraries
library(dplyr)
library(tidyverse)

##data cleaning
flowersData <- iris
head(flowersData,10)
str(flowersData)
unique(flowersData$Species)

average.length <- mean(flowersData$Sepal.Length)


avgCols <- 
  flowersData %>%
  filter(Species == "setosa") %>%
  select(c(Sepal.Length,Sepal.Width,Petal.Length,Petal.Width))%>%
  apply(2,mean)

summary(flowersData)
dim(flowersData)

grpiris <- 
  flowersData %>%
  group_by(Species)%>%
  select(-5)%>%
  #apply(2,mean)
  summarise_all(mean)


