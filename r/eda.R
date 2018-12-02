library(tidyverse)
library(scales)
library(rpart)
library(rpart.plot)
library(randomForest)

ibm <- read_csv("data/WA_Fn-UseC_-HR-Employee-Attrition.csv")

ibm %>% 
  group_by(Attrition) %>% 
  summarise(
    n = n(),
    percentage = n() / length(ibm$Attrition)
  )
