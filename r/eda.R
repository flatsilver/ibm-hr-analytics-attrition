library(tidyverse)
library(scales)
library(rpart)
library(rpart.plot)
library(randomForest)
library(GGally)

ibm <- read_csv("data/WA_Fn-UseC_-HR-Employee-Attrition.csv")

ibm %>% 
  group_by(Attrition) %>% 
  summarise(
    n = n(),
    percentage = n() / length(ibm$Attrition)
  )

foo <- ibm %>% 
  group_by(Attrition) %>% 
  count()

p1 <- foo %>% 
  mutate(percentage = str_c(as.character(round(n / sum(foo$n) * 100, 1)), "%")) %>% 
  ggplot(aes(Attrition, n, fill = Attrition)) +
  geom_col() +
  geom_label(aes(label = percentage), position = position_dodge(width = 1)) +
  labs(y = "Number of people") +
  theme(legend.position = "none")

ggplot(ibm, aes(MonthlyIncome, MonthlyRate)) +
  geom_hex()

pm <- ggpairs(ibm, mapping = aes(colour = Attrition), columns = c("MonthlyIncome", "MonthlyRate", "DailyRate", "HourlyRate"))
pm
