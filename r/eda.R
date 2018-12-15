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

 p1

ggplot(ibm, aes(MonthlyIncome, MonthlyRate)) +
  geom_hex()

# Visualize Income and Rate
pm <- ggpairs(ibm, mapping = aes(colour = Attrition, alpha = 0.7), columns = c("MonthlyIncome", "MonthlyRate", "DailyRate", "HourlyRate"))
pm

ggplot(ibm, aes(Attrition, MonthlyIncome, fill = Attrition)) +
  geom_boxplot()

# job level ---------------------------------------------------------------

ibm$JobLevel %>% table()

ggplot(ibm, aes(JobLevel, MonthlyIncome, group = JobLevel)) +
  geom_boxplot(aes(fill = factor(JobLevel))) +
  theme(legend.position = "none")

ggplot(ibm, aes(factor(JobLevel))) +
  geom_bar(aes(fill = factor(JobLevel))) +
  facet_grid(~ Attrition)

ggplot(ibm, aes(factor(JobLevel), group = Attrition)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)),
           stat = "count", alpha = 0.7) +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop..),
            stat = "count", vjust = -.5) +
  facet_grid(~ Attrition)

ggplot(ibm, aes(factor(JobLevel), fill = Attrition)) +
  geom_bar(position = "fill") +
  labs(x = "Job Level", y = "Percentage")

ggplot(ibm, aes(Department, fill = Attrition)) +
  geom_bar(position = "fill")

ggplot(ibm, aes(factor(Education), MonthlyIncome)) +
  geom_boxplot()

ibm %>% 
  group_by(Attrition) %>% 
  summarise(
    env_mean = mean(EnvironmentSatisfaction),
    job_mean = mean(JobSatisfaction),
    n = n()
  )

ggplot(ibm, aes(OverTime, fill = Attrition)) +
  geom_bar(position = "fill")

# OverTime effect Attrition
ibm %>% 
  count(Attrition, OverTime) %>% 
  ggplot(aes(Attrition, OverTime)) +
  geom_tile(aes(fill = n))
  
  