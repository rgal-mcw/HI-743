install.packages("tidyverse")
library(tidyverse)

install.packages("NHANES")
library(NHANES)

health.data = NHANES

## dplyr usage
### Select
health.data = health.data %>% select(Age, Gender, BMI, BPSysAve, TotChol) 

### Filter
health.data = health.data %>% filter(Age > 50, BMI > 18.5)

### Mutate
health.data = health.data %>% mutate(BMI_Category = ifelse(BMI > 25, "Overweight", "Normal"))

### Summary & group_by
gender.summary = health.data %>% group_by(Gender) %>% summarise(avg_BMI = mean(BMI, na.rm=TRUE)) 

# Missing Values
sum(is.na(health.data))
colSums(is.na(health.data))

health.data_no.missing = health.data %>% drop_na()

## Impute Missing Values
### Mean
health.data = health.data %>% mutate(BPSysAve = ifelse(is.na(BPSysAve), mean(BPSysAve, na.rm = TRUE), BPSysAve))
### Median
health.data = health.data %>% mutate(BPSysAve = ifelse(is.na(BPSysAve), median(BPSysAve, na.rm = TRUE), BPSysAve))


# Cardinality
sapply(health.data, function(x) length(unique(x)))


# ggplot2 
## Histogram
ggplot(data = health.data, aes(x=BMI)) +
  geom_histogram(binwidth = 2, fill="blue", color="black") +
  labs(title = "BMI Distribution", x="BMI", y="Count")
  