library(tidyverse)
#install.packages("NHANES")
library(NHANES)

?NHANES
glimpse(NHANES)

data = NHANES
data = data %>% select(Age, Gender, BMI, BPSysAve, TotChol)
data = data %>% filter(Age > 50, BMI > 18.5)
data = data %>% mutate(BMI_Category = ifelse(BMI > 25, "Overweight", "Normal"))
data %>% group_by(Gender) %>% summarise(avg_BMI = mean(BMI, na.rm = TRUE))

sum(is.na(data)) # Count how many NA's total (rows can have multiple)
colSums(is.na(data)) # See where NA's are

#Can drop missing data
data.drop = data %>% drop_na()


#Fill with the mean:
data.mean = data %>% mutate(BMI = ifelse(is.na(BMI), mean(BMI, na.rm = TRUE), BMI))
# Fill with the median:
data.med = data %>% mutate(BMI = ifelse(is.na(BMI), median(BMI, na.rm = TRUE), BMI))
# Fill using group-wise mean:
data = data %>% group_by(Gender) %>% mutate(BMI = ifelse(is.na(BMI), mean(BMI, na.rm = TRUE), BMI)) %>% ungroup()

#Check cardinality
sapply(data, function(x) length(unique(x)))
?sapply


## Figures
# Outlier detection
ggplot(data, aes(x = Gender, y = BMI)) +
  geom_boxplot() +
  labs(title = "BMI Outliers by Gender")

# Get dataframe of just outliers using IQR
Q1 = quantile(data$BMI, 0.25, na.rm = TRUE)
Q3 = quantile(data$BMI, 0.75, na.rm = TRUE)
IQR = Q3 - Q1
outliers = data %>% filter(BMI < (Q1 - 1.5 * IQR) | BMI > (Q3 + 1.5 * IQR))
outliers

# Histogram
ggplot(data, aes(x = BMI)) +
  geom_histogram(binwidth = 2, fill = "blue", color = "black") +
  labs(title = "BMI Distribution", x = "BMI", y = "Count")
## BMI is right skewed due to outliers. How to handle?

ggplot(data, aes(x = Age, y = BMI)) +
  geom_point() +
  labs(title = "Age vs BMI", subtitle = "NHANES Dataset", x = "Age (years)", y = "Body Mass Index (BMI)")

ggplot(data, aes(x = Age, y = BMI, color = Gender)) +
  geom_point() +
  scale_color_manual(values = c("Male" = "blue", "Female" = "red")) +
  labs(title = "Age vs BMI by Gender")

ggplot(data, aes(x = Age, y = BPSysAve)) +
  geom_point() +
  theme(panel.grid.major = element_line(color = "grey", size = 0.5),
        panel.grid.minor = element_line(color = "lightgrey", size = 0.25))

ggplot(data, aes(x = Age, y = BMI)) +
  geom_point() +
  annotate("text", x = 75, y = 60, label = "Possible Outliers above BMI = 60", color = "red", size = 3)


