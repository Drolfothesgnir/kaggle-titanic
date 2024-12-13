library(dplyr)
library(ggplot2)

source("utils/load_data.R")
source("utils/create_boxplot.R")
source("two_ladies.R")

df <- load_train_data() %>%
  impute_embarked()
df %>%
  mutate(log_fare = log(Fare)) %>%
  create_boxplot(
    "Pclass",
    "log_fare",
    "Distribution of Log Fares based on passengers class",
    "Passengers class",
    "Log Fare"
  )

summary(aov(Fare ~ Pclass, data = df))
# Df  Sum Sq Mean Sq F value Pr(>F)    
# Pclass        2  776030  388015   242.3 <2e-16 ***
# Residuals   888 1421769    1601                   
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

class_based_fare_medians <- df %>%
  group_by(Pclass) %>%
  summarize(median_fare = median(Fare))

#   Pclass median_fare
#   <fct>        <dbl>
# 1 1            60.3 
# 2 2            14.2 
# 3 3             8.05

summary(aov(Fare ~ Pclass + Embarked, data = df))
# Df  Sum Sq Mean Sq F value   Pr(>F)    
# Pclass        2  776030  388015 245.666  < 2e-16 ***
# Embarked      2   22381   11191   7.085 0.000886 ***
# Residuals   886 1399387    1579                     
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

class_embarked_based_fare_medians <- df %>%
  group_by(Pclass, Embarked) %>%
  summarize(median_fare = median(Fare))
#   Groups:   Pclass [3]
#   Pclass Embarked median_fare
#   <fct>  <chr>          <dbl>
# 1 1      C              78.3 
# 2 1      Q              90   
# 3 1      S              52.6 
# 4 2      C              24   
# 5 2      Q              12.4 
# 6 2      S              13.5 
# 7 3      C               7.90
# 8 3      Q               7.75
# 9 3      S               8.05

impute_fare <- function(data) {
  data %>%
    left_join(class_embarked_based_fare_medians, by = c("Pclass", "Embarked")) %>%
    mutate(fare_imputed = is.na(Fare),
           Fare = coalesce(Fare, median_fare)) %>%
    select(-median_fare)
}