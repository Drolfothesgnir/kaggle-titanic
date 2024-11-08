library(tidyverse)

df <- read_rds("processed_data.rds")

sum(is.na(df$Age)) # 177

# <-------------------- Feature Engineering -------------------->

# Regex for extracting title from Name of the passenger
# 'Moran, Mr. James' -> 'Mr.'
regex_string <- ".*, ([A-Za-z ]+\\.?) "

# Should work fine, since every passenger (in the training set) has a title
extract_title <- function(name) {
  return (str_match(name, regex_string)[2])
}

# combining number of parents/children and number of siblings
df <- df %>%
  mutate(family_size = Parch + SibSp)

# extracting title from a name
df <- df %>%
  mutate(title = as.factor(sapply(Name, extract_title, USE.NAMES = FALSE)))

# Combining rare titles into more common groups

coerce_titles <- function(title) {
  return(
    case_when(
      grepl("Capt|Col|Major", title) ~ "Military",
      grepl("Mme|Ms", title) ~ "Mrs.",
      grepl("Don|Lady|Sir|the Countess|Jonkheer", title) ~ "Noble",
      grepl("Mlle", title) ~ "Miss.",
      grepl("Dr|Rev", title) ~ "Professional",
      .default  = as.character(title)
    )
  )
}

# Rare titles are grouped together because with their unique titles they represent
# only few people
title_clean <- coerce_titles(as.character(df$title))

df <- df %>%
  mutate(title_clean = factor(title_clean))

# <-------------------- Visualization of Age distributions -------------------->

source("utils/create_boxplot.R")
source("utils/create_violin_point_plot.R")

df_age_1 <- df %>%
  filter(!is.na(Age))

# check if age is related to class
create_boxplot(df_age_1,
               "Pclass",
               "Age",
               title = "Age distribution by passengers class",
               box_color = "orange")

# Check of age is related to family size
create_violin_point_plot(df_age_1, "family_size", "Age")

create_boxplot(df_age_1, "Sex", "Age", title = "Age distribution by passengers sex", fill = "Sex")

create_boxplot(df_age_1,
               "title_clean",
               "Age",
               title = "Distribution of age for every title",
               x_label = "Title")

create_boxplot(
  df_age_1,
  "title_clean",
  "Age",
  fill = "Pclass",
  dodge = TRUE,
  title = "Distribution of age for every title, grouped by passengers class",
  x_label = "Title"
)

# Plots suggests that different titlt groups have different age distributions.
# Also within (almost) each title group, class-based age distributions are different.

# <-------------------- Age imputation -------------------->

rare_titles <- c("Military", "Noble", "Professional")

small_group_medians <- df_age_1 %>%
  filter(title_clean %in% rare_titles) %>%
  group_by(title_clean) %>%
  summarise(median_age = median(Age)) %>%
  crossing(Pclass = factor(1:3))

large_group_medians <- df_age_1 %>%
  filter(!(title_clean %in% rare_titles)) %>%
  group_by(title_clean, Pclass) %>%
  summarise(median_age = median(Age))

age_imputation_data <- bind_rows(small_group_medians, large_group_medians)

age_imputation_data

# Imputing age from title and class
df <- df %>%
  left_join(age_imputation_data, by = c("title_clean", "Pclass")) %>%
  mutate(age_imputed = is.na(Age), Age = coalesce(Age, median_age)) %>%
  select(-median_age)

df %>% 
  summarise(
    missing_age = sum(is.na(Age)),
    imputed_count = sum(age_imputed),
    total_rows = n()
  )

saveRDS(df, "./processed_data.rds")