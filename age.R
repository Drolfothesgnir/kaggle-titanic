library(tidyverse)

source("utils/load_data.R")
source("two_ladies.R")
source("feature_engineering.R")

df <- load_train_data() %>%
  impute_embarked() %>%
  engineer_features()

sum(is.na(df$Age)) # 177

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

create_boxplot(
  df_age_1,
  "title_clean",
  "Age",
  fill = "Sex",
  dodge = TRUE,
  title = "Distribution of age for every title, grouped by passengers sex",
  x_label = "Title"
)

# Plots suggests that different title groups have different age distributions.
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
impute_age <- function(data) {
  return (
    data %>%
      left_join(age_imputation_data, by = c("title_clean", "Pclass")) %>%
      mutate(
        age_imputed = is.na(Age),
        Age = coalesce(Age, median_age)
      ) %>%
      select(-median_age)
  )
}

df <- df %>%
  impute_age()

df %>%
  summarise(
    missing_age = sum(is.na(Age)),
    imputed_count = sum(age_imputed),
    total_rows = n()
  )
