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

# Combiing rare titles into more common groups

coerce_titles <- function(title) {
  return(case_when(
    grepl("Capt|Col|Major", title) ~ "Military",
    grepl("Mme|Ms", title) ~ "Mrs.",
    grepl("Don|Lady|Sir|the Countess|Jonkheer", title) ~ "Noble",
    grepl("Mlle", title) ~ "Miss.",
    grepl("Dr|Rev", title) ~ "Professional",
    .default  = as.character(title)
  ))
}

title_clean <- coerce_titles(as.character(df$title))

df <- df %>%
  mutate(title_clean = factor(title_clean))

# <-------------------- Visualization of Age distributions -------------------->

source("utils/create_boxplot.R")
source("utils/create_violin_point_plot.R")

df_age_1 <- df %>%
  filter(!is.na(Age))

create_boxplot(df_age_1,
               "title_clean",
               "Age",
               title = "Age distribution by passengers title")

# check if age is related to class
create_boxplot(df_age_1,
               "Pclass",
               "Age",
               title = "Age distribution by passengers class",
                box_color = "orange"
               )

# Check of age is related to family size
create_violin_point_plot(df_age_1, "family_size", "Age")

create_boxplot(df_age_1,
               "Sex",
               "Age",
               title = "Age distribution by passengers sex",
               fill = "Sex"
)

create_boxplot(
  df_age_1, 
  "title_clean", 
  "Age", 
  fill = "Pclass", 
  dodge = TRUE, 
  title = "Distribution of age for every title, grouped by passengers class",
  x_label = "Title"
  )

# check requirements for ANOVA
# for (i in 1:3) {
#   data <- df_age_1 %>%
#     filter(Pclass == i)
#   print(sprintf("Class %d", i))
#   print(shapiro.test(data$Age))
#   print(var(data$Age))
# }
# 
# # H0 <- Age is the same between classes
# summary(aov(Age ~ Pclass, data = df_age_1))
# # reject H0
# kruskal.test(Age ~ Pclass, data = df_age_1)
# # reject H0
# 
# title_class_age_dist <- df_age_1 %>%
#   group_by(title, Pclass) %>%
#   summarise(median_age = median(Age)) %>%
#   pivot_wider(names_from = Pclass, values_from = median_age)
# 
# # Convert title_class_age_dist to long format for joining
# age_imputation_data <- title_class_age_dist %>%
#   pivot_longer(cols = -title,
#                names_to = "Pclass",
#                values_to = "median_age")
# 
# df <- df %>%
#   left_join(age_imputation_data, by = c("title", "Pclass")) %>%
#   mutate(Age = coalesce(Age, median_age)) %>%
#   select(-median_age)
# 
# saveRDS(df, "processed_data.rds")
