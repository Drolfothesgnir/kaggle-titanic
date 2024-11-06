library(tidyverse)

df <- read_rds("processed_data.rds")

sum(is.na(df$Age))

regex_string <- ".*, ([A-Za-z ]+\\.?) "

extract_title <- function(name) {
  return (str_match(name, regex_string)[2])
}

df <- df %>%
  mutate(title = as.factor(sapply(Name, extract_title, USE.NAMES = FALSE)), family_size = Parch + SibSp)

df_age_1 <- df %>%
  filter(!is.na(Age))

unique_titles <- df %>%
  group_by(title) %>%
  summarise(n = n()) %>%
  filter(n < 6) %>%
  select(title)

df_age_1 <- df_age_1 %>%
  filter(!(title %in% unique_titles$title))

df_age_1 %>%
  ggplot(aes(x = title, y = Age)) +
  geom_boxplot()

# check if age depends on class

df_age_1 %>%
  ggplot(aes(x = Pclass, y = Age)) +
  geom_boxplot()

df_age_1 %>%
  ggplot(aes(x = factor(family_size), y = Age)) +
  geom_boxplot()

df_age_1 %>%
  ggplot(aes(x = Sex, y = Age)) +
  geom_boxplot()


df_age_1 %>%
  ggplot(aes(x = title, y = Age, fill = Pclass)) +
  geom_boxplot(position = "dodge") +
  labs(title = "Distribution of age for every title, grouped by passengers class", x = "Title")

# check requirements for ANOVA
for (i in 1:3) {
  data <- df_age_1 %>%
    filter(Pclass == i)
  print(sprintf("Class %d", i))
  print(shapiro.test(data$Age))
  print(var(data$Age))
}

# H0 <- Age is the same between classes
summary(aov(Age ~ Pclass, data = df_age_1))
# reject H0
kruskal.test(Age ~ Pclass, data = df_age_1)
# reject H0

title_class_age_dist <- df_age_1 %>%
  group_by(title, Pclass) %>%
  summarise(median_age = median(Age)) %>%
  pivot_wider(names_from = Pclass, values_from = median_age)

# Convert title_class_age_dist to long format for joining
age_imputation_data <- title_class_age_dist %>%
  pivot_longer(cols = -title,
               names_to = "Pclass",
               values_to = "median_age")

df <- df %>%
  left_join(age_imputation_data, by = c("title", "Pclass")) %>%
  mutate(Age = coalesce(Age, median_age)) %>%
  select(-median_age)

saveRDS(df, "processed_data.rds")
