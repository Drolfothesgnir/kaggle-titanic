# Two ladies, Miss Amelie Icard and Mrs. Martha Evelyn have unknown location of
# embarkation

library(tidyverse)

source("utils/load_data.R")

df <- load_train_data()

df %>%
  filter(is.na(Embarked))

df %>%
  filter(Pclass == 1) %>%
  drop_na(Embarked) %>%
  group_by(Embarked) %>%
  summarise(n_embarked = n())

# Embarked n_embarked
# <fct>           <int>
# 1 S               127
# 2 C                85
# 3 Q                 2

# Most of the first class passengers embarked in Southampton.
# Ticket number suggests it was purchased in Southampton

impute_embarked <- function(data) {
  return (
    data %>%
      mutate(Embarked = if_else(is.na(Embarked), "S", Embarked))
  )
}