# Two ladies, Miss Amelie Icard and Mrs. Martha Evelyn have unknown location of 
# embarkation

library(tidyverse)

df <- read_rds("processed_data.rds")

df %>%
  filter(is.na(Embarked))

df %>% 
  filter(Pclass == 1) %>%
  drop_na(Embarked) %>%
  group_by(Embarked) %>%
  summarise(n_embarked = n())

# Most of the first class passengers embarked in Southampton.
# Ticket number suggests it was purchased in Southampton