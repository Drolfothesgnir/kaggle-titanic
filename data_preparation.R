library(tidyverse)

pathname <- './data/train.csv'
df <- read_csv(
  pathname,
  col_types = cols(
    PassengerId = "i",
    Survived = "f",
    Pclass  =  col_factor(levels = c("1", "2", "3")),
    Name = "c",
    Sex = "f",
    Age = "d",
    SibSp = "i",
    Parch = "i",
    Ticket = "c",
    Fare = "d",
    Cabin = "c",
    Embarked = "f"
  )
)

df <- df %>%
  mutate(
    cabin_multiple = ifelse(is.na(Cabin), 0, str_count(Cabin, " ") + 1),
    cabin_deck = as.factor(ifelse(is.na(Cabin), "NA", substr(Cabin, 1,1)))
  )

saveRDS(df, "./processed_data.rds")