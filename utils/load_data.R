library(tidyverse)

pathname <- './data/train.csv'

load_data <- function() {
  return (read_csv(
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
  ))
}

