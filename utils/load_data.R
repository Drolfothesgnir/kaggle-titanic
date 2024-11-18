library(tidyverse)

train_pathname <- './data/train.csv'
test_pathname <- './data/test.csv'

load_train_data <- function() {
  return (read_csv(
    train_pathname,
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

load_test_data <- function() {
  return (read_csv(
    test_pathname,
    col_types = cols(
      PassengerId = "i",
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
