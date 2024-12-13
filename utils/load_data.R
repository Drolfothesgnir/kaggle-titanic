library(tidyverse)

train_pathname <- './data/train.csv'
test_pathname <- './data/test.csv'

load_train_data <- function() {
  data <- read_csv(
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
  )
  data$Survived <- factor(data$Survived,
                          levels = c(0, 1),
                          labels = c("No", "Yes"))
  
  return (data)
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
