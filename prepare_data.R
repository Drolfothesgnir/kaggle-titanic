source("utils/load_data.R")
source("two_ladies.R")
source("feature_engineering.R")
source("age.R")
source("fare.R")

prepare_train_data <- function() {
  load_train_data() %>%
    impute_embarked() %>%
    engineer_features() %>%
    impute_age() %>%
    engineer_features_2()
}

prepare_test_data <- function() {
  load_test_data() %>%
    engineer_features() %>%
    impute_age() %>%
    impute_fare() %>%
    engineer_features_2()
}