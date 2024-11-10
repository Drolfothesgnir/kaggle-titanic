source("utils/load_data.R")
source("two_ladies.R")
source("feature_engineering.R")
source("age.R")

prepare_data <- function(df) {
  return(load_data() %>%
           impute_embarked() %>%
           engineer_features() %>%
           impute_age())
}