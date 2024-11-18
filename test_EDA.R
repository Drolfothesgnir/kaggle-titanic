library(tidyverse)

source("utils/load_data.R")
source("feature_engineering.R")
source("age.R")

df <- load_test_data() %>%
  engineer_features() %>%
  impute_age()

df