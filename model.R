library(caret)
library(tidyverse)

df <- read_rds("processed_data.rds")

set.seed(1)
# Set up cross-validation with k folds (e.g., 10)
train_control <- trainControl(method = "cv", number = 10)

# Train and cross-validate the model on the same dataset in a proper way
model <- train(
  Survived ~ Age + Sex + Pclass + title_clean + family_size,
  data = df,
  method = "glm",
  family = "binomial",
  trControl = train_control
)

# View cross-validation results
print(model)
summary(model)