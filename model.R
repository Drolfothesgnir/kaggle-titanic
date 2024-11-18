library(caret)
library(tidyverse)
source("prepare_data.R")

# Load and prepare data once
df <- prepare_data() %>%
  mutate(Survived = factor(Survived, levels = c(0, 1), labels = c("No", "Yes")))

# Set up cross-validation control once
set.seed(1)
train_control <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,  # Get probability predictions
  savePredictions = "final",  # Save predictions for later analysis
)

# Create formula once
model_formula <- Survived ~ Age + Pclass + title_clean + family_size

# Train multiple models
models <- list()

# Logistic Regression
models[["logistic"]] <- train(
  model_formula,
  data = df,
  method = "glm",
  family = "binomial",
  trControl = train_control
)

# LDA
models[["lda"]] <- train(
  model_formula,
  data = df,
  method = "lda",
  trControl = train_control
)

# Compare results
results <- resamples(models)
summary(results)

# Visualization of comparison
bwplot(results)

# Detailed model summaries
print(models[["logistic"]])
print(models[["lda"]])