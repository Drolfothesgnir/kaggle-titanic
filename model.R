library(caret)
library(tidyverse)
source("prepare_data.R")

# Load and prepare data once
df <- prepare_data() %>%
  mutate(Survived = factor(
    Survived,
    levels = c(0, 1),
    labels = c("No", "Yes")
  ))

# Set up cross-validation control once
set.seed(1)
train_control <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,
  # Get probability predictions
  savePredictions = "final",
  # Save predictions for later analysis
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
models[["lda"]] <- train(model_formula,
                         data = df,
                         method = "lda",
                         trControl = train_control)

# Random Forest
models[["rf"]] <- train(
  model_formula,
  data = df,
  method = "rf",
  trControl = train_control,
  tuneGrid = expand.grid(mtry = 1:4),
  importance = TRUE
)

# Add GBM
models[["gbm"]] <- train(
  model_formula,
  data = df,
  method = "gbm",
  trControl = train_control,
  tuneGrid = expand.grid(
    n.trees = c(100, 200),
    interaction.depth = c(3, 5),
    shrinkage = c(0.1, 0.01),
    n.minobsinnode = 10
  ),
  verbose = FALSE
)

# Add XGBoost with focused tuning
models[["xgb"]] <- train(
  model_formula,
  data = df,
  method = "xgbTree",
  trControl = train_control,
  tuneGrid = expand.grid(
    nrounds = c(100, 200),
    max_depth = c(3, 6),
    eta = c(0.1, 0.3),
    gamma = 0,
    colsample_bytree = 0.8,
    min_child_weight = 1,
    subsample = 0.8
  ),
  verbosity = 0
)

# Compare results
results <- resamples(models)
summary(results)

# Visualization of comparison
bwplot(results)

# Detailed model summaries
sapply(models, print)

varImp(models[["rf"]])
plot(varImp(models[["rf"]]))

source("utils/get_pred_data.R")


pred_data <- lapply(names(models), function(model_name) {
  get_pred_data(models[[model_name]], df, "Survived")
})

names(pred_data) <- names(models)

get_model_metrics(pred_data)
