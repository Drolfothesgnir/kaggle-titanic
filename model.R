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
  savePredictions = "final"
  # Save predictions for later analysis
)

# Create formula once
model_formula <- Survived ~ Age + Pclass + title_clean + family_size


model_specs <- list(
  logistic = list(method = "glm", family = "binomial"),
  lda = list(method = "lda"),
  rf = list(
    method = "rf",
    tuneGrid = expand.grid(mtry = 1:4),
    importance = TRUE
  ),
  gbm = list(
    method="gbm",
    tuneGrid = expand.grid(
      n.trees = c(100, 200),
      interaction.depth = c(3, 5),
      shrinkage = c(0.1, 0.01),
      n.minobsinnode = 10
    ),
    verbose = FALSE
  ),
  xgb = list(
    method = "xgbTree",
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
)

# Train multiple models
models <- map(names(model_specs), function(model_name) {
  specs <- model_specs[[model_name]]
  do.call(train, c(
    list(
      form = model_formula,
      data = df,
      trControl = train_control
    ),
    specs
  ))
})

names(models) <- names(model_specs)

# Compare results
results <- resamples(models)
summary(results)

# Visualization of comparison
bwplot(results)

# Detailed model summaries
walk(names(models), ~{
  cat("\nModel: ", .x, "\n")
  print(models[[.x]])
})

varImp(models[["rf"]])
plot(varImp(models[["rf"]]))

source("utils/get_pred_data.R")

pred_data <- map(models, ~get_pred_data(.x, df, "Survived"))

names(pred_data) <- names(models)

get_model_metrics(pred_data)
