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
set.seed(43)
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
  # lda = list(method = "lda"),
  rf = list(
    method = "rf",
    tuneGrid = expand.grid(mtry = 1:4),
    importance = TRUE
  ),
  # gbm = list(
  #   method="gbm",
  #   tuneGrid = expand.grid(
  #     n.trees = c(100, 200, 300),
  #     interaction.depth = c(3, 5),
  #     shrinkage = c(0.1, 0.05),
  #     n.minobsinnode = c(5, 10)
  #   ),
  #   verbose = FALSE
  # ),
  # xgb = list(
  #   method = "xgbTree",
  #   tuneGrid = expand.grid(
  #     nrounds = c(100, 200, 300),
  #     max_depth = c(3, 6),
  #     eta = c(0.1, 0.3),
  #     gamma = c(0,0.1),
  #     colsample_bytree = c(0.8, 1.0),
  #     min_child_weight = c(1,3),
  #     subsample = c(0.8, 1.0)
  #   ),
  #   verbosity = 0
  # )
  xgb_stage_1 = list(
    method = "xgbTree",
    tuneGrid = expand.grid(
      nrounds = c(100, 300), # 100 best
      # Wide range, few points
      max_depth = c(3, 6), # 3 best
      # Try shallow and deep
      eta = c(0.3, 0.1), # 0.3 best
      # Try faster and slower learning
      subsample = 0.8,
      # Keep other params fixed
      colsample_bytree = 0.8,
      min_child_weight = 1,
      gamma = 0
    ),
    verbosity = 0
  )
  # ,
  # xgb_stage_2 = list(
  #   method = "xgbTree",
  #   tuneGrid = expand.grid(
  #     nrounds = c(50, 100, 150),        # Around 100
  #     max_depth = c(2, 3, 4),           # Around 3
  #     eta = c(0.2, 0.3, 0.4),          # Around 0.3
  #     gamma = c(0, 0.1),               # Try light pruning
  #     colsample_bytree = c(0.7, 0.8, 0.9),  # Around 0.8
  #     min_child_weight = c(1, 2),      # Around 1
  #     subsample = c(0.7, 0.8, 0.9)     # Around 0.8
  #   ),
  #   verbosity = 0
  # )
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
walk(names(models), ~ {
  cat("\nModel: ", .x, "\n")
  print(models[[.x]])
})

# varImp(models[["rf"]])
# plot(varImp(models[["rf"]]))

source("utils/get_pred_data.R")

# get predictions on the test set for every model
pred_data <- map(models, ~ get_pred_data(.x, df, "Survived"))

names(pred_data) <- names(models)

# get accuracy, sensitivity, specificity, precision and F of each model
get_model_metrics(pred_data)
