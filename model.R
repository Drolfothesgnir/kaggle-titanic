library(caret)
library(tidyverse)
source("prepare_data.R")

# Load and prepare data once
train_df <- prepare_train_data()
test_df <- prepare_test_data()


# Set up cross-validation control once
set.seed(7)
train_control <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,
  # Get probability predictions
  savePredictions = "final"
  # Save predictions for later analysis
)

# Create formula once
# Manav
# model_formula <- Survived ~ Pclass + Sex + Age + Fare + Embarked + title_clean_2 + is_alone + Age*Pclass

# Mine old
# model_formula <- Survived ~ Age + Pclass + title_clean_alt + family_size

# Current best
# model_formula <- Survived ~ Pclass + Age + fare_band + title_clean_alt + family_band

model_formula <- Survived ~ Pclass + Age + fare_band + title_clean_alt + family_band

model_specs <- list(
  logistic = list(method = "glm", family = "binomial"),
  # rf = list(
  #   method = "ranger",
  #   tuneGrid = expand.grid(
  #     mtry = 2:4,                     # Reduce from 5 to force more randomness
  #     splitrule = "extratrees",       # Keep extratrees since it worked well
  #     min.node.size = c(15, 20, 25)   # Increase min node size significantly
  #   ),
  #   importance = "permutation"
  # )
  # xgb_stage_1 = list(
  #   method = "xgbTree",
  #   tuneGrid = expand.grid(
  #     nrounds = c(100, 300), # 100 best
  #     # Wide range, few points
  #     max_depth = c(3, 6), # 3 best
  #     # Try shallow and deep
  #     eta = c(0.3, 0.1), # 0.3 best
  #     # Try faster and slower learning
  #     subsample = 0.8,
  #     # Keep other params fixed
  #     colsample_bytree = 0.8,
  #     min_child_weight = 1,
  #     gamma = 0
  #   ),
  #   verbosity = 0
  # )
  # ,
  xgb_stage_2 = list(
    method = "xgbTree",
    tuneGrid = expand.grid(
      nrounds = c(75, 100, 125),      # Center around 100 which was best
      max_depth = c(2, 3, 4),         # Center around 3 which was best
      eta = c(0.05, 0.1, 0.15),       # More granular around 0.1
      gamma = c(0, 0.05, 0.1),        # Try light regularization
      colsample_bytree = c(0.7, 0.8, 0.9),  # Vary feature sampling
      min_child_weight = c(1, 2),     # Try slightly higher min weight
      subsample = c(0.7, 0.8, 0.9)    # Vary row sampling
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
      data = train_df,
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

source("utils/get_pred_data.R")

# get predictions on the test set for every model
pred_data <- map(models, ~ get_pred_data(.x, train_df, "Survived"))

names(pred_data) <- names(models)

# get accuracy, sensitivity, specificity, precision and F of each model
get_model_metrics(pred_data)

# rf_importance <- varImp(models[["rf"]])
# print("Random Forest Feature Importance:")
# print(rf_importance)
# plot(varImp(models[["rf"]]), main = "Feature Importance For RF")
#
xgb_importance <- varImp(models[["xgb_stage_2"]])
print("\nXGBoost Feature Importance:")
print(xgb_importance)
plot(varImp(models[["xgb_stage_2"]]), main = "Feature Importance For XGB")
#
# # For logistic regression - get coefficients
# log_coef <- coef(models[["logistic"]]$finalModel)
# print("\nLogistic Regression Coefficients:")
# print(log_coef)
# barplot(log_coef, main="Log. Reg. Coefficient Estimates")
#
# disagreement_analysis <- tibble(
#   actual = df$Survived,
#   logistic = predict(models[["logistic"]], df),
#   rf = predict(models[["rf"]], df),
#   xgb = predict(models[["xgb_stage_2"]], df)
# ) %>%
#   mutate(all_agree = (rf == logistic) & (rf == xgb)) %>%
#   cbind(df[, c("Age", "Pclass", "title_clean_alt", "title_clean", "family_size")])
#
# disagreement_analysis
#
# disagreement_cases <- disagreement_analysis %>%
#   filter(!all_agree)
#
# disagreement_cases
#
# # Submission
# median_fare <- median(df$Fare, na.rm = TRUE)
# test_df <- prepare_test_data() %>%
#   replace_na(list(Fare = median_fare))
pred <- predict(models[["xgb_stage_2"]], test_df)
pred <- ifelse(pred == "Yes", 1, 0)
submission_df <- test_df %>%
  select(PassengerId) %>%
  mutate(Survived = pred)

write.csv(submission_df, "submission.csv", row.names = FALSE)
