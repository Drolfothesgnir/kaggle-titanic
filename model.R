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
model_formula <- Survived ~ Age + Pclass + title_clean_alt + family_size


model_specs <- list(
  logistic = list(method = "glm", family = "binomial"),
  # lda = list(method = "lda"),
  rf = list(
    method = "rf",
    tuneGrid = expand.grid(mtry = 1:4),
    importance = TRUE
  ),
  svm_linear = list(
    method = "svmLinear",
    tuneGrid = expand.grid(
      C = c(0.1, 0.5, 1, 2, 5)  # Cost parameter
    ),
    preProcess = c("center", "scale")
  ),
  svm_radial_fine = list(
    method = "svmRadial",
    tuneGrid = expand.grid(
      sigma = seq(0.03, 0.07, by = 0.01),  # Finer grid around 0.05
      C = c(0.3, 0.4, 0.5, 0.6, 0.7)       # Finer grid around 0.5
    ),
    preProcess = c("center", "scale")
  ),
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
      nrounds = c(50, 75, 100, 125),        # Around 100
      max_depth = c(2, 3, 4),               # Around 3
      eta = c(0.2, 0.3, 0.4),              # Around 0.3
      gamma = c(0, 0.1),                   # Try light pruning
      colsample_bytree = c(0.7, 0.8, 0.9),  # Around 0.8
      min_child_weight = c(1, 2),          # Around 1
      subsample = c(0.7, 0.8, 0.9)         # Around 0.8
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
walk(names(models), ~ {
  cat("\nModel: ", .x, "\n")
  print(models[[.x]])
})

source("utils/get_pred_data.R")

# get predictions on the test set for every model
pred_data <- map(models, ~ get_pred_data(.x, df, "Survived"))

names(pred_data) <- names(models)

# get accuracy, sensitivity, specificity, precision and F of each model
get_model_metrics(pred_data)

rf_importance <- varImp(models[["rf"]])
print("Random Forest Feature Importance:")
print(rf_importance)
plot(varImp(models[["rf"]]), main = "Feature Importance For RF")

xgb_importance <- varImp(models[["xgb_stage_2"]])
print("\nXGBoost Feature Importance:")
print(xgb_importance)
plot(varImp(models[["xgb_stage_2"]]), main = "Feature Importance For XGB")

# For logistic regression - get coefficients
log_coef <- coef(models[["logistic"]]$finalModel)
print("\nLogistic Regression Coefficients:")
print(log_coef)
barplot(log_coef, main="Log. Reg. Coefficient Estimates")

disagreement_analysis <- tibble(
  actual = df$Survived,
  logistic = predict(models[["logistic"]], df),
  rf = predict(models[["rf"]], df),
  xgb = predict(models[["xgb_stage_2"]], df)
) %>%
  mutate(all_agree = (rf == logistic) & (rf == xgb)) %>%
  cbind(df[, c("Age", "Pclass", "title_clean_alt", "title_clean", "family_size")])

disagreement_analysis

disagreement_cases <- disagreement_analysis %>%
  filter(!all_agree)

disagreement_cases

#    actual logistic  rf xgb all_agree  Age Pclass title_clean_alt  title_clean family_size
# 1     Yes      Yes Yes  No     FALSE 26.0      3           Miss.        Miss.           0
# 2     Yes      Yes  No  No     FALSE 28.0      1             Mr.          Mr.           0
# 3      No       No Yes Yes     FALSE 18.0      3           Miss.        Miss.           2
# 4      No      Yes  No  No     FALSE 40.0      3            Mrs.         Mrs.           1
# 5      No      Yes  No  No     FALSE 28.0      1             Mr.          Mr.           0
# 6     Yes       No Yes Yes     FALSE 33.0      3            Mrs.         Mrs.           3
# 7      No      Yes Yes  No     FALSE 28.0      3           Miss.        Miss.           0
# 8      No      Yes  No Yes     FALSE 20.0      3           Miss.        Miss.           1
# 9     Yes       No Yes Yes     FALSE 18.0      3           Miss.        Miss.           2
# 10     No      Yes  No  No     FALSE 47.0      3            Mrs.         Mrs.           1
# 11     No      Yes  No  No     FALSE 24.0      1             Mr.          Mr.           0
# 12    Yes      Yes Yes  No     FALSE 24.0      3            Mrs.         Mrs.           1
# 13     No      Yes Yes  No     FALSE 25.0      3           Miss.        Miss.           0
# 14     No      Yes Yes  No     FALSE 29.0      3            Mrs.         Mrs.           2
# 15     No      Yes  No  No     FALSE 41.0      3            Mrs.         Mrs.           2
# 16    Yes      Yes Yes  No     FALSE 29.0      3            Mrs.         Mrs.           2
# 17     No      Yes Yes  No     FALSE 24.0      3           Miss.        Miss.           0
# 18    Yes      Yes Yes  No     FALSE 26.0      3           Miss.        Miss.           0
# 19    Yes       No Yes Yes     FALSE 18.0      3           Miss.        Miss.           2
# 20     No      Yes  No  No     FALSE 45.0      3            Mrs.         Mrs.           1
# 21     No      Yes  No  No     FALSE 22.0      1             Mr.          Mr.           0
# 22     No       No  No Yes     FALSE 27.0      1             Mr.          Mr.           2
# 23    Yes       No  No Yes     FALSE 36.0      1             Mr.          Mr.           3
# 24     No      Yes  No Yes     FALSE 31.0      3           Miss.        Miss.           0
# 25     No      Yes  No  No     FALSE 21.0      3           Miss.        Miss.           1
# 26     No      Yes  No Yes     FALSE 10.0      3           Miss.        Miss.           2
# 27     No      Yes Yes  No     FALSE 28.0      3            Mrs.         Mrs.           2
# 28    Yes      Yes  No  No     FALSE 28.0      1             Mr.          Mr.           0
# 29    Yes       No  No Yes     FALSE 34.0      1             Mr.          Mr.           0
# 30    Yes       No Yes Yes     FALSE  5.0      3           Miss.        Miss.           3
# 31     No      Yes  No  No     FALSE 30.0      1             Mr.          Mr.           0
# 32    Yes       No  No Yes     FALSE 49.0      1             Mr.          Mr.           1
# 33    Yes       No  No Yes     FALSE 48.0      1             Mr.          Mr.           0
# 34    Yes      Yes Yes  No     FALSE 63.0      3            Mrs.         Mrs.           0
# 35     No      Yes  No  No     FALSE 37.0      3           Miss.        Miss.           0
# 36     No      Yes  No  No     FALSE 18.0      1             Mr.          Mr.           1
# 37    Yes       No  No Yes     FALSE 36.0      1             Mr.          Mr.           0
# 38    Yes       No  No Yes     FALSE 17.0      1             Mr.          Mr.           2
# 39    Yes       No  No Yes     FALSE 36.0      1             Mr.          Mr.           0
# 40     No       No  No Yes     FALSE 36.0      1             Mr.          Mr.           0
# 41     No       No Yes Yes     FALSE 18.0      3           Miss.        Miss.           2
# 42    Yes       No  No Yes     FALSE 49.0      1             Mr.        Noble           1
# 43    Yes       No  No Yes     FALSE 35.0      1             Mr.          Mr.           0
# 44    Yes      Yes  No Yes     FALSE 27.0      1             Mr.          Mr.           0
# 45     No      Yes Yes  No     FALSE 26.0      3            Mrs.         Mrs.           1
# 46    Yes      Yes  No Yes     FALSE 32.0      1             Mr. Professional           0
# 47    Yes       No  No Yes     FALSE 48.0      1             Mr.          Mr.           1
# 48     No       No  No Yes     FALSE 31.0      1             Mr.          Mr.           1
# 49    Yes       No  No Yes     FALSE 36.0      1             Mr.          Mr.           1
# 50    Yes      Yes  No Yes     FALSE 27.0      1             Mr.          Mr.           0
# 51    Yes       No  No Yes     FALSE 31.0      1             Mr.          Mr.           1
# 52     No       No  No Yes     FALSE 49.0      1             Mr.          Mr.           2
# 53    Yes       No  No Yes     FALSE 35.0      1             Mr.          Mr.           0
# 54    Yes       No  No Yes     FALSE 48.0      1             Mr.          Mr.           1
# 55    Yes       No  No Yes     FALSE 27.0      1             Mr.          Mr.           1
# 56     No      Yes  No  No     FALSE 25.0      3           Miss.        Miss.           1
# 57    Yes       No  No Yes     FALSE 35.0      1             Mr.          Mr.           0
# 58     No       No  No Yes     FALSE 36.0      1             Mr.          Mr.           1
# 59     No      Yes  No  No     FALSE 19.0      1             Mr.          Mr.           1
# 60     No      Yes  No Yes     FALSE 30.5      3           Miss.        Miss.           0
# 61    Yes       No Yes Yes     FALSE 54.0      2            Mrs.         Mrs.           4
# 62     No      Yes  No  No     FALSE 29.0      1             Mr.          Mr.           0
# 63     No      Yes Yes  No     FALSE 30.0      3            Mrs.         Mrs.           2
# 64    Yes      Yes Yes  No     FALSE 24.0      3            Mrs.         Mrs.           3
# 65     No      Yes  No Yes     FALSE 31.0      1             Mr.          Mr.           0
# 66     No      Yes  No Yes     FALSE 33.0      1             Mr.          Mr.           0
# 67     No       No Yes  No     FALSE 18.0      3           Miss.        Miss.           3
# 68    Yes      Yes  No  No     FALSE 26.0      1             Mr.          Mr.           0

# Submission
test_df <- prepare_test_data()
pred <- predict(models[["rf"]], test_df)
pred <- ifelse(pred == "Yes", 1, 0)
submission_df <- test_df %>%
  select(PassengerId) %>%
  mutate(Survived = pred)

write.csv(submission_df, "submission.csv", row.names = FALSE)
  