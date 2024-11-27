library(dplyr)
library(yardstick)

get_pred_data <- function(model, data, response_var) {
  if (!response_var %in% names(data)) {
    stop(sprintf("Response variable '%s' not found in data", response_var))
  }
  
  pred <- predict(model, data)
  actual <- data[[response_var]]
  
  tibble(predicted = pred, actual = actual)
}

get_model_metrics <- function(pred_data_list) {
  lapply(names(pred_data_list), function(model_name) {
    data <- pred_data_list[[model_name]]
    
    metrics <- metric_set(
      yardstick::accuracy,
      yardstick::sens,
      yardstick::spec,
      yardstick::precision,
      yardstick::f_meas
    )
    
    metrics(data, truth = actual, estimate = predicted) %>%
      mutate(model = model_name)
  }) %>%
    bind_rows() %>%
    pivot_wider(names_from = .metric, values_from = .estimate)
}