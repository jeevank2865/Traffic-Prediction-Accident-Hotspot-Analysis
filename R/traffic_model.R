# ==============================================================================
# UrbanPulse — R/traffic_model.R
# Machine Learning & Statistical Time-Series Forecasting Engines
# ==============================================================================

suppressPackageStartupMessages({
  library(randomForest)
  library(stats)
})

#' Compute Standard Regression Performance Metrics
#' Reports MAE, RMSE, and R-squared with limitation caveats
#' @param actual Numeric vector of ground-truth target values
#' @param predicted Numeric vector of model predictions
#' @return Named vector of metrics
compute_regression_metrics <- function(actual, predicted) {
  valid <- !is.na(actual) & !is.na(predicted)
  y <- actual[valid]
  y_hat <- predicted[valid]
  n <- length(y)
  
  if (n == 0) {
    return(c(MAE = NA, RMSE = NA, R2 = NA, N = 0))
  }
  
  mae <- mean(abs(y - y_hat))
  rmse <- sqrt(mean((y - y_hat)^2))
  
  # R2 computation with boundary protection
  ss_tot <- sum((y - mean(y))^2)
  ss_res <- sum((y - y_hat)^2)
  r2 <- if (ss_tot > 0) 1 - (ss_res / ss_tot) else 0
  
  c(
    MAE = round(mae, 3),
    RMSE = round(rmse, 3),
    R2 = round(r2, 4),
    N = n
  )
}

#' Compute Binary Classification Performance Metrics
#' Reports Accuracy, Precision, Recall, F1-Score, and Confusion Matrix
#' @param actual Binary vector (0 or 1)
#' @param predicted_prob Numeric vector of predicted probabilities [0, 1]
#' @param threshold Decision threshold (default 0.5)
#' @return List containing scalar metrics and confusion matrix
compute_classification_metrics <- function(actual, predicted_prob, threshold = 0.5) {
  valid <- !is.na(actual) & !is.na(predicted_prob)
  y <- as.integer(actual[valid])
  y_pred <- as.integer(predicted_prob[valid] >= threshold)
  
  tp <- sum(y == 1 & y_pred == 1)
  fp <- sum(y == 0 & y_pred == 1)
  tn <- sum(y == 0 & y_pred == 0)
  fn <- sum(y == 1 & y_pred == 0)
  
  accuracy <- (tp + tn) / max(1, (tp + tn + fp + fn))
  precision <- if ((tp + fp) > 0) tp / (tp + fp) else 0
  recall <- if ((tp + fn) > 0) tp / (tp + fn) else 0
  f1 <- if ((precision + recall) > 0) 2 * (precision * recall) / (precision + recall) else 0
  
  cm <- matrix(c(tn, fp, fn, tp), nrow = 2, byrow = TRUE,
               dimnames = list(Actual = c("Uncongested (0)", "Congested (1)"),
                               Predicted = c("Uncongested (0)", "Congested (1)")))
  
  list(
    metrics = c(
      Accuracy = round(accuracy, 4),
      Precision = round(precision, 4),
      Recall = round(recall, 4),
      F1_Score = round(f1, 4),
      Prevalence = round(mean(y), 4),
      N = length(y)
    ),
    confusion_matrix = cm
  )
}

#' Fit Persistence / Seasonal Naive Baseline Model
#' Predicts current step using 1-step lag (flow_lag_1)
#' @param test_df Testing dataframe containing flow and flow_lag_1
#' @return Vector of baseline predictions
predict_persistence_baseline <- function(test_df) {
  if ("flow_lag_1" %in% names(test_df)) {
    return(test_df$flow_lag_1)
  }
  stop("Persistence baseline requires 'flow_lag_1' column.")
}

#' Fit Linear Regression Forecasting Model
#' @param train_df Training dataframe
#' @param formula Model formula
#' @return Fitted lm object
fit_linear_traffic_model <- function(train_df, formula) {
  stats::lm(formula, data = train_df)
}

#' Fit Random Forest Regression Forecasting Model
#' @param train_df Training dataframe
#' @param formula Model formula
#' @param ntree Number of trees (default 100 for fast local execution)
#' @return Fitted randomForest object
fit_rf_traffic_model <- function(train_df, formula, ntree = 100) {
  set.seed(42)
  # Ensure complete cases for random forest
  model_vars <- all.vars(formula)
  train_clean <- stats::na.omit(train_df[, model_vars])
  
  randomForest::randomForest(
    formula,
    data = train_clean,
    ntree = ntree,
    importance = TRUE
  )
}

#' Fit Congestion Classification Model (Logistic Regression)
#' @param train_df Training dataframe
#' @param formula Model formula
#' @return Fitted glm object
fit_congestion_classifier <- function(train_df, formula) {
  stats::glm(formula, data = train_df, family = stats::binomial(link = "logit"))
}
