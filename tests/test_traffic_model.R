# ==============================================================================
# UrbanPulse — tests/test_traffic_model.R
# Unit Tests for Forecasting & Classification Metrics
# ==============================================================================

library(testthat)
root_dir <- if (dir.exists("R")) getwd() else if (dir.exists("../R")) dirname(getwd()) else getwd()
source(file.path(root_dir, "R", "helpers.R"))
source(file.path(root_dir, "R", "traffic_model.R"))

test_that("chronological_split strictly partitions without overlap", {
  n <- 100
  splits <- chronological_split(n, train_prop = 0.70, val_prop = 0.15)
  expect_equal(length(splits), 100)
  expect_equal(as.character(splits[1:70]), rep("train", 70))
  expect_equal(as.character(splits[71:85]), rep("val", 15))
  expect_equal(as.character(splits[86:100]), rep("test", 15))
})

test_that("compute_regression_metrics returns valid MAE, RMSE, and R2", {
  actual <- c(10, 20, 30, 40, 50)
  predicted <- c(12, 18, 33, 38, 52)
  
  metrics <- compute_regression_metrics(actual, predicted)
  expect_true(all(c("MAE", "RMSE", "R2", "N") %in% names(metrics)))
  expect_equal(metrics["N"][[1]], 5)
  expect_lt(metrics["MAE"][[1]], 3.0)
  expect_gt(metrics["R2"][[1]], 0.90)
})

test_that("compute_classification_metrics computes accurate confusion matrix and F1", {
  actual <- c(1, 1, 0, 0, 1, 0)
  probs  <- c(0.9, 0.8, 0.2, 0.1, 0.4, 0.3)
  
  res <- compute_classification_metrics(actual, probs, threshold = 0.5)
  expect_true("metrics" %in% names(res))
  expect_true("confusion_matrix" %in% names(res))
  expect_equal(res$metrics["Accuracy"][[1]], 5/6, tolerance = 0.01)
  expect_equal(res$metrics["Precision"][[1]], 1.0)
})
