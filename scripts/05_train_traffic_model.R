#!/usr/bin/env Rscript
# ==============================================================================
# UrbanPulse — 05_train_traffic_model.R
# Phase 7: Model Training with Strict Chronological Partitioning & Zero Leakage
# ==============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(randomForest)
})

base_dir <- getwd()
source(file.path(base_dir, "R", "helpers.R"))
source(file.path(base_dir, "R", "missing_data.R"))
source(file.path(base_dir, "R", "traffic_model.R"))

proc_dir <- file.path(base_dir, "data", "processed")
model_dir <- file.path(base_dir, "models")
dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)

cat("=== UrbanPulse Phase 7: Traffic Prediction Model Training ===\n")

# 1. Load preprocessed feature-engineered dataset
traffic_data <- suppressWarnings(read_csv(file.path(proc_dir, "utd19_traffic_features.csv"), show_col_types = FALSE))
cat(sprintf("Loaded traffic features: %d rows across %d detectors\n", nrow(traffic_data), length(unique(traffic_data$detid))))

# 2. Sort chronologically by datetime to preserve temporal causality
traffic_data <- traffic_data[order(traffic_data$datetime), ]

# 3. Create strict chronological partition (70% Train, 15% Val, 15% Test)
n <- nrow(traffic_data)
traffic_data$partition <- chronological_split(n, train_prop = 0.70, val_prop = 0.15)
cat("Chronological Partitions:\n")
print(table(traffic_data$partition))

train_raw <- traffic_data[traffic_data$partition == "train", ]
val_raw   <- traffic_data[traffic_data$partition == "val", ]
test_raw  <- traffic_data[traffic_data$partition == "test", ]

cat(sprintf("Train Period: %s to %s (N = %d)\n", min(train_raw$day), max(train_raw$day), nrow(train_raw)))
cat(sprintf("Val Period:   %s to %s (N = %d)\n", min(val_raw$day), max(val_raw$day), nrow(val_raw)))
cat(sprintf("Test Period:  %s to %s (N = %d)\n", min(test_raw$day), max(test_raw$day), nrow(test_raw)))

# 4. Fit training-set imputer (Strict Zero Data Leakage)
predictor_cols <- c("hour", "day_of_week", "is_weekend", "flow_lag_1", "flow_lag_2", "flow_roll_mean_3", "occ")
imputer <- fit_imputer(train_raw, num_cols = predictor_cols)

train_clean <- transform_imputer(imputer, train_raw)
val_clean   <- transform_imputer(imputer, val_raw)
test_clean  <- transform_imputer(imputer, test_raw)

# Save imputer and partition datasets for reproducible evaluation
saveRDS(imputer, file.path(model_dir, "traffic_imputer.rds"))
write.csv(train_clean, file.path(proc_dir, "traffic_train_clean.csv"), row.names = FALSE)
write.csv(val_clean,   file.path(proc_dir, "traffic_val_clean.csv"), row.names = FALSE)
write.csv(test_clean,  file.path(proc_dir, "traffic_test_clean.csv"), row.names = FALSE)

# ------------------------------------------------------------------------------
# 5. Fit Candidate Forecasting Models
# ------------------------------------------------------------------------------
regression_formula <- flow ~ hour + day_of_week + is_weekend + flow_lag_1 + flow_lag_2 + flow_roll_mean_3 + occ

cat("\n[1/3] Fitting Linear Regression Model...\n")
lm_model <- fit_linear_traffic_model(train_clean, regression_formula)
saveRDS(lm_model, file.path(model_dir, "model_linear_regression.rds"))
cat("Linear model fitted and saved.\n")

cat("\n[2/3] Fitting Random Forest Regression Model (ntree = 100)...\n")
# Sample 15,000 training records for fast convergence without losing representativeness
set.seed(42)
rf_sample_idx <- sample(seq_len(nrow(train_clean)), min(15000, nrow(train_clean)))
rf_model <- fit_rf_traffic_model(train_clean[rf_sample_idx, ], regression_formula, ntree = 100)
saveRDS(rf_model, file.path(model_dir, "model_random_forest.rds"))
cat("Random Forest model fitted and saved.\n")

cat("\n[3/3] Fitting Congestion Classification Model (Logistic Regression)...\n")
# Congestion target: is_congested (predicting congestion onset from temporal patterns and flow buildup)
classification_formula <- is_congested ~ hour + is_weekend + flow_lag_1 + flow_lag_2
cls_model <- fit_congestion_classifier(train_clean, classification_formula)
saveRDS(cls_model, file.path(model_dir, "model_congestion_classifier.rds"))
cat("Congestion classifier fitted and saved.\n")

# Save training metadata
metadata <- list(
  training_timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  total_observations = n,
  train_n = nrow(train_clean),
  val_n = nrow(val_clean),
  test_n = nrow(test_clean),
  prediction_target = "flow (vehicles per 5-minute interval)",
  congestion_target = "is_congested (occupancy >= 0.20 threshold)",
  features = predictor_cols,
  split_type = "Chronological (Zero Future Leakage)"
)
saveRDS(metadata, file.path(model_dir, "training_metadata.rds"))

cat("=== Traffic Model Training Completed Successfully ===\n")
