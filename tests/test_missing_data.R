# ==============================================================================
# UrbanPulse — tests/test_missing_data.R
# Unit Tests for Missing-Data Protocol & Leakage-Free Imputer
# ==============================================================================

library(testthat)
root_dir <- if (dir.exists("R")) getwd() else if (dir.exists("../R")) dirname(getwd()) else getwd()
source(file.path(root_dir, "R", "helpers.R"))
source(file.path(root_dir, "R", "missing_data.R"))

test_that("audit_missingness identifies missing values and assigns rationales", {
  test_df <- data.frame(
    flow = c(10, 20, NA, 40),
    speed = c(50, NA, 45, NA),
    road = c("Arterial", "", "Secondary", "NA"),
    stringsAsFactors = FALSE
  )
  
  audit <- audit_missingness(test_df, dataset_name = "test", essential_cols = c("flow"))
  expect_equal(nrow(audit), 3)
  expect_equal(audit$missing_count[audit$column_name == "flow"], 1)
  expect_equal(audit$missing_count[audit$column_name == "speed"], 2)
  expect_equal(audit$missing_count[audit$column_name == "road"], 2)
  expect_true(grepl("Target", audit$proposed_treatment[audit$column_name == "flow"]))
})

test_that("fit_imputer and transform_imputer operate with zero data leakage", {
  train_df <- data.frame(
    feat1 = c(10, 20, 30, 40, NA), # median = 25
    cat1  = c("A", "A", "B", NA, "A"), # mode = A
    stringsAsFactors = FALSE
  )
  test_df <- data.frame(
    feat1 = c(NA, 100),
    cat1  = c(NA, "B"),
    stringsAsFactors = FALSE
  )
  
  imputer <- fit_imputer(train_df, num_cols = "feat1", cat_cols = "cat1")
  expect_equal(imputer$num_medians$feat1, 25)
  expect_equal(imputer$cat_modes$cat1, "A")
  
  transformed_test <- transform_imputer(imputer, test_df)
  # Ensure test missing value was filled with training median (25)
  expect_equal(transformed_test$feat1[1], 25)
  # Ensure indicator flag was added
  expect_equal(transformed_test$feat1_was_imputed, c(1, 0))
  expect_equal(transformed_test$cat1[1], "Unknown")
})
