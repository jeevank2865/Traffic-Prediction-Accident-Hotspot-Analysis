# ==============================================================================
# UrbanPulse — tests/test_data_cleaning.R
# Unit Tests for Data Cleaning & Helper Transformations
# ==============================================================================

library(testthat)
root_dir <- if (dir.exists("R")) getwd() else if (dir.exists("../R")) dirname(getwd()) else getwd()
source(file.path(root_dir, "R", "helpers.R"))
source(file.path(root_dir, "R", "data_cleaning.R"))

test_that("parse_clean_numeric handles edge cases and Indian formatting", {
  expect_equal(parse_clean_numeric("1,234"), 1234)
  expect_equal(parse_clean_numeric("3,72,181"), 372181)
  expect_equal(parse_clean_numeric("-18.6%"), -18.6)
  expect_true(is.na(parse_clean_numeric("NA")))
  expect_true(is.na(parse_clean_numeric("")))
  expect_true(is.na(parse_clean_numeric("-")))
  expect_equal(parse_clean_numeric(c("10", "20", "30")), c(10, 20, 30))
})

test_that("normalize_station_name standardizes prefixes and abbreviations", {
  expect_equal(normalize_station_name("Traffic Police Station, Adugodi"), "adugodi")
  expect_equal(normalize_station_name("Cubbon Park"), "cubbonpark")
  expect_equal(normalize_station_name("H.Grounds"), "high grounds")
  expect_equal(normalize_station_name("Ulsoor"), "halasooru")
  expect_equal(normalize_station_name("Halasur"), "halasooru")
  expect_equal(normalize_station_name("S.S.Nagar"), "sadashivanagar")
  expect_equal(normalize_station_name("W.Garden"), "wilson garden")
  expect_equal(normalize_station_name("U.Gate"), "upparpet")
})

test_that("format_interval_time accurately formats seconds from midnight", {
  expect_equal(format_interval_time(0), "00:00")
  expect_equal(format_interval_time(300), "00:05")
  expect_equal(format_interval_time(3600), "01:00")
  expect_equal(format_interval_time(43200), "12:00")
  expect_equal(format_interval_time(86100), "23:55")
})

test_that("Processed station panel has valid structure and non-negative counts", {
  panel_file <- file.path(root_dir, "data", "processed", "bengaluru_crashes_station_panel.csv")
  skip_if_not(file.exists(panel_file), "Processed crashes panel does not exist")
  
  df <- read.csv(panel_file)
  expect_gt(nrow(df), 50)
  expect_true(all(c("year", "station_raw", "total_crashes", "fatal_crashes") %in% names(df)))
  expect_true(all(df$total_crashes >= 0, na.rm = TRUE))
  expect_true(all(df$fatal_crashes >= 0, na.rm = TRUE))
  # Verify summary total rows were excluded
  expect_false(any(grepl("^total$", df$station_raw, ignore.case = TRUE)))
})
