# ==============================================================================
# UrbanPulse — tests/test_accident_analysis.R
# Unit Tests for Hotspot Identification & Spatial Geometry
# ==============================================================================

library(testthat)
root_dir <- if (dir.exists("R")) getwd() else if (dir.exists("../R")) dirname(getwd()) else getwd()
source(file.path(root_dir, "R", "helpers.R"))
source(file.path(root_dir, "R", "accident_analysis.R"))

test_that("rank_accident_hotspots accurately sorts and computes shares", {
  dummy_crashes <- data.frame(
    year = rep(2024, 4),
    station_raw = c("Station A", "Station B", "Station C", "Station D"),
    total_crashes = c(100, 50, 30, 20),
    fatal_crashes = c(20, 5, 3, 2),
    stringsAsFactors = FALSE
  )
  
  ranked <- rank_accident_hotspots(dummy_crashes, selected_year = 2024, top_n = 3)
  expect_equal(nrow(ranked), 3)
  expect_equal(ranked$station_raw[1], "Station A")
  expect_equal(ranked$share_of_city_pct[1], 50.0) # 100 / 200
  expect_equal(ranked$cumulative_share_pct[2], 75.0) # 150 / 200
})

test_that("compute_haversine_dist_matrix calculates valid non-negative distances", {
  # Vidhana Soudha to Kempegowda Bus Station (approx 2.5 km)
  lats <- c(12.9797, 12.9778)
  lons <- c(77.5907, 77.5713)
  
  dist_mat <- compute_haversine_dist_matrix(lats, lons)
  expect_equal(dim(dist_mat), c(2, 2))
  expect_equal(dist_mat[1, 1], 0)
  expect_equal(dist_mat[2, 2], 0)
  expect_gt(dist_mat[1, 2], 1.5)
  expect_lt(dist_mat[1, 2], 3.5)
})

test_that("KML Parser extracts valid Bengaluru coordinates", {
  kml_file <- file.path(root_dir, "data", "raw", "bengaluru_traffic_police_stations.kml")
  skip_if_not(file.exists(kml_file), "KML file not present")
  
  geo_df <- parse_btp_kml(kml_file)
  expect_gt(nrow(geo_df), 30)
  valid_coords <- geo_df[geo_df$coord_valid == TRUE, ]
  expect_gt(nrow(valid_coords), 30)
  # All valid lats must be in Bengaluru bounding box
  expect_true(all(valid_coords$latitude >= 12.7 & valid_coords$latitude <= 13.3))
  expect_true(all(valid_coords$longitude >= 77.3 & valid_coords$longitude <= 77.9))
})
