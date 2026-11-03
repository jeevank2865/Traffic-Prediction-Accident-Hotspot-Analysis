# ==============================================================================
# UrbanPulse — R/accident_analysis.R
# Spatial Hotspot Identification, Corridor Clustering, and Exposure Analysis
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(stats)
})

#' Rank Traffic Police Stations by Accident Frequencies and Severity
#' @param crashes_df Cleaned Bengaluru crashes station panel
#' @param selected_year Target year to rank (e.g., 2024)
#' @param top_n Number of top stations to return
#' @return Ranked dataframe of accident hotspots
rank_accident_hotspots <- function(crashes_df, selected_year = 2024, top_n = 15) {
  filtered <- crashes_df %>%
    filter(year == selected_year, !is.na(total_crashes), total_crashes > 0) %>%
    arrange(desc(total_crashes))
  
  if (nrow(filtered) == 0) return(data.frame())
  
  # Calculate cumulative share of city accidents
  total_city_crashes <- sum(filtered$total_crashes, na.rm = TRUE)
  filtered <- filtered %>%
    mutate(
      rank = row_number(),
      share_of_city_pct = round(100 * total_crashes / total_city_crashes, 2),
      cumulative_share_pct = round(100 * cumsum(total_crashes) / total_city_crashes, 2),
      fatality_percentage = round(100 * fatal_crashes / total_crashes, 2)
    )
  
  head(filtered, top_n)
}

#' Haversine Distance Matrix between Coordinate Pairs (in kilometers)
#' @param lats Numeric vector of latitudes
#' @param lons Numeric vector of longitudes
#' @return Symmetric distance matrix in kilometers
compute_haversine_dist_matrix <- function(lats, lons) {
  n <- length(lats)
  R <- 6371 # Earth radius in km
  d_mat <- matrix(0, nrow = n, ncol = n)
  
  rad_lats <- lats * pi / 180
  rad_lons <- lons * pi / 180
  
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {
        dlat <- rad_lats[j] - rad_lats[i]
        dlon <- rad_lons[j] - rad_lons[i]
        a <- sin(dlat / 2)^2 + cos(rad_lats[i]) * cos(rad_lats[j]) * sin(dlon / 2)^2
        c <- 2 * atan2(sqrt(a), sqrt(1 - a))
        d_mat[i, j] <- R * c
      }
    }
  }
  d_mat
}

#' Identify Spatial Corridors / Hotspot Clusters using Geocoded Police Stations
#' Groups high-crash stations within spatial proximity (e.g., 3.5 km corridor distance)
#' @param crashes_df Cleaned crashes panel with valid coordinates
#' @param selected_year Year to cluster
#' @param distance_threshold_km Spatial linkage distance (default 3.5 km)
#' @return Enriched dataframe with cluster IDs and corridor labels
spatial_cluster_stations <- function(crashes_df, selected_year = 2024, distance_threshold_km = 3.5) {
  spatial_df <- crashes_df %>%
    filter(year == selected_year, coord_valid == TRUE, !is.na(total_crashes))
  
  if (nrow(spatial_df) < 3) return(spatial_df)
  
  # Compute distance matrix
  dist_mat <- compute_haversine_dist_matrix(spatial_df$latitude, spatial_df$longitude)
  dist_obj <- stats::as.dist(dist_mat)
  
  # Hierarchical single-linkage clustering to detect elongated arterial corridors
  hc <- stats::hclust(dist_obj, method = "single")
  clusters <- stats::cutree(hc, h = distance_threshold_km)
  spatial_df$spatial_corridor_id <- paste("Corridor", clusters)
  
  # Summarize corridor metrics
  corridor_summary <- spatial_df %>%
    group_by(spatial_corridor_id) %>%
    summarize(
      station_count = n(),
      total_corridor_crashes = sum(total_crashes, na.rm = TRUE),
      total_corridor_fatalities = sum(fatal_crashes, na.rm = TRUE),
      corridor_stations = paste(station_raw, collapse = ", "),
      center_lat = mean(latitude),
      center_lon = mean(longitude),
      .groups = "drop"
    ) %>%
    arrange(desc(total_corridor_crashes))
  
  list(
    stations_clustered = spatial_df,
    corridor_summary = corridor_summary
  )
}

#' Macro Exposure Analysis: City Accident Rates per 10,000 Registered Vehicles
#' Documents why station-level exposure denominators are analytically restricted
#' @param crash_trend_df Annual crash trend dataframe
#' @param vehicles_df Annual registered vehicles dataframe
#' @return Merged longitudinal exposure dataset
compute_city_exposure_rates <- function(crash_trend_df, vehicles_df) {
  # Sum total registered vehicles by year across categories
  veh_totals <- vehicles_df %>%
    group_by(year) %>%
    summarize(total_registered_vehicles = sum(registered_count, na.rm = TRUE), .groups = "drop")
  
  merged <- inner_join(crash_trend_df, veh_totals, by = "year") %>%
    mutate(
      crashes_per_10k_vehicles = round(10000 * total_accidents / total_registered_vehicles, 2),
      fatalities_per_10k_vehicles = round(10000 * fatal_accidents / total_registered_vehicles, 2)
    )
  
  merged
}
