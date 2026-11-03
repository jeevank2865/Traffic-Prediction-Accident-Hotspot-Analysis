# ==============================================================================
# UrbanPulse — R/data_cleaning.R
# Reusable Preprocessing, Feature Engineering, and Transformation Pipelines
# ==============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(lubridate)
})

root_dir <- if (dir.exists("R")) getwd() else if (dir.exists("../R")) dirname(getwd()) else getwd()
source(file.path(root_dir, "R", "helpers.R"))
source(file.path(root_dir, "R", "missing_data.R"))

#' Clean and consolidate multi-year Bengaluru Police Station crash records
#' Standardizes schemas from 2021 through 2024, joins official KML coordinates
#' @return Cleaned consolidated dataframe with station coordinates and severity counts
clean_bengaluru_crashes <- function(c2022_path, c2023_path, c2024_path, kml_path) {
  # 1. Parse 2021-2022 records
  df_2022_raw <- suppressWarnings(read_csv(c2022_path, show_col_types = FALSE))
  names(df_2022_raw) <- trimws(names(df_2022_raw))
  
  # Remove total summary rows if present (e.g., Total, Grand Total, North Total)
  df_2022_raw <- df_2022_raw[!grepl("total", df_2022_raw$Station, ignore.case = TRUE), ]
  
  df_2021 <- data.frame(
    year = 2021L,
    zone = trimws(as.character(df_2022_raw$Zone)),
    sub_division = "Unknown",
    station_raw = trimws(as.character(df_2022_raw$Station)),
    station_norm = normalize_station_name(df_2022_raw$Station),
    fatal_crashes = parse_clean_numeric(df_2022_raw$`2021 - Fatal`),
    killed_people = parse_clean_numeric(df_2022_raw$`2021 - Killed`),
    non_fatal_crashes = parse_clean_numeric(df_2022_raw$`2021 - Non-Fatal`),
    injured_people = parse_clean_numeric(df_2022_raw$`2021 - Injured`),
    total_crashes = parse_clean_numeric(df_2022_raw$`2021 - Total Cases`),
    stringsAsFactors = FALSE
  )
  
  df_2022 <- data.frame(
    year = 2022L,
    zone = trimws(as.character(df_2022_raw$Zone)),
    sub_division = "Unknown",
    station_raw = trimws(as.character(df_2022_raw$Station)),
    station_norm = normalize_station_name(df_2022_raw$Station),
    fatal_crashes = parse_clean_numeric(df_2022_raw$`2022 - Fatal`),
    killed_people = parse_clean_numeric(df_2022_raw$`2022 - Killed`),
    non_fatal_crashes = parse_clean_numeric(df_2022_raw$`2022 - Non-Fatal`),
    injured_people = parse_clean_numeric(df_2022_raw$`2022 - Injured`),
    total_crashes = parse_clean_numeric(df_2022_raw$`2022 - Total Cases`),
    stringsAsFactors = FALSE
  )
  
  # 2. Parse 2023 records
  df_2023_raw <- suppressWarnings(read_csv(c2023_path, show_col_types = FALSE))
  names(df_2023_raw) <- trimws(names(df_2023_raw))
  df_2023_raw <- df_2023_raw[!grepl("total", df_2023_raw$Station, ignore.case = TRUE), ]
  
  df_2023 <- data.frame(
    year = 2023L,
    zone = trimws(as.character(df_2023_raw$Zone)),
    sub_division = trimws(as.character(df_2023_raw$`Sub-division`)),
    station_raw = trimws(as.character(df_2023_raw$Station)),
    station_norm = normalize_station_name(df_2023_raw$Station),
    fatal_crashes = parse_clean_numeric(df_2023_raw$`2023 - Fatal Cases`),
    killed_people = parse_clean_numeric(df_2023_raw$`2023 - Killed People`),
    non_fatal_crashes = parse_clean_numeric(df_2023_raw$`2023 - Non-Fatal`),
    injured_people = parse_clean_numeric(df_2023_raw$`2023 - Injured People`),
    total_crashes = parse_clean_numeric(df_2023_raw$`2023 - Total Cases`),
    stringsAsFactors = FALSE
  )
  
  # 3. Parse 2024 records
  df_2024_raw <- suppressWarnings(read_csv(c2024_path, show_col_types = FALSE))
  names(df_2024_raw) <- trimws(names(df_2024_raw))
  df_2024_raw <- df_2024_raw[!grepl("total", df_2024_raw$Station, ignore.case = TRUE), ]
  
  # 2024 reports non-fatal, fatal, total crashes (killed/injured not itemized in this report)
  df_2024 <- data.frame(
    year = 2024L,
    zone = trimws(as.character(df_2024_raw$Zone)),
    sub_division = trimws(as.character(df_2024_raw$`Sub-division`)),
    station_raw = trimws(as.character(df_2024_raw$Station)),
    station_norm = normalize_station_name(df_2024_raw$Station),
    fatal_crashes = parse_clean_numeric(df_2024_raw$`2024-Fatal crashes`),
    killed_people = NA_real_,
    non_fatal_crashes = parse_clean_numeric(df_2024_raw$`2024-Non Fatal`),
    injured_people = NA_real_,
    total_crashes = parse_clean_numeric(df_2024_raw$`2024-Total crashes`),
    stringsAsFactors = FALSE
  )
  
  # Combine across years
  combined_crashes <- rbind(df_2021, df_2022, df_2023, df_2024)
  
  # Validate consistency: fatal + non_fatal should equal total_crashes
  calc_total <- combined_crashes$fatal_crashes + combined_crashes$non_fatal_crashes
  mismatch <- which(!is.na(calc_total) & !is.na(combined_crashes$total_crashes) & calc_total != combined_crashes$total_crashes)
  if (length(mismatch) > 0) {
    # If discrepancy exists, use reported total_crashes as primary ground truth
    combined_crashes$total_mismatch_flag <- 0L
    combined_crashes$total_mismatch_flag[mismatch] <- 1L
  } else {
    combined_crashes$total_mismatch_flag <- 0L
  }
  
  # 4. Join official coordinates from KML
  btp_geo <- parse_btp_kml(kml_path)
  # Deduplicate geo by station_norm
  btp_geo_dedup <- btp_geo[!duplicated(btp_geo$station_norm), ]
  
  crashes_geo <- left_join(combined_crashes, btp_geo_dedup[, c("station_norm", "longitude", "latitude", "coord_valid")], by = "station_norm")
  crashes_geo$coord_valid[is.na(crashes_geo$coord_valid)] <- FALSE
  
  # Add severity ratio: fatal / total crashes
  crashes_geo$fatality_rate <- ifelse(crashes_geo$total_crashes > 0,
                                      round(crashes_geo$fatal_crashes / crashes_geo$total_crashes, 4),
                                      0)
  crashes_geo
}

#' Clean historical city-wide crash trend (2007-2025)
#' @return Cleaned dataframe with annual city totals
clean_bengaluru_crash_trend <- function(trend_path) {
  df <- suppressWarnings(read_csv(trend_path, show_col_types = FALSE))
  names(df) <- trimws(names(df))
  
  data.frame(
    year = as.integer(parse_clean_numeric(df$Year)),
    fatal_accidents = parse_clean_numeric(df$`Fatal Road Accidents`),
    killed = parse_clean_numeric(df$Killed),
    non_fatal_accidents = parse_clean_numeric(df$`Non-Fatal Road Accidents`),
    total_accidents = parse_clean_numeric(df$Total),
    fatality_rate = round(parse_clean_numeric(df$`Fatal Road Accidents`) / parse_clean_numeric(df$Total), 4),
    stringsAsFactors = FALSE
  )
}

#' Clean and pivot Bengaluru vehicle registrations
#' @return Long-format dataframe of vehicles by category, type, and year
clean_registered_vehicles <- function(veh_path) {
  df <- suppressWarnings(read_csv(veh_path, show_col_types = FALSE))
  names(df) <- trimws(names(df))
  
  # Identify year columns (e.g., 2017 to 2026)
  year_cols <- grep("^[0-9]{4}$", names(df), value = TRUE)
  
  # Clean category and type
  df$category <- trimws(as.character(df$Category))
  df$vehicle_type <- trimws(as.character(df$`Type of Vehicle`))
  
  long_df <- pivot_longer(
    df,
    cols = all_of(year_cols),
    names_to = "year_str",
    values_to = "registered_vehicles_raw"
  )
  
  data.frame(
    category = long_df$category,
    vehicle_type = long_df$vehicle_type,
    year = as.integer(long_df$year_str),
    registered_count = parse_clean_numeric(long_df$registered_vehicles_raw),
    stringsAsFactors = FALSE
  )
}

#' Clean traffic violations time-series
#' @return Standardized annual violations dataset
clean_traffic_violations <- function(viol_path) {
  df <- suppressWarnings(read_csv(viol_path, show_col_types = FALSE))
  names(df) <- trimws(names(df))
  
  data.frame(
    year = as.integer(parse_clean_numeric(df$YEAR)),
    mv_act_cases = parse_clean_numeric(df$`M.V.Act Cases`),
    mv_act_fine = parse_clean_numeric(df$`M.V.Act Fine`),
    kp_act_cases = parse_clean_numeric(df$`K.P.Act Cases`),
    kp_act_fine = parse_clean_numeric(df$`K.P.Act Fine`),
    towing_cases = parse_clean_numeric(df$`Towing Cases`),
    towing_charges = parse_clean_numeric(df$`Towing Charges`),
    automation_cases = parse_clean_numeric(df$`Automation cases`),
    automation_fine = parse_clean_numeric(df$`Automation Fine`),
    total_cases = parse_clean_numeric(df$`Total Cases Booked`),
    total_fine = parse_clean_numeric(df$`Total Fine Collected`),
    stringsAsFactors = FALSE
  )
}

#' Clean and feature-engineer UTD19 continuous traffic loop measurements
#' Computes temporal features, sensor metadata linkage, lags, and rolling stats
#' Strictly avoids future data leakage during feature generation
#' @return Feature-engineered traffic dataframe ready for modeling
clean_utd19_traffic <- function(flow_path, detectors_path, max_records = NULL) {
  # 1. Read detector metadata
  det_raw <- suppressWarnings(read_csv(detectors_path, col_types = cols(.default = "c"), show_col_types = FALSE))
  names(det_raw) <- trimws(names(det_raw))
  
  det_clean <- data.frame(
    detid = trimws(as.character(det_raw$detid)),
    road = trimws(as.character(det_raw$road)),
    fclass = trimws(as.character(det_raw$fclass)),
    speed_limit = parse_clean_numeric(det_raw$limit),
    lanes = parse_clean_numeric(det_raw$lanes),
    long = parse_clean_numeric(det_raw$long),
    lat = parse_clean_numeric(det_raw$lat),
    citycode = trimws(as.character(det_raw$citycode)),
    stringsAsFactors = FALSE
  )
  det_clean <- det_clean[!duplicated(det_clean$detid), ]
  
  # 2. Read flow records
  flow_raw <- suppressWarnings(read_csv(flow_path, col_types = cols(.default = "c"), n_max = if(is.null(max_records)) Inf else max_records, show_col_types = FALSE))
  names(flow_raw) <- trimws(names(flow_raw))
  
  # Filter valid error code (error == 1 or 0)
  err_num <- parse_clean_numeric(flow_raw$error)
  valid_mask <- !is.na(err_num) & err_num %in% c(0, 1)
  flow_valid <- flow_raw[valid_mask, ]
  
  flow_clean <- data.frame(
    day = trimws(as.character(flow_valid$day)),
    interval_sec = as.integer(parse_clean_numeric(flow_valid$interval)),
    detid = trimws(as.character(flow_valid$detid)),
    flow = parse_clean_numeric(flow_valid$flow),
    occ = parse_clean_numeric(flow_valid$occ),
    speed = parse_clean_numeric(flow_valid$speed),
    stringsAsFactors = FALSE
  )
  
  # Exclude negative flow values (invalid measurement artifacts)
  flow_clean <- flow_clean[!is.na(flow_clean$flow) & flow_clean$flow >= 0, ]
  
  # Exclude extreme unphysical flow outliers (> 3000 veh/hr/lane = > 250 veh/5min/lane)
  flow_clean <- flow_clean[flow_clean$flow <= 300, ]
  
  # Temporal feature generation
  time_str <- format_interval_time(flow_clean$interval_sec)
  datetime_str <- paste(flow_clean$day, time_str)
  flow_clean$datetime <- as.POSIXct(datetime_str, format = "%Y-%m-%d %H:%M", tz = "UTC")
  
  flow_clean$hour <- as.integer(floor(flow_clean$interval_sec / 3600))
  flow_clean$minute <- as.integer(floor((flow_clean$interval_sec %% 3600) / 60))
  flow_clean$day_of_week <- as.integer(format(flow_clean$datetime, "%w")) # 0 = Sunday, 6 = Saturday
  flow_clean$day_name <- factor(weekdays(flow_clean$datetime), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  flow_clean$is_weekend <- as.integer(flow_clean$day_of_week %in% c(0, 6))
  flow_clean$interval_id <- as.integer(flow_clean$interval_sec / 300) # 0 to 287
  
  # Peak period classification (defensible empirical thresholds)
  # Morning peak: 07:00 - 10:00 (hours 7, 8, 9)
  # Evening peak: 17:00 - 20:00 (hours 17, 18, 19)
  flow_clean$is_morning_peak <- as.integer(flow_clean$hour %in% 7:9 & flow_clean$is_weekend == 0)
  flow_clean$is_evening_peak <- as.integer(flow_clean$hour %in% 17:19 & flow_clean$is_weekend == 0)
  flow_clean$traffic_period <- ifelse(flow_clean$is_morning_peak == 1, "Morning Peak",
                                      ifelse(flow_clean$is_evening_peak == 1, "Evening Peak",
                                             ifelse(flow_clean$hour >= 22 | flow_clean$hour < 6, "Night Off-Peak", "Daytime Regular")))
  flow_clean$traffic_period <- factor(flow_clean$traffic_period, levels = c("Daytime Regular", "Morning Peak", "Evening Peak", "Night Off-Peak"))
  
  # Congestion Classification:
  # When occupancy > 0.20 (20% road time occupied), flow drops due to queue spillback (standard transportation engineering threshold)
  # Defensible empirical definition:
  flow_clean$is_congested <- as.integer(!is.na(flow_clean$occ) & flow_clean$occ >= 0.20)
  
  # Join detector metadata
  flow_enriched <- left_join(flow_clean, det_clean, by = "detid")
  
  # Sort strictly chronologically by detector and datetime to compute lag features without leakage
  flow_enriched <- flow_enriched[order(flow_enriched$detid, flow_enriched$datetime), ]
  
  # Group-wise lags and rolling statistics (avoiding future leakage)
  flow_enriched <- flow_enriched %>%
    group_by(detid) %>%
    mutate(
      flow_lag_1 = dplyr::lag(flow, 1), # 5-min lag
      flow_lag_2 = dplyr::lag(flow, 2), # 10-min lag
      flow_lag_12 = dplyr::lag(flow, 12), # 1-hour lag
      occ_lag_1 = dplyr::lag(occ, 1),
      # Rolling mean of past 3 intervals (excluding current to prevent leakage)
      flow_roll_mean_3 = (dplyr::lag(flow, 1) + dplyr::lag(flow, 2) + dplyr::lag(flow, 3)) / 3
    ) %>%
    ungroup()
  
  as.data.frame(flow_enriched)
}

#' Clean national and million-plus cities road accident statistics
#' @return Cleaned cities comparison dataframe
clean_national_road_safety <- function(annual_path, cities_path) {
  ann_df <- suppressWarnings(read_csv(annual_path, show_col_types = FALSE))
  names(ann_df) <- trimws(names(ann_df))
  
  cities_df <- suppressWarnings(read_csv(cities_path, show_col_types = FALSE))
  names(cities_df) <- trimws(names(cities_df))
  
  cities_clean <- data.frame(
    city = trimws(as.character(cities_df$City)),
    accidents_2023 = parse_clean_numeric(cities_df$`2023 Accidents`),
    accidents_2024 = parse_clean_numeric(cities_df$`2024 Accidents`),
    ranking_accidents_2024 = as.integer(parse_clean_numeric(cities_df$`2024 Ranking Accidents`)),
    killed_2023 = parse_clean_numeric(cities_df$`2023 Killed`),
    killed_2024 = parse_clean_numeric(cities_df$`2024 Killed`),
    ranking_killed_2024 = as.integer(parse_clean_numeric(cities_df$`2024 Ranking Killed`)),
    injured_2024 = parse_clean_numeric(cities_df$`2024 Injured`),
    stringsAsFactors = FALSE
  )
  
  # Remove summary total row if present
  cities_clean <- cities_clean[!grepl("^total", cities_clean$city, ignore.case = TRUE), ]
  
  # Compute fatality severity rate
  cities_clean$fatality_rate_2024 <- ifelse(cities_clean$accidents_2024 > 0,
                                            round(cities_clean$killed_2024 / cities_clean$accidents_2024, 4),
                                            0)
  cities_clean
}
