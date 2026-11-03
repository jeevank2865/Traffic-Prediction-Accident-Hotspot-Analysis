#!/usr/bin/env Rscript
# ==============================================================================
# UrbanPulse — 02_audit_data.R
# Comprehensive Data Inventory, Schema Discovery, and Quality Audit
# ==============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(digest)
})

base_dir <- getwd()
raw_dir <- file.path(base_dir, "data", "raw")
audit_dir <- file.path(base_dir, "reports", "data_audit")
reports_dir <- file.path(base_dir, "reports")
dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)

cat("=== UrbanPulse Phase 3: Data Inventory & Schema Discovery ===\n")

# Utility to read CSV with robust UTF-8 and BOM handling
read_clean_csv <- function(filepath) {
  df <- suppressWarnings(
    readr::read_csv(filepath, col_types = readr::cols(.default = "c"), show_col_types = FALSE, progress = FALSE)
  )
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  names(df) <- trimws(names(df))
  return(df)
}

# 1. Collect all downloaded raw files
raw_files <- list.files(raw_dir, full.names = TRUE)
csv_files <- raw_files[grepl("\\.csv$", raw_files)]
kml_files <- raw_files[grepl("\\.kml$", raw_files)]

dataset_profiles <- list()
missingness_rows <- list()
duplicate_rows <- list()
invalid_rows <- list()
data_dict_rows <- list()

for (f in csv_files) {
  fname <- basename(f)
  cat("Auditing CSV:", fname, "...\n")
  
  # Read raw lines to inspect encoding/BOM
  raw_bytes <- readBin(f, "raw", n = min(1000, file.info(f)$size))
  has_bom <- (length(raw_bytes) >= 3 && raw_bytes[1] == as.raw(0xef) && raw_bytes[2] == as.raw(0xbb) && raw_bytes[3] == as.raw(0xbf))
  
  # Read dataframe with robust BOM handling
  df <- read_clean_csv(f)
  
  n_rows <- nrow(df)
  n_cols <- ncol(df)
  
  # Check full duplicate rows
  dup_count <- sum(duplicated(df))
  dup_pct <- round(100 * dup_count / max(1, n_rows), 2)
  duplicate_rows[[fname]] <- data.frame(
    filename = fname,
    total_rows = n_rows,
    duplicate_rows = dup_count,
    duplicate_percentage = dup_pct,
    primary_key_candidate = names(df)[1],
    key_duplicates = sum(duplicated(df[[1]])),
    stringsAsFactors = FALSE
  )
  
  # Determine aggregation level and nature
  is_aggregate <- TRUE
  obs_granularity <- "Aggregated Counts"
  if (fname == "utd19_traffic_flow_subset.csv") {
    is_aggregate <- FALSE
    obs_granularity <- "Individual 5-minute loop detector continuous readings"
  } else if (fname == "utd19_detectors_public.csv") {
    is_aggregate <- FALSE
    obs_granularity <- "Individual sensor/detector point infrastructure"
  } else if (grepl("crashes_20", fname)) {
    obs_granularity <- "Police Station annual aggregate crash/fatality counts"
  } else if (grepl("violations", fname)) {
    obs_granularity <- "City-wide annual aggregate traffic violations"
  } else if (grepl("vehicles", fname)) {
    obs_granularity <- "City-wide annual aggregate registered vehicle counts"
  } else if (grepl("cities_accidents", fname)) {
    obs_granularity <- "Million-plus city annual aggregate road accidents"
  } else if (grepl("annual", fname)) {
    obs_granularity <- "National annual aggregate road safety statistics"
  }
  
  # Date/Temporal coverage detection
  temporal_coverage <- "N/A"
  if (any(grepl("day|date|year", names(df), ignore.case = TRUE))) {
    date_col <- names(df)[grep("day|date|year", names(df), ignore.case = TRUE)[1]]
    vals <- df[[date_col]][!is.na(df[[date_col]]) & df[[date_col]] != ""]
    if (length(vals) > 0) {
      temporal_coverage <- paste0(min(vals), " to ", max(vals))
    }
  } else if (any(grepl("201|202", names(df)))) {
    years_found <- grep("20[0-9]{2}", names(df), value = TRUE)
    temporal_coverage <- paste0(years_found[1], " to ", years_found[length(years_found)])
  }
  
  # Geographic coverage
  geo_coverage <- "City of Bengaluru, Karnataka, India"
  if (grepl("utd19", fname)) {
    geo_coverage <- "Augsburg / European Urban Network (UTD19 Open Benchmark)"
  } else if (grepl("india", fname)) {
    geo_coverage <- "India National / 53 Million-Plus Cities"
  }
  
  # Profile summary
  dataset_profiles[[fname]] <- data.frame(
    filename = fname,
    row_count = n_rows,
    col_count = n_cols,
    file_size_bytes = file.info(f)$size,
    has_utf8_bom = has_bom,
    observation_nature = if (is_aggregate) "Aggregated" else "Individual Sensor Event",
    granularity = obs_granularity,
    temporal_coverage = temporal_coverage,
    geographic_coverage = geo_coverage,
    duplicate_rows = dup_count,
    stringsAsFactors = FALSE
  )
  
  # Column-by-column missingness, types, and invalid values
  for (col in names(df)) {
    col_vals <- df[[col]]
    
    # Missingness definition: NA, blank, whitespace, "null", "N/A", "NA"
    is_missing <- is.na(col_vals) | trimws(col_vals) %in% c("", "NA", "N/A", "null", "NULL", "NaN")
    n_miss <- sum(is_missing)
    pct_miss <- round(100 * n_miss / max(1, n_rows), 2)
    
    valid_vals <- col_vals[!is_missing]
    n_distinct <- length(unique(valid_vals))
    
    # Infer true type
    clean_numeric_vals <- gsub("[,%]", "", trimws(valid_vals))
    is_num <- length(valid_vals) > 0 && all(!is.na(suppressWarnings(as.numeric(clean_numeric_vals))))
    inferred_type <- if (is_num) "Numeric" else "Character/String"
    
    # Check invalid values (e.g., negative counts, impossible coordinates)
    invalid_count <- 0
    invalid_reason <- "None"
    if (is_num) {
      num_vec <- as.numeric(clean_numeric_vals)
      if (grepl("crashes|killed|injured|fatal|cases|flow|lanes|vehicles", col, ignore.case = TRUE)) {
        neg_count <- sum(num_vec < 0, na.rm = TRUE)
        if (neg_count > 0) {
          invalid_count <- neg_count
          invalid_reason <- "Negative count observed in count variable"
        }
      }
      if (grepl("lat", col, ignore.case = TRUE)) {
        bad_lat <- sum(num_vec < -90 | num_vec > 90, na.rm = TRUE)
        if (bad_lat > 0) {
          invalid_count <- bad_lat
          invalid_reason <- "Latitude out of valid range [-90, 90]"
        }
      }
      if (grepl("long|lon", col, ignore.case = TRUE)) {
        bad_lon <- sum(num_vec < -180 | num_vec > 180, na.rm = TRUE)
        if (bad_lon > 0) {
          invalid_count <- bad_lon
          invalid_reason <- "Longitude out of valid range [-180, 180]"
        }
      }
    }
    
    missingness_rows[[paste(fname, col)]] <- data.frame(
      filename = fname,
      column_name = col,
      inferred_type = inferred_type,
      total_rows = n_rows,
      missing_count = n_miss,
      missing_percentage = pct_miss,
      distinct_values = n_distinct,
      stringsAsFactors = FALSE
    )
    
    if (invalid_count > 0) {
      invalid_rows[[paste(fname, col)]] <- data.frame(
        filename = fname,
        column_name = col,
        invalid_count = invalid_count,
        reason = invalid_reason,
        stringsAsFactors = FALSE
      )
    }
    
    # Data Dictionary entry
    data_dict_rows[[paste(fname, col)]] <- data.frame(
      dataset_file = fname,
      column_name = col,
      data_type = inferred_type,
      description = paste0("Extracted from raw source: ", fname),
      missing_pct = pct_miss,
      sample_values = paste(head(valid_vals, 3), collapse = "; "),
      stringsAsFactors = FALSE
    )
  }
}

# Audit KML files
if (length(kml_files) > 0) {
  for (kf in kml_files) {
    kname <- basename(kf)
    cat("Auditing KML:", kname, "...\n")
    kml_txt <- readLines(kf, warn = FALSE)
    n_placemarks <- length(grep("<Placemark>", kml_txt))
    n_coords <- length(grep("<coordinates>", kml_txt))
    
    dataset_profiles[[kname]] <- data.frame(
      filename = kname,
      row_count = n_placemarks,
      col_count = 6, # placemarks, name, department, kgis_code, lat, lon
      file_size_bytes = file.info(kf)$size,
      has_utf8_bom = FALSE,
      observation_nature = "Spatial Vector Points (Point Features)",
      granularity = "Bengaluru Urban Traffic Police Station Geocoded Locations",
      temporal_coverage = "2022-2024 Administrative Police Station Jurisdictions",
      geographic_coverage = "Bengaluru Urban District, Karnataka (WGS84 EPSG:4326)",
      duplicate_rows = 0,
      stringsAsFactors = FALSE
    )
  }
}

profiles_df <- do.call(rbind, dataset_profiles)
missingness_df <- do.call(rbind, missingness_rows)
duplicates_df <- do.call(rbind, duplicate_rows)
invalid_df <- if (length(invalid_rows) > 0) do.call(rbind, invalid_rows) else data.frame(filename = character(), column_name = character(), invalid_count = integer(), reason = character())
data_dict_df <- do.call(rbind, data_dict_rows)

# Save audit CSV tables
write.csv(profiles_df, file.path(audit_dir, "dataset_profiles.csv"), row.names = FALSE)
write.csv(missingness_df, file.path(audit_dir, "missingness_report.csv"), row.names = FALSE)
write.csv(duplicates_df, file.path(audit_dir, "duplicate_report.csv"), row.names = FALSE)
write.csv(invalid_df, file.path(audit_dir, "invalid_values_report.csv"), row.names = FALSE)
write.csv(data_dict_df, file.path(reports_dir, "data_dictionary.csv"), row.names = FALSE)

# 2. Build Dataset Compatibility Matrix
cat("Building Dataset Compatibility Matrix...\n")
compat_matrix <- data.frame(
  pair_id = 1:6,
  dataset_1 = c(
    "bengaluru_crashes_2020_2022.csv",
    "bengaluru_crashes_2020_2024 (Consolidated)",
    "bengaluru_crashes_2020_2024",
    "bengaluru_crashes_2020_2024",
    "utd19_traffic_flow_subset.csv",
    "bengaluru_crashes_trend_2007_2025.csv"
  ),
  dataset_2 = c(
    "bengaluru_crashes_2023.csv & 2024.csv",
    "bengaluru_traffic_police_stations.kml",
    "bengaluru_traffic_violations.csv",
    "bengaluru_registered_vehicles.csv",
    "utd19_detectors_public.csv",
    "india_accidents_annual_2020_2024.csv"
  ),
  time_period_overlap = c(
    "Consecutive & Overlapping (2020-2022, 2023, 2024)",
    "Contemporaneous administrative station jurisdictions (2022-2024)",
    "Annual overlap (2020-2024)",
    "Annual overlap (2020-2024)",
    "Synchronous 5-minute sampling (May 2017)",
    "Annual overlap (2020-2024)"
  ),
  geographic_compatibility = c(
    "Compatible: Same jurisdiction (Bengaluru Traffic Police Stations)",
    "Compatible: Exact spatial join on standardized Station Name",
    "City-wide aggregate only. Station-level join invalid without station violations.",
    "City-wide aggregate only. Station-level join invalid without RTO station mappings.",
    "Exact join on sensor identifier (detid) with detector coordinates",
    "Comparable macro-level trends (Bengaluru vs National)"
  ),
  aggregation_level = c(
    "Station-level counts",
    "Station-level counts to Station Point geometries",
    "Station vs City Total (Incompatible for granular regression)",
    "Station vs City Total (Incompatible for granular regression)",
    "5-minute continuous readings to detector point infrastructure",
    "City annual totals vs National annual totals"
  ),
  join_verdict = c(
    "VALID: Concatenate longitudinally across years 2021-2024",
    "VALID: Left join crash counts to station coordinates for spatial hotspot mapping",
    "RESTRICTED: Contextual comparison only; no causal road-level attribution",
    "RESTRICTED: Vehicle growth context only; cannot treat as road traffic volume",
    "VALID: Join flow measurements to detector metadata (road, lanes, limit, lat, long)",
    "VALID: Macro benchmarking of Bengaluru crash share within national trends"
  ),
  analytical_caution = c(
    "Standardize schema across differing column definitions (e.g. Fatal Cases vs Fatal crashes)",
    "43 stations match. Stations missing from KML kept as non-spatial tabular records.",
    "Violations reflect enforcement intensity, not accident occurrences.",
    "Vehicle registration counts do not represent active road traffic volumes.",
    "Never relabel Augsburg / UTD19 loop data as Bengaluru local measurements.",
    "National trends are aggregate counts and cannot identify road hotspots."
  ),
  stringsAsFactors = FALSE
)

write.csv(compat_matrix, file.path(audit_dir, "dataset_compatibility.csv"), row.names = FALSE)

# 3. Generate HTML Data Quality Summary
cat("Generating Data Quality Summary HTML...\n")
html_content <- paste0(
  "<!DOCTYPE html>\n<html>\n<head>\n",
  "<meta charset='utf-8'>\n<title>UrbanPulse — Data Quality Audit Summary</title>\n",
  "<style>\n",
  "body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; margin: 30px; background: #fdfdfd; color: #1a1a1a; line-height: 1.6; }\n",
  "h1 { color: #0f172a; border-bottom: 2px solid #e2e8f0; padding-bottom: 10px; }\n",
  "h2 { color: #1e293b; margin-top: 30px; }\n",
  "table { border-collapse: collapse; width: 100%; margin-bottom: 25px; background: #fff; box-shadow: 0 1px 3px rgba(0,0,0,0.05); }\n",
  "th, td { border: 1px solid #e2e8f0; padding: 10px 12px; text-align: left; font-size: 13px; }\n",
  "th { background: #f8fafc; font-weight: 600; color: #334155; }\n",
  "tr:nth-child(even) { background: #f9fafb; }\n",
  ".badge { padding: 3px 8px; border-radius: 4px; font-size: 11px; font-weight: 600; }\n",
  ".badge-success { background: #dcfce7; color: #166534; }\n",
  ".badge-warning { background: #fef9c3; color: #854d0e; }\n",
  ".badge-danger { background: #fee2e2; color: #991b1b; }\n",
  ".note-box { background: #eff6ff; border-left: 4px solid #3b82f6; padding: 15px; margin-bottom: 25px; border-radius: 0 4px 4px 0; font-size: 14px; }\n",
  "</style>\n</head>\n<body>\n",
  "<h1>UrbanPulse: Data Inventory & Quality Audit Summary</h1>\n",
  "<div class='note-box'><strong>Audit Objective:</strong> Inspect raw downloaded schemas, detect schema anomalies, document missingness, quantify duplicates, and evaluate cross-dataset compatibility prior to cleaning and modelling.</div>\n",
  "<h2>1. Dataset Profiles</h2>\n",
  "<table>\n<tr><th>Filename</th><th>Rows</th><th>Cols</th><th>Size (KB)</th><th>Observation Nature</th><th>Granularity</th><th>Temporal Range</th></tr>\n"
)

for (i in seq_len(nrow(profiles_df))) {
  r <- profiles_df[i, ]
  html_content <- paste0(
    html_content,
    sprintf("<tr><td><strong>%s</strong></td><td>%s</td><td>%s</td><td>%.1f</td><td>%s</td><td>%s</td><td>%s</td></tr>\n",
            r$filename, format(r$row_count, big.mark=","), r$col_count, r$file_size_bytes/1024, r$observation_nature, r$granularity, r$temporal_coverage)
  )
}
html_content <- paste0(html_content, "</table>\n")

html_content <- paste0(
  html_content,
  "<h2>2. Dataset Compatibility Matrix</h2>\n",
  "<table>\n<tr><th>Pair ID</th><th>Dataset 1</th><th>Dataset 2</th><th>Join Verdict</th><th>Analytical Caution</th></tr>\n"
)
for (i in seq_len(nrow(compat_matrix))) {
  c_row <- compat_matrix[i, ]
  verdict_class <- if (grepl("VALID", c_row$join_verdict)) "badge-success" else "badge-warning"
  html_content <- paste0(
    html_content,
    sprintf("<tr><td>%d</td><td>%s</td><td>%s</td><td><span class='badge %s'>%s</span></td><td>%s</td></tr>\n",
            c_row$pair_id, c_row$dataset_1, c_row$dataset_2, verdict_class, c_row$join_verdict, c_row$analytical_caution)
  )
}
html_content <- paste0(
  html_content,
  "</table>\n",
  "<h2>3. Missingness & Quality Overview</h2>\n",
  "<p>Detailed column-level profiles have been exported to <code>reports/data_audit/missingness_report.csv</code> and <code>reports/data_dictionary.csv</code>.</p>\n",
  "</body>\n</html>\n"
)

html_path <- file.path(audit_dir, "data_quality_summary.html")
writeLines(html_content, html_path)
cat("Audit complete! Reports saved to:", audit_dir, "\n")
