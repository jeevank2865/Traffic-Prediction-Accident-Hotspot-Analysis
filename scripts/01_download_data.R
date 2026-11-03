#!/usr/bin/env Rscript
# ==============================================================================
# UrbanPulse — 01_download_data.R
# Automated Dataset Acquisition Pipeline
# ==============================================================================

suppressPackageStartupMessages({
  if (!requireNamespace("httr", quietly = TRUE)) install.packages("httr", repos = "https://cloud.r-project.org")
  if (!requireNamespace("digest", quietly = TRUE)) install.packages("digest", repos = "https://cloud.r-project.org")
  library(httr)
  library(digest)
})

# Define paths
base_dir <- getwd()
raw_dir <- file.path(base_dir, "data", "raw")
audit_dir <- file.path(base_dir, "reports", "data_audit")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)

log_file <- file.path(audit_dir, "download_log.txt")
log_conn <- file(log_file, open = "wt")

log_msg <- function(...) {
  msg <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", paste(...))
  cat(msg, "\n")
  cat(msg, "\n", file = log_conn)
}

log_msg("=== UrbanPulse Automated Data Acquisition Started ===")
log_msg("Project Directory:", base_dir)
log_msg("Raw Data Directory:", raw_dir)

# Check available disk space
df_out <- tryCatch(system("df -h .", intern = TRUE), error = function(e) "N/A")
log_msg("Disk Space Status:\n", paste(df_out, collapse = "\n"))

# Source Registry definition
source_registry <- data.frame(
  dataset_id = c(
    "dataset_a_crashes_2020_2022",
    "dataset_a_crashes_2023",
    "dataset_a_crashes_2024",
    "dataset_a_crashes_trend_2007_2025",
    "dataset_a_police_stations_kml",
    "dataset_b_violations",
    "dataset_c_registered_vehicles",
    "dataset_d_utd19_detectors",
    "dataset_d_utd19_flow_subset",
    "dataset_e_india_accidents_annual",
    "dataset_e_india_cities_accidents",
    "dataset_f_weather_openmeteo",
    "dataset_f_weather_imd"
  ),
  dataset_group = c(
    "Dataset A — Bengaluru Crashes",
    "Dataset A — Bengaluru Crashes",
    "Dataset A — Bengaluru Crashes",
    "Dataset A — Bengaluru Crashes",
    "Dataset A — Bengaluru Spatial (Stations)",
    "Dataset B — Traffic Violations",
    "Dataset C — Registered Vehicles",
    "Dataset D — UTD19 Traffic",
    "Dataset D — UTD19 Traffic",
    "Dataset E — National / City Road Safety",
    "Dataset E — National / City Road Safety",
    "Dataset F — Weather",
    "Dataset F — Weather"
  ),
  filename = c(
    "bengaluru_crashes_2020_2022.csv",
    "bengaluru_crashes_2023.csv",
    "bengaluru_crashes_2024.csv",
    "bengaluru_crashes_trend_2007_2025.csv",
    "bengaluru_traffic_police_stations.kml",
    "bengaluru_traffic_violations.csv",
    "bengaluru_registered_vehicles.csv",
    "utd19_detectors_public.csv",
    "utd19_traffic_flow_subset.csv",
    "india_accidents_annual_2020_2024.csv",
    "india_cities_accidents_2023_2024.csv",
    "bengaluru_weather_openmeteo.json",
    "bengaluru_weather_imd.txt"
  ),
  url = c(
    "https://newdata.opencity.in/dataset/94e986d6-7836-4a8e-aa3f-273bee4ea795/resource/b3744a95-e486-4022-9c20-ad178dcf23dd/download/492d3dc6-ffc3-4b0e-b7d9-176d0ef7f1ec.csv",
    "https://newdata.opencity.in/dataset/94e986d6-7836-4a8e-aa3f-273bee4ea795/resource/8f0f281c-2cb6-4491-ac76-d1874ce38583/download/abc5af52-08a7-4435-8ba1-12b99f62ee28.csv",
    "https://data.opencity.in/dataset/94e986d6-7836-4a8e-aa3f-273bee4ea795/resource/e59bc255-7b94-49df-b934-9b40fb2cc741/download/74e645e3-85d2-4d81-a133-4f346f87fdd6.csv",
    "https://data.opencity.in/dataset/94e986d6-7836-4a8e-aa3f-273bee4ea795/resource/aad98dbe-7451-4ab1-aa8a-8d5c1e46b9a7/download/a583a07c-731b-4e8d-b0cb-d06e76ccc00c.csv",
    "https://data.opencity.in/dataset/4ea93dd2-940d-4a21-ab1f-f248505ebd0e/resource/94ff1e77-8955-40b3-bcf5-57b956f107aa/download/5cebcbef-ef7b-433d-b5f4-1e0c2a78b9c4.kml",
    "https://data.opencity.in/dataset/7fe6e1c6-43c8-4e16-974b-099a21484c37/resource/169b0ea2-4092-4d78-b450-ae39ad2d8197/download/169b0ea2-4092-4d78-b450-ae39ad2d8197.csv",
    "https://data.opencity.in/dataset/474a6418-202e-484c-a159-829f4fabbe16/resource/89735a23-c64b-4d52-b1e8-3030a424c34e/download/89735a23-c64b-4d52-b1e8-3030a424c34e.csv",
    "https://www.research-collection.ethz.ch/server/api/core/bitstreams/c8b49420-4f45-4746-8844-777d3bf7ca11/content",
    "https://www.research-collection.ethz.ch/server/api/core/bitstreams/e4a8f42a-15fb-42f0-9d1b-bbd1b01a4107/content",
    "https://data.opencity.in/dataset/33d29ab0-f9e8-4fc7-b404-c93c1ed8e1b8/resource/f8af91cb-7b12-4404-bbca-976321be84bd/download/road-accidents-annual-2020-2024.csv",
    "https://data.opencity.in/dataset/33d29ab0-f9e8-4fc7-b404-c93c1ed8e1b8/resource/ac75369a-d065-463a-941e-702c6943e796/download/road-accidents-2024-cities-accidents-fatalities.csv",
    "https://archive-api.open-meteo.com/v1/archive?latitude=12.9716&longitude=77.5946&start_date=2021-01-01&end_date=2023-12-31&daily=temperature_2m_mean,precipitation_sum,rain_sum,wind_speed_10m_max&timezone=Asia%2FKolkata",
    "https://mausam.imd.gov.in/"
  ),
  license = c(
    "Open Data Commons Open Database License (ODbL) / OpenCity",
    "Open Data Commons Open Database License (ODbL) / OpenCity",
    "Open Data Commons Open Database License (ODbL) / OpenCity",
    "Open Data Commons Open Database License (ODbL) / OpenCity",
    "Karnataka GIS / OpenCity Public Domain",
    "Open Data Commons Open Database License (ODbL) / OpenCity",
    "Open Data Commons Open Database License (ODbL) / OpenCity",
    "Creative Commons Attribution 4.0 International (CC BY 4.0) - ETH Zurich",
    "Creative Commons Attribution 4.0 International (CC BY 4.0) - ETH Zurich",
    "National Crime Records Bureau (NCRB) / OpenCity Public",
    "National Crime Records Bureau (NCRB) / OpenCity Public",
    "Creative Commons Attribution 4.0 (CC BY 4.0)",
    "Government of India Open Access Policy / Restricted Portal"
  ),
  is_subset = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE),
  requires_manual = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE),
  is_optional = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE),
  status = "Pending",
  notes = "",
  stringsAsFactors = FALSE
)

# Helper function to download with retry and verification
download_resource <- function(row) {
  dest_file <- file.path(raw_dir, row$filename)
  url <- row$url
  is_subset <- row$is_subset
  requires_manual <- row$requires_manual
  is_optional <- row$is_optional
  
  if (requires_manual) {
    log_msg(sprintf("MANUAL REQUIRED: %s requires interactive portal access / registration. Skipping automated download.", row$dataset_id))
    return(list(
      status = "Manual Required",
      size = 0,
      sha256 = "N/A",
      notes = "Requires interactive portal registration/authentication (IMD Mausam). Automated access not permitted."
    ))
  }
  
  # Check if valid file already exists
  if (file.exists(dest_file) && file.info(dest_file)$size > 100) {
    log_msg(sprintf("File %s already exists and is non-empty (%d bytes). Verifying...", row$filename, file.info(dest_file)$size))
    # Check if not an HTML error page
    head_txt <- readLines(dest_file, n = 5, warn = FALSE)
    if (!any(grepl("<!DOCTYPE html>|<html", head_txt, ignore.case = TRUE))) {
      sha <- digest(dest_file, algo = "sha256", file = TRUE)
      return(list(
        status = "Confirmed (Cached)",
        size = file.info(dest_file)$size,
        sha256 = sha,
        notes = "Existing verified file preserved."
      ))
    } else {
      log_msg(sprintf("Existing file %s is an HTML error page. Re-downloading...", row$filename))
      unlink(dest_file)
    }
  }
  
  log_msg(sprintf("Initiating download for %s -> %s", row$dataset_id, row$filename))
  max_retries <- 3
  attempt <- 1
  success <- FALSE
  last_err <- ""
  
  while (attempt <= max_retries && !success) {
    log_msg(sprintf("  Attempt %d of %d for %s", attempt, max_retries, row$filename))
    res <- tryCatch({
      headers <- c("User-Agent" = "UrbanPulse-Academic-Research/1.0 (Macintosh; Apple Silicon)")
      
      if (is_subset) {
        # Range request for 10MB of UTD19 traffic measurements (~246k records)
        # Avoids downloading entire 7GB archive as instructed in Phase 2 & 11
        log_msg("  Performing bounded HTTP Range download (10 MB subset for manageable traffic benchmark)")
        r <- GET(url, config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE),
                 add_headers("User-Agent" = "Mozilla/5.0", "Range" = "bytes=0-10485760"), timeout(30))
      } else {
        r <- GET(url, config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE),
                 add_headers("User-Agent" = "Mozilla/5.0"), timeout(30))
      }
      
      http_code <- status_code(r)
      log_msg(sprintf("  HTTP Response Code: %d", http_code))
      
      if (http_code %in% c(200, 206)) {
        content_bin <- content(r, as = "raw")
        if (length(content_bin) < 50) {
          stop(sprintf("Content too small (%d bytes), likely error", length(content_bin)))
        }
        # Check if response is HTML error page
        sample_chars <- rawToChar(content_bin[1:min(500, length(content_bin))])
        if (grepl("<!DOCTYPE html>|<html", sample_chars, ignore.case = TRUE) && !grepl(".html", row$filename)) {
          stop("Server returned HTML page instead of expected data file")
        }
        
        # If subset, ensure we cleanly terminate at the last full line
        if (is_subset) {
          text_all <- rawToChar(content_bin)
          last_nl <- max(gregexpr("\n", text_all)[[1]])
          if (last_nl > 0) {
            text_all <- substr(text_all, 1, last_nl)
            writeLines(text_all, dest_file, useBytes = TRUE)
          } else {
            writeBin(content_bin, dest_file)
          }
        } else {
          writeBin(content_bin, dest_file)
        }
        
        file_sz <- file.info(dest_file)$size
        sha <- digest(dest_file, algo = "sha256", file = TRUE)
        log_msg(sprintf("  SUCCESS: Downloaded %s (%d bytes, SHA256: %s)", row$filename, file_sz, sha))
        return(list(
          status = "Confirmed",
          size = file_sz,
          sha256 = sha,
          notes = if (is_subset) "10 MB partial content range stream (246k rows)" else "Full verified download"
        ))
      } else {
        stop(sprintf("HTTP error status code %d", http_code))
      }
    }, error = function(e) {
      log_msg(sprintf("  Warning on attempt %d: %s", attempt, e$message))
      return(e$message)
    })
    
    if (is.list(res) && !is.null(res$status)) {
      success <- TRUE
      return(res)
    } else {
      last_err <- as.character(res)
      attempt <- attempt + 1
      Sys.sleep(2)
    }
  }
  
  # If failed
  log_msg(sprintf("FAILED download for %s after %d attempts: %s", row$filename, max_retries, last_err))
  return(list(
    status = if (is_optional) "Optional - Unavailable" else "Failed",
    size = 0,
    sha256 = "N/A",
    notes = sprintf("Download failed: %s", last_err)
  ))
}

# Execute downloads
results <- list()
for (i in seq_len(nrow(source_registry))) {
  row <- source_registry[i, ]
  res <- download_resource(row)
  source_registry$status[i] <- res$status
  source_registry$notes[i] <- res$notes
  results[[i]] <- data.frame(
    dataset_id = row$dataset_id,
    dataset_group = row$dataset_group,
    filename = row$filename,
    retrieval_timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    file_size_bytes = res$size,
    sha256 = res$sha256,
    download_status = res$status,
    license = row$license,
    notes = res$notes,
    source_url = row$url,
    stringsAsFactors = FALSE
  )
}

manifest_df <- do.call(rbind, results)

# Save Manifest and Source Register
manifest_path <- file.path(audit_dir, "download_manifest.csv")
write.csv(manifest_df, manifest_path, row.names = FALSE)
log_msg("Download manifest written to:", manifest_path)

source_reg_path <- file.path(audit_dir, "source_register.csv")
write.csv(source_registry, source_reg_path, row.names = FALSE)
log_msg("Source register written to:", source_reg_path)

log_msg("=== Download Pipeline Completed ===")
close(log_conn)

print(manifest_df[, c("filename", "download_status", "file_size_bytes")])
