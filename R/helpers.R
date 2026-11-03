# ==============================================================================
# UrbanPulse — R/helpers.R
# General Helper Utilities, String Cleaning, and Geographical Mappings
# ==============================================================================

#' Safely parse strings to numeric by stripping commas, percentages, and whitespace
#' @param x Character vector or numeric vector
#' @return Numeric vector with NA for invalid/unparseable values
parse_clean_numeric <- function(x) {
  if (is.numeric(x)) return(x)
  if (is.null(x) || length(x) == 0) return(numeric(0))
  x_clean <- gsub("[,%]", "", trimws(as.character(x)))
  x_clean[x_clean %in% c("", "NA", "N/A", "null", "NULL", "-", ".")] <- NA
  suppressWarnings(as.numeric(x_clean))
}

#' Normalize station name for consistent cross-dataset linkage
#' Preserves original names while generating a standardized match key
#' @param name Character vector of station names
#' @return Character vector of standardized lowercase alphanumeric keys
normalize_station_name <- function(name) {
  s <- tolower(trimws(as.character(name)))
  # Remove prefixes like "traffic police station,"
  s <- gsub("^traffic police station,?\\s*", "", s)
  s <- gsub("\\s*traffic police station$", "", s)
  s <- gsub("^tps\\s*-\\s*[0-9]+\\s*", "", s)
  # Standardize known BTP station abbreviations and spelling variants
  s <- gsub("^h\\.?\\s*grounds", "high grounds", s)
  s <- gsub("^w\\.?\\s*garden", "wilson garden", s)
  s <- gsub("^s\\.?\\s*s\\.?\\s*nagar", "sadashivanagar", s)
  s <- gsub("^u\\.?\\s*gate", "upparpet", s)
  s <- gsub("^ulsoor", "halasooru", s)
  s <- gsub("^halasur", "halasooru", s)
  s <- gsub("^cubbon\\s*park", "cubbonpark", s)
  s <- gsub("^ashokanagar", "ashoknagar", s)
  s <- gsub("^k\\.?\\s*g\\.?\\s*halli", "k g halli", s)
  s <- gsub("^k\\.?\\s*r\\.?\\s*puram", "k r puram", s)
  s <- gsub("^r\\.?\\s*t\\.?\\s*nagar", "r t nagar", s)
  s <- gsub("^v\\.?\\s*v\\.?\\s*puram", "v v puram", s)
  s <- gsub("^m\\.?\\s*t\\.?\\s*b", "malleshwaram", s)
  # Strip punctuation
  s <- gsub("[^a-z0-9 ]", "", s)
  s <- gsub("\\s+", " ", s)
  trimws(s)
}

#' Convert seconds from midnight to HH:MM format
#' @param interval_sec Numeric vector of seconds (e.g., 0, 300, 3600)
#' @return Character vector of formatted times "HH:MM"
format_interval_time <- function(interval_sec) {
  hours <- floor(interval_sec / 3600) %% 24
  minutes <- floor((interval_sec %% 3600) / 60)
  sprintf("%02d:%02d", hours, minutes)
}

#' Extract placemarks and coordinates from KML string
#' @param kml_path Path to the KML file
#' @return Data frame with station_raw, station_norm, longitude, latitude
parse_btp_kml <- function(kml_path) {
  if (!file.exists(kml_path)) return(data.frame())
  kml_txt <- readLines(kml_path, warn = FALSE)
  full_str <- paste(kml_txt, collapse = "\n")
  
  placemarks <- unlist(regmatches(full_str, gregexpr("<Placemark>.*?</Placemark>", full_str)))
  if (length(placemarks) == 0) return(data.frame())
  
  res <- lapply(placemarks, function(pm) {
    # Extract station name
    nm_m <- regmatches(pm, regexec("<SimpleData name=\"TRF_POL_STAName\">([^<]+)</SimpleData>", pm))[[1]]
    name <- if (length(nm_m) > 1) trimws(nm_m[2]) else "Unknown Station"
    
    # Extract coordinates
    co_m <- regmatches(pm, regexec("<coordinates>([^<]+)</coordinates>", pm))[[1]]
    coords <- if (length(co_m) > 1) strsplit(trimws(co_m[2]), ",")[[1]] else c(NA, NA)
    
    lon <- suppressWarnings(as.numeric(coords[1]))
    lat <- suppressWarnings(as.numeric(coords[2]))
    
    data.frame(
      station_kml_raw = name,
      station_norm = normalize_station_name(name),
      longitude = lon,
      latitude = lat,
      stringsAsFactors = FALSE
    )
  })
  
  out_df <- do.call(rbind, res)
  # Filter valid coordinates in Bengaluru bounding box (approx lat 12.7-13.3, lon 77.3-77.9)
  out_df$coord_valid <- !is.na(out_df$longitude) & !is.na(out_df$latitude) &
    out_df$latitude >= 12.7 & out_df$latitude <= 13.3 &
    out_df$longitude >= 77.3 & out_df$longitude <= 77.9
  out_df
}

#' Create consistent train-validation-test chronological split index
#' @param n Total number of time steps
#' @param train_prop Proportion for training (default 0.70)
#' @param val_prop Proportion for validation (default 0.15)
#' @return Factor vector with levels "train", "val", "test"
chronological_split <- function(n, train_prop = 0.70, val_prop = 0.15) {
  train_end <- floor(n * train_prop)
  val_end <- floor(n * (train_prop + val_prop))
  
  split <- rep("test", n)
  split[1:train_end] <- "train"
  if (val_end > train_end) {
    split[(train_end + 1):val_end] <- "val"
  }
  factor(split, levels = c("train", "val", "test"))
}

#' Load verified Bengaluru hospitals dataset with graceful Places API fallback
#' @param csv_path Path to the local verified hospitals dataset
#' @param api_key Optional Google Places API key (defaults to Sys.getenv("GOOGLE_MAPS_API_KEY", ""))
#' @return Data frame with hospital_name, facility_type, zone, address, emergency_trauma, latitude, longitude, contact_phone
load_bengaluru_hospitals <- function(csv_path = file.path(getwd(), "data", "processed", "bengaluru_hospitals.csv"),
                                    api_key = Sys.getenv("GOOGLE_MAPS_API_KEY", "")) {
  # 1. Attempt Google Places API query if key is configured
  if (nzchar(trimws(api_key))) {
    tryCatch({
      url <- sprintf(
        "https://maps.googleapis.com/maps/api/place/nearbysearch/json?location=12.9716,77.5946&radius=25000&type=hospital&key=%s",
        curl::curl_escape(trimws(api_key))
      )
      con <- url(url, open = "r")
      raw_json <- paste(readLines(con, warn = FALSE), collapse = "")
      close(con)
      parsed <- jsonlite::fromJSON(raw_json)
      if (!is.null(parsed$status) && parsed$status == "OK" && length(parsed$results) > 0) {
        places_df <- data.frame(
          hospital_name = as.character(parsed$results$name),
          facility_type = "Verified Medical Facility (Google Places)",
          zone = "Bengaluru Urban",
          address = ifelse(!is.null(parsed$results$vicinity), as.character(parsed$results$vicinity), "Bengaluru"),
          emergency_trauma = "Emergency Care Facility",
          latitude = as.numeric(parsed$results$geometry$location$lat),
          longitude = as.numeric(parsed$results$geometry$location$lng),
          contact_phone = "108 Emergency",
          stringsAsFactors = FALSE
        )
        places_df <- places_df[!is.na(places_df$latitude) & !is.na(places_df$longitude), ]
        if (nrow(places_df) > 0) {
          return(places_df)
        }
      }
    }, error = function(e) {
      # Graceful fallback to local verified dataset
    })
  }
  
  # 2. Return local verified dataset
  if (file.exists(csv_path)) {
    return(suppressWarnings(readr::read_csv(csv_path, show_col_types = FALSE)))
  }
  
  data.frame(
    hospital_name = character(), facility_type = character(), zone = character(),
    address = character(), emergency_trauma = character(),
    latitude = numeric(), longitude = numeric(), contact_phone = character(),
    stringsAsFactors = FALSE
  )
}
