#!/usr/bin/env Rscript
# ==============================================================================
# UrbanPulse — 03_missing_data_analysis.R
# Phase 4: Missing Data Audit, Treatment Formulation, and Sensitivity Analysis
# ==============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

base_dir <- getwd()
source(file.path(base_dir, "R", "helpers.R"))
source(file.path(base_dir, "R", "missing_data.R"))
source(file.path(base_dir, "R", "data_cleaning.R"))

raw_dir <- file.path(base_dir, "data", "raw")
audit_dir <- file.path(base_dir, "reports", "data_audit")
dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)

cat("=== UrbanPulse Phase 4: Missing Data Analysis & Sensitivity ===\n")

# 1. Audit missingness across every raw dataset
datasets_to_audit <- list(
  list(name = "bengaluru_crashes_2020_2022", file = "bengaluru_crashes_2020_2022.csv", ess = c("Zone", "Station", "2021 - Total Cases")),
  list(name = "bengaluru_crashes_2023", file = "bengaluru_crashes_2023.csv", ess = c("Station", "2023 - Total Cases")),
  list(name = "bengaluru_crashes_2024", file = "bengaluru_crashes_2024.csv", ess = c("Station", "2024-Total crashes")),
  list(name = "bengaluru_crashes_trend", file = "bengaluru_crashes_trend_2007_2025.csv", ess = c("Year", "Total")),
  list(name = "bengaluru_traffic_violations", file = "bengaluru_traffic_violations.csv", ess = c("YEAR", "Total Cases Booked")),
  list(name = "bengaluru_registered_vehicles", file = "bengaluru_registered_vehicles.csv", ess = c("Category", "Type of Vehicle")),
  list(name = "utd19_traffic_flow_subset", file = "utd19_traffic_flow_subset.csv", ess = c("day", "interval", "detid", "flow")),
  list(name = "utd19_detectors_public", file = "utd19_detectors_public.csv", ess = c("detid", "road", "long", "lat"))
)

audit_results <- list()
for (item in datasets_to_audit) {
  fpath <- file.path(raw_dir, item$file)
  if (file.exists(fpath)) {
    df_raw <- suppressWarnings(read_csv(fpath, col_types = cols(.default = "c"), show_col_types = FALSE))
    audit_results[[item$name]] <- audit_missingness(df_raw, dataset_name = item$name, essential_cols = item$ess)
  }
}

full_missing_report <- do.call(rbind, audit_results)
write.csv(full_missing_report, file.path(audit_dir, "missingness_report.csv"), row.names = FALSE)
cat("Missingness report written to reports/data_audit/missingness_report.csv\n")

# 2. Run Missing Data Sensitivity Analysis on Traffic Data
cat("Running missing-data sensitivity analysis on traffic series...\n")
traffic_df <- clean_utd19_traffic(
  flow_path = file.path(raw_dir, "utd19_traffic_flow_subset.csv"),
  detectors_path = file.path(raw_dir, "utd19_detectors_public.csv"),
  max_records = 50000
)

# Sort chronologically and create chronological train/test split
traffic_df <- traffic_df[order(traffic_df$datetime), ]
n_total <- nrow(traffic_df)
train_idx <- 1:floor(0.75 * n_total)
test_idx <- (floor(0.75 * n_total) + 1):n_total

train_split <- traffic_df[train_idx, ]
test_split <- traffic_df[test_idx, ]

# Run sensitivity comparison
sens_results <- run_missing_sensitivity(
  train_data = train_split,
  test_data = test_split,
  target_col = "flow",
  feature_cols = c("hour", "day_of_week", "flow_lag_1", "occ")
)

write.csv(sens_results, file.path(audit_dir, "missing_sensitivity_analysis.csv"), row.names = FALSE)
cat("Sensitivity analysis written to reports/data_audit/missing_sensitivity_analysis.csv\n")

# 3. Generate Markdown Documentation
md_content <- paste0(
  "# UrbanPulse: Missing-Data Analysis and Treatment Rationale\n\n",
  "## 1. Missingness Taxonomy and Empirical Observations\n\n",
  "| Dataset | Column | Missing Count | Missing Pct | Treatment Strategy | Rationale |\n",
  "|---|---|---|---|---|---|\n"
)

miss_high <- full_missing_report[full_missing_report$missing_count > 0, ]
for (i in seq_len(min(15, nrow(miss_high)))) {
  r <- miss_high[i, ]
  md_content <- paste0(
    md_content,
    sprintf("| %s | %s | %d | %.1f%% | %s | %s |\n",
            r$dataset, r$column_name, r$missing_count, r$missing_pct, r$proposed_treatment, r$treatment_rationale)
  )
}

md_content <- paste0(
  md_content,
  "\n## 2. Leakage-Free Imputation Protocol\n\n",
  "- **Target Variable Protection**: Target values (`flow`, `total_crashes`) are NEVER imputed. Model evaluation is conducted strictly on observed target values to prevent synthetic inflation of accuracy.\n",
  "- **Partition Boundary Integrity**: Feature statistics (e.g., median occupancy) are computed exclusively on the chronological training set and applied forward to validation and test sets.\n",
  "- **Missingness Indicators**: Missing predictor values are flagged with explicit binary indicators (`_was_imputed`) to preserve pattern information.\n\n",
  "## 3. Missingness Mechanism Limitation\n\n",
  "Transportation sensor dropouts and administrative report transitions are influenced by operational cycles and detector health. Missing Completely at Random (MCAR) cannot be defensibly claimed without sensor diagnostics. Consequently, sensitivity analysis evaluates both complete case and median-imputed strategies.\n\n",
  "## 4. Sensitivity Analysis Results\n\n",
  "```\n",
  paste(capture.output(print(sens_results)), collapse = "\n"),
  "\n```\n"
)

writeLines(md_content, file.path(audit_dir, "missingness_summary.md"))
cat("Missing-data analysis complete!\n")
