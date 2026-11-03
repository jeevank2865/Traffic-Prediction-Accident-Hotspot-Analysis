# UrbanPulse — Traffic Prediction & Accident Hotspot Analysis Using R

An academic data science, machine learning, and spatial analytics system for short-term traffic volume forecasting and road accident hotspot analysis.

Developed for **Apple Silicon (MacBook Air M2)** running **macOS Sonoma** with **R version 4.6.1**.

---

## 📌 Project Overview

UrbanPulse provides an end-to-end empirical workflow for intelligent transportation analytics:
1. **Automated Data Acquisition**: Pulls real open datasets from OpenCity, Karnataka GIS, and ETH Zurich (UTD19).
2. **Schema Discovery & Missingness Governance**: Audits raw schemas, sanitizes UTF-8 BOM encoding, and applies leakage-free training-set imputation.
3. **Traffic Volume Forecasting**: Chronologically trained Persistence, Multiple Linear Regression, and Random Forest ensembles evaluated on out-of-sample continuous 5-minute loop detector readings.
4. **Spatial Hotspot Analytics**: Ranks Bengaluru Traffic Police station jurisdictions using official Karnataka GIS coordinates and isolates high-risk arterial corridors.
5. **Interactive R Shiny Dashboard**: Modern, responsive, classic white dashboard featuring interactive Leaflet maps, Plotly charts, and DT data explorers.
6. **Automated Unit Testing**: 60-assertion test suite built with `testthat`.

---

## 📁 Repository Structure

```
UrbanPulse/
├── UrbanPulse.Rproj                 # RStudio Project file
├── app.R                            # Interactive R Shiny Dashboard
├── README.md                        # Documentation & Execution Guide
├── renv.lock                        # Reproducible dependency lockfile
├── data/
│   ├── raw/                         # Downloaded raw datasets & KML
│   ├── processed/                   # Cleaned, geocoded, feature-engineered datasets
│   └── external/                    # External spatial references
├── scripts/
│   ├── 01_download_data.R           # Automated download & verification pipeline
│   ├── 02_audit_data.R              # Schema discovery & compatibility matrix
│   ├── 03_missing_data_analysis.R   # Missingness audit & sensitivity analysis
│   ├── 04_exploratory_analysis.R    # Data cleaning & exploratory findings
│   ├── 05_train_traffic_model.R     # Chronological model training (zero leakage)
│   ├── 06_evaluate_models.R         # Out-of-sample evaluation & diagnostics
│   └── 07_accident_hotspots.R       # Spatial hotspot ranking & corridor clustering
├── R/
│   ├── helpers.R                    # String normalization, coordinate parser
│   ├── data_cleaning.R              # Modular cleaning functions
│   ├── missing_data.R               # Imputer engines & sensitivity evaluators
│   ├── traffic_model.R              # Machine learning training & evaluation
│   ├── accident_analysis.R          # Haversine distance, corridor clustering
│   └── visualization.R              # Leaflet mapping & Plotly themes
├── models/                          # Trained RDS model artifacts & metadata
├── reports/
│   ├── data_audit/                  # Download manifest, profiles, compatibility
│   ├── eda/                         # Statistical findings, hotspot reports
│   ├── figures/                     # Publication-quality charts (300 DPI)
│   ├── model_evaluation/            # Metrics leaderboard, residual diagnostics
│   └── final_report/                # Comprehensive academic markdown report
└── tests/
    ├── test_data_cleaning.R         # Unit tests for data cleaning & normalization
    ├── test_missing_data.R          # Unit tests for leakage-free imputation
    ├── test_traffic_model.R         # Unit tests for forecasting & metrics
    └── test_accident_analysis.R     # Unit tests for hotspots & spatial geometry
```

---

## 🚀 Quickstart & Pipeline Execution

### 1. Prerequisites
Ensure R (≥ 4.0) is installed on your Mac:
```bash
R --version
```

### 2. Run the Full Sequential Pipeline
You can execute each phase step-by-step from the terminal:

```bash
cd /Users/jeevan.k/.gemini/antigravity-ide/scratch/UrbanPulse

# Phase 2: Ingest all accessible open datasets
Rscript scripts/01_download_data.R

# Phase 3: Profile schemas and audit data quality
Rscript scripts/02_audit_data.R

# Phase 4: Analyze missingness & run sensitivity tests
Rscript scripts/03_missing_data_analysis.R

# Phase 5 & 6: Clean datasets & generate exploratory figures
Rscript scripts/04_exploratory_analysis.R

# Phase 7: Train machine learning forecasting models
Rscript scripts/05_train_traffic_model.R

# Phase 7: Evaluate models on independent test period
Rscript scripts/06_evaluate_models.R

# Phase 8: Perform spatial hotspot clustering
Rscript scripts/07_accident_hotspots.R
```

### 3. Run Automated Unit Tests
```bash
Rscript -e 'testthat::test_dir("tests", reporter = "summary")'
```
*Result: 60 passed out of 60 assertions (0 failures).*

### 4. Launch the Interactive Shiny Dashboard
```bash
Rscript -e 'shiny::runApp("app.R", launch.browser = TRUE, port = 8080)'
```
The dashboard opens at `http://127.0.0.1:8080`.

---

## 📊 Experimental Results

### Traffic Volume Forecasting (5-Minute Continuous Prediction)
Evaluated on an independent, non-overlapping chronological test period (May 23–25, 2017, N = 5,672):

| Model | Model Architecture | Test MAE | Test RMSE | Test R² | RMSE Improvement |
|---|---|---|---|---|---|
| **Persistence Baseline** | Naive Lag-1 ($\hat{y}_t = y_{t-1}$) | 5.387 | 14.327 | 0.9591 | Baseline (0.00%) |
| **Linear Regression** | Multi-variable Linear Model | 6.712 | 14.115 | 0.9603 | +1.48% |
| **Random Forest** | Machine Learning Bagging Ensemble | 7.202 | **13.823** | **0.9620** | **+3.52%** |

### Congestion Onset Classification (Occupancy ≥ 0.20)
- **Accuracy**: 81.38%
- **Precision**: 60.48%
- **Recall**: 50.66%
- **F1-Score**: 0.5514

### Top Bengaluru Accident Hotspots (2024 BTP Station Panel)
1. **Yalahanka** (North Zone): 242 total crashes (56 fatal) — 5.04% of city total
2. **K.R. Puram** (East Zone): 241 total crashes (34 fatal) — 5.02% of city total
3. **International Airport** (North Zone): 240 total crashes (60 fatal) — 5.00% of city total
4. **Whitefield** (East Zone): 189 total crashes (44 fatal) — 3.94% of city total
5. **Electronic City** (South Zone): 178 total crashes (42 fatal) — 3.71% of city total

*Pareto Concentration Finding*: **75.5%** of all recorded crashes in 2024 occurred within the top **20%** of police stations.

---

## ⚖️ Academic Integrity & Limitations

- **No Fabricated Road Coordinates**: Historical accident records are reported at police station jurisdiction resolution; station centroids from official Karnataka GIS are used for mapping rather than inventing road-level GPS coordinates.
- **Accident Count vs. Risk**: Recorded crash totals indicate historical incident frequencies. Localized accident risk cannot be estimated without link-level exposure (Vehicle Miles Traveled or AADT).
- **Benchmark Traffic Data**: Traffic measurements are transparently sourced from the open UTD19 urban sensor benchmark and are never falsely relabeled as Bengaluru street counts.
- **Zero Temporal Leakage**: All imputation statistics and normalizers are computed strictly on training partitions and applied forward to test partitions.
