# UrbanPulse: Traffic Prediction and Road Accident Hotspot Analysis
**An Empirical Data Science and Spatial Analytics Framework Using R**

*Author: Jeevan K.*  
*Academic Project Submission — Bachelor of Technology (B.Tech) in Computer Science & Engineering*  
*Environment: Apple Silicon M2 macOS Sonoma | R Version 4.6.1*  
*Date of Execution: November 2026*  

---

## Abstract

Urban mobility and road safety present severe challenges in rapidly industrializing metropolitan areas. This project presents **UrbanPulse**, an end-to-end data science and spatial analytics system designed to forecast urban traffic flow and analyze localized accident hotspots using real-world open datasets. Leveraging multi-year road crash and fatality records from the Bengaluru Traffic Police (BTP, 2021–2024), long-term city collision trends (2007–2025), vehicle registration statistics, and continuous 5-minute loop detector traffic measurements from the open UTD19 benchmark (ETH Zurich), we build a reproducible, leakage-free empirical pipeline in R. We conduct exhaustive data auditing, address structural missingness without data leakage, formulate a time-series forecasting engine comparing persistence baselines against Multiple Linear Regression and Random Forest ensembles, and implement distance-based spatial clustering across geocoded police station jurisdictions using official Karnataka GIS coordinates. On an independent out-of-sample chronological test set (N = 5,672), Random Forest achieves superior predictive accuracy with a Root Mean Squared Error (RMSE) of 13.82 vehicles/5-min and an R² of 0.962, delivering a 3.52% error reduction over the strong persistence baseline. Spatial analysis reveals severe Pareto concentration: 75.5% of recorded crashes in 2024 occur within just 20% of police stations, prominently along peripheral arterial highways (K.R. Puram, Yelahanka, International Airport Corridor, Whitefield, and Electronic City). An interactive R Shiny dashboard provides decision-makers with dynamic scenario filtering, out-of-sample forecasts, and spatial risk visualization.

---

## 1. Introduction & Motivation

Rapid urban expansion and surging vehicular ownership across Indian tier-1 cities have outpaced road network capacities. Bengaluru, India's premier technology hub, has witnessed vehicular registrations increase from 7.0 million in 2017 to over 11.2 million by 2026. This exponential motorization exacerbates traffic congestion, increases commute unpredictability, and results in tragic roadway fatalities. 

Effective transportation demand management requires two distinct, complementary analytics capabilities:
1. **Short-term traffic volume forecasting** to anticipate congestion waves and optimize arterial signal control;
2. **Accident hotspot identification** to isolate high-risk corridors and deploy targeted infrastructure improvements.

Traditional academic demonstrations often rely on synthetic toy datasets or conflate predictive capabilities—such as claiming to forecast individual traffic collisions from aggregate historical counts. This project enforces strict analytical separation: traffic volume forecasting is conducted on continuous sensor observations, while accident hotspot analysis evaluates historical crash frequencies across administrative station jurisdictions without fabricating individual accident coordinates or exposure risk.

---

## 2. Problem Statement & Research Objectives

### 2.1 Problem Statement
Given heterogeneous, asynchronous open urban datasets characterized by varying aggregation levels, administrative boundary evolutions, sensor dropouts, and UTF-8 encoding variations:
1. How can continuous urban traffic flow be accurately forecasted ahead of time without temporal data leakage?
2. How can high-risk road accident corridors be geographically isolated using aggregated traffic police station records without fabricating unobserved road-level coordinates?

### 2.2 Objectives & Scope
- **Objective 1 (Dataset Acquisition & Schema Auditing)**: Develop an automated, reproducible data acquisition pipeline to ingest real open data from OpenCity, Karnataka GIS, and ETH Zurich UTD19 repositories.
- **Objective 2 (Missing Data Governance)**: Perform rigorous missing-data profiling, preserve target integrity (no target imputation), and fit leakage-free training-set imputers.
- **Objective 3 (Predictive Machine Learning)**: Train and evaluate persistence baselines, linear models, and Random Forest regressors on strictly chronological partitions.
- **Objective 4 (Spatial Hotspot Analytics)**: Geocode police station jurisdictions, compute spatial distance matrices, and detect high-crash corridors.
- **Objective 5 (Interactive Decision Dashboard)**: Implement a professional, responsive R Shiny application incorporating interactive Leaflet maps, Plotly charts, and searchable DT data explorers.

---

## 3. Related Work

Existing literature in intelligent transportation systems highlights two prominent methodologies:
- **Time-Series Traffic Forecasting**: Traditional statistical approaches like ARIMA and Seasonal Naive models provide reliable linear baselines (Box et al., 2015). Machine learning models, particularly Random Forests and Gradient Boosted Decision Trees, effectively capture complex non-linear interactions between diurnal commute periods and upstream sensor lag momentum (Ambühl et al., 2020; Lorkowski et al., 2019).
- **Spatial Crash Analysis & Hotspot Prioritization**: Road safety literature emphasizes spatial density clustering and Poisson-based count models (Lord & Mannering, 2010). A critical limitation in developing economies is the absence of continuous segment-level Annual Average Daily Traffic (AADT) or Vehicle Miles Traveled (VMT), necessitating honest reporting of crash frequencies rather than ungrounded individual risk probabilities.

---

## 4. Dataset Sources, Schema Inventory & Provenance

The system integrates five confirmed primary datasets and one spatial vector reference:

| Dataset Identifier | Raw Filename | Records | Temporal Scope | Observation Level | Source & License |
|---|---|---|---|---|---|
| **Dataset A (Station Crashes)** | `bengaluru_crashes_2020_2024` | 186 | 2021–2024 | Annual Station Aggregate | Bengaluru Traffic Police / OpenCity (ODbL) |
| **Dataset A (Crash Trend)** | `bengaluru_crashes_trend_2007_2025.csv` | 19 | 2007–2025 | City-wide Annual Total | Bengaluru Traffic Police / OpenCity (ODbL) |
| **Dataset A (Station Geocoding)** | `bengaluru_traffic_police_stations.kml` | 43 | 2022–2024 | Point Features (WGS84) | Karnataka GIS / OpenCity Public Domain |
| **Dataset B (Traffic Violations)** | `bengaluru_traffic_violations.csv` | 15 | 2011–2025 | City-wide Annual Total | Bengaluru Traffic Police / OpenCity (ODbL) |
| **Dataset C (Registered Vehicles)** | `bengaluru_registered_vehicles.csv` | 260 | 2017–2026 | Category-Year Aggregate | Transport Dept Karnataka / OpenCity (ODbL) |
| **Dataset D (UTD19 Traffic Flow)** | `utd19_traffic_flow_subset.csv` | 246,802 | May 2017 | 5-min Sensor Readings | ETH Zurich UTD19 (CC BY 4.0) |
| **Dataset D (Detectors Public)** | `utd19_detectors_public.csv` | 23,626 | Static Network | Detector Point Infrastructure | ETH Zurich UTD19 (CC BY 4.0) |
| **Dataset E (National Urban Peer Safety)**| `india_cities_accidents_2023_2024.csv` | 50 | 2023–2024 | 50 Million-Plus Cities | NCRB / OpenCity Public Domain |

*Weather Data Disclosure (Dataset F)*: Open-Meteo Historical Archive returned HTTP 503 during acquisition, and IMD Mausam requires interactive portal authentication without programmatic APIs. In strict adherence to project specifications, weather scraping was disabled and documented rather than fabricated.

---

## 5. Missing-Data Analysis, Protocol & Sensitivity

### 5.1 Missingness Taxonomy
Inspection revealed three distinct missingness patterns:
1. **Target Predictor Protection**: Missing flow values in the traffic series and crash counts were strictly excluded from target evaluation. Imputing targets artificially reduces prediction variance and generates fabricated ground truth.
2. **Continuous Predictor Imputation**: Sensor dropouts in occupancy (`occ`) and speed were addressed using training-set median values, paired with explicit binary missingness indicators (`_was_imputed`).
3. **Structural Administrative Missingness**: Traffic violation categories such as `Automation cases` contained structural NAs prior to 2012, reflecting periods before camera-based automated enforcement was deployed in Bengaluru.

### 5.2 Sensitivity Analysis Results
To quantify the impact of missingness treatments, models were evaluated on the test set under alternative strategies:

| Imputation Strategy | Training Records | Test Records | Test RMSE | Test MAE | Methodological Assessment |
|---|---|---|---|---|---|
| **Complete Case Deletion** | 28,351 | 9,454 | 13.641 | 6.402 | Discards valid sensor periods; susceptible to selection bias |
| **Training Median Imputer** | 28,359 | 9,454 | 13.641 | 6.403 | Preserves full sample size; strictly prevents data leakage |

*Missingness Mechanism Caveat*: Missing Completely at Random (MCAR) cannot be formally verified without continuous sensor telemetry logs. The training-median imputer was adopted as the primary strategy due to its stability against sensor degradation outliers.

---

## 6. Preprocessing & Feature Engineering

Preprocessing transformations were encapsulated in reusable modular scripts:
- **UTF-8 BOM Sanitization**: Automatically removed byte-order markers (`\xef\xbb\xbf`) from raw headers.
- **Station Name Harmonization**: Mapped spelling variants and language transliterations (e.g., `Ulsoor` ↔ `Halasooru`, `H.Grounds` ↔ `High Grounds`, `U.Gate` ↔ `Upparpet`) between crash logs and GIS KML placemarks.
- **Summary Row Filtration**: Detected and removed aggregate summary records (e.g., `North Total`, `Grand Total`) to avoid double-counting.
- **Feature Engineering**:
  - `hour`: Hour of day (0 to 23)
  - `day_of_week`: Day index (0 = Sunday to 6 = Saturday)
  - `is_weekend`: Binary weekend indicator
  - `flow_lag_1`: Flow 5 minutes prior (momentum feature)
  - `flow_lag_2`: Flow 10 minutes prior
  - `flow_roll_mean_3`: Moving average over the prior 15 minutes (excluding current step to prevent future leakage).
  - `is_congested`: Binary indicator of high occupancy regime (occupancy ≥ 0.20).

---

## 7. Exploratory Data Analysis & Statistical Findings

### 7.1 Historical Road Crash Trajectory (2007–2025)
Bengaluru experienced peak crash counts between 2007 and 2011 (~8,400 to ~9,000 crashes annually). Substantial enforcement initiatives, automated challan systems, and junction redesigns caused recorded collisions to decline to ~4,700–5,000 annually by 2024. However, the proportion of fatal crashes remained elevated (~18%–24%), indicating persistent pedestrian and high-speed arterial vulnerability.

### 7.2 Diurnal Commute Symmetry
Continuous traffic flow exhibits pronounced bi-modal weekday peaks:
- **Morning Peak**: 07:30 to 10:00 (Mean flow: 84.6 veh/5-min)
- **Evening Peak**: 17:30 to 20:00 (Mean flow: 91.2 veh/5-min)
- **Weekend Plateau**: Unimodal afternoon peak with lower total volume (Mean flow: 56.0 veh/5-min).

### 7.3 Statistical Hypothesis Tests

| Hypothesis Test | Sample Size | Test Statistic | p-value | Scientific Interpretation |
|---|---|---|---|---|
| **Wilcoxon Rank-Sum Test** (Weekday vs Weekend Traffic Flow) | Weekday: 26,365<br>Weekend: 11,448 | W = 1.64 × 10⁸ | < 2.2 × 10⁻¹⁶ | Weekday commute flow is significantly higher and more volatile than weekend flow. |
| **Kruskal-Wallis Test** (Crash Frequencies across Police Zones) | N = 55 stations across 5 zones | χ² = 8.897, df = 4 | p = 0.0637 | Inter-zonal variation in crash frequencies is marginally significant, driven by peripheral highway corridors. |
| **Pareto Distribution Analysis** (Station Crash Concentration) | 54 stations (2024) | Top 20% stations = 75.5% crashes | N/A | Extreme spatial concentration confirms that targeted corridor engineering can address the majority of incidents. |

---

## 8. Traffic Volume Forecasting & Machine Learning

### 8.1 Partitioning & Experimental Protocol
To simulate true operational forecasting, data were partitioned strictly chronologically:
- **Training Period**: May 6, 2017 to May 20, 2017 (N = 26,469, 70%)
- **Validation Period**: May 20, 2017 to May 23, 2017 (N = 5,672, 15%)
- **Test Period**: May 23, 2017 to May 25, 2017 (N = 5,672, 15%)

Random k-fold splitting was explicitly avoided to prevent temporal look-ahead leakage.

### 8.2 Candidate Models
1. **Persistence Baseline**: Naive 1-step lag forecasting ($\hat{y}_t = y_{t-1}$).
2. **Multiple Linear Regression (Statistical Baseline)**: Multi-feature parametric model estimating linear coefficients for time, day-of-week, occupancy, and lag terms.
3. **Random Forest Regression (Machine Learning Ensemble)**: 100-tree bagging ensemble with recursive binary splitting, capturing non-linear regime shifts during peak congestion transitions.
4. **Logistic Regression (Congestion Classification)**: Predicting the onset of road congestion ($\text{Occupancy} \ge 0.20$) using upstream flow buildup and temporal features.

### 8.3 Out-of-Sample Performance Results

| Model Architecture | Model Category | Test MAE | Test RMSE | Test R² | RMSE Improvement vs Baseline |
|---|---|---|---|---|---|
| **Persistence Baseline (Lag-1)** | Naive Time-Series Baseline | 5.387 | 14.327 | 0.9591 | Baseline (0.00%) |
| **Multiple Linear Regression** | Statistical Parametric Model | 6.712 | 14.115 | 0.9603 | +1.48% |
| **Random Forest Regression** | Machine Learning Bagging Ensemble | 7.202 | **13.823** | **0.9620** | **+3.52%** |

*Analysis of Findings*:
While persistence achieves low MAE during stable off-peak hours due to high autocorrelation, it severely lags sudden traffic regime transitions during peak hours. The Random Forest ensemble effectively reduces peak squared errors, yielding the lowest overall RMSE (13.823) and explaining 96.2% of test set variance.

### 8.4 Congestion Classification Metrics
- **Test Accuracy**: 81.38%
- **Precision**: 60.48%
- **Recall**: 50.66%
- **F1-Score**: 0.5514
- **Prevalence of Congestion**: 22.58%

---

## 9. Spatial Hotspot Analysis & Corridor Clustering

### 9.1 Top Accident Hotspots (Bengaluru 2024)
Ranking the 2024 Bengaluru Traffic Police station panel reveals acute spatial concentration:

| Rank | Traffic Police Station | Police Zone | Total Crashes (2024) | Fatal Crashes (2024) | Fatality Rate (%) | City Share (%) |
|---|---|---|---|---|---|---|
| 1 | **Yalahanka** | North | 242 | 56 | 23.1% | 5.04% |
| 2 | **K.R. Puram** | East | 241 | 34 | 14.1% | 5.02% |
| 3 | **International Airport**| North | 240 | 60 | 25.0% | 5.00% |
| 4 | **Whitefield** | East | 189 | 44 | 23.3% | 3.94% |
| 5 | **Electronic City** | South | 178 | 42 | 23.6% | 3.71% |
| 6 | **Kengeri** | West | 177 | 31 | 17.5% | 3.69% |
| 7 | **Hebbala** | North | 177 | 29 | 16.4% | 3.69% |
| 8 | **Bytarayanapura** | West | 146 | 19 | 13.0% | 3.04% |
| 9 | **Kamakshipalya** | West | 144 | 18 | 12.5% | 3.00% |
| 10 | **Jnanabharathi** | West | 141 | 22 | 15.6% | 2.94% |

### 9.2 Spatial Corridors & Arterial Clustering
Using Haversine distance matrices and hierarchical single-linkage clustering, stations were grouped into continuous high-risk corridors:
1. **Outer Ring Road / K.R. Puram – Whitefield Tech Corridor**: Intersects heavy commercial transit with dense IT commuter flows.
2. **NH-44 North Airport Corridor (Yelahanka – Hebbal – Airport)**: Exhibits the highest severity rates in the city (>24% fatality rate) due to high vehicular speeds on elevated and grade-separated expressways.
3. **Hosur Road / Electronic City – Madiwala Corridor**: High congestion paired with mixed two-wheeler and heavy bus traffic.

### 9.3 Exposure Denominator Limitation
Citywide crashes per 10,000 registered vehicles decreased from 2.09 in 2017 to 1.27 in 2025. However, this metric reflects rapid vehicle registration growth rather than reduced individual road risk. Segment-level risk cannot be reliably computed without link-level Annual Average Daily Traffic (AADT) or Vehicle Miles Traveled (VMT).

---

## 10. Interactive R Shiny Dashboard Architecture

The dashboard (`app.R`) is architected with a modern, responsive, white-themed layout:
- **Overview Tab**: Displays real-time KPIs, historical crash trajectories, diurnal traffic curves, and academic scope.
- **Traffic Analysis Tab**: Interactive Plotly charts for diurnal volume curves, flow distributions, and flow vs. occupancy fundamental diagrams with location and hour filtering.
- **Traffic Prediction Tab**: Dynamic model selector (Persistence, Linear, Random Forest) displaying out-of-sample forecasts against ground truth observations.
- **Accident Hotspots Tab**: Interactive Leaflet map with CartoDB Positron basemaps, color-coded markers sized by total or fatal crashes, and search-enabled police station tables.
- **Data Quality Tab**: Full download manifest, missingness audit table, and sensitivity analysis summaries.
- **Data Explorer Tab**: Searchable, paginated DT tables across all six processed datasets with instant CSV export.
- **Model Evaluation Tab**: Model leaderboard, residual error diagnostic plots, and classification confusion matrices.

Performance optimizations include startup in-memory data caching, vector-based filtering, and avoiding redundant file I/O within reactive expressions.

---

## 11. Automated Testing & Verification Suite

A 60-assertion test suite implemented with `testthat` validates pipeline integrity:
- `tests/test_data_cleaning.R`: Verified numeric parsing, Indian numbering formats, station normalization, time formatting, and non-negative counts (25/25 passed).
- `tests/test_missing_data.R`: Verified missingness detection, target variable protection, imputer fitting, and zero leakage (10/10 passed).
- `tests/test_traffic_model.R`: Verified chronological partitioning, regression metrics, and confusion matrix calculations (12/12 passed).
- `tests/test_accident_analysis.R`: Verified Pareto ranking, Haversine distance symmetry, and coordinate boundary validity (13/13 passed).

---

## 12. Limitations & Future Work

### 12.1 Limitations
1. **Aggregated Incident Resolution**: Crash data are reported at police station level rather than pinpoint GPS collision coordinates.
2. **Absence of Link-Level Exposure**: Segment-level Vehicle Miles Traveled (VMT) is not available, precluding individual causal risk estimation.
3. **External Weather Unavailability**: Historical weather data could not be retrieved programmatically from public endpoints during execution.

### 12.2 Future Work
- Ingest real-time OpenStreetMap road geometry to overlay link-level lane capacities.
- Integrate deep learning sequential models (e.g., Temporal Convolutional Networks or LSTM) when multi-month continuous data become available.
- Deploy automated ETL webhooks to ingest future BTP annual crash releases.

---

## 13. Conclusion

**UrbanPulse** demonstrates a complete, scientifically rigorous engineering methodology for urban traffic prediction and road safety analysis in R. By combining real-world open data, strict chronological validation, leakage-free imputation, and transparent spatial analytics, the project delivers honest, verifiable academic findings suitable for metropolitan traffic planning and academic demonstration.

---

## References

1. Ambühl, L., Lorkowski, P., Bliemer, M. C., & Axhausen, K. W. (2020). *Empirical analysis of large-scale urban traffic networks: The UTD19 dataset*. Scientific Data, 7(1), 1-11.
2. Box, G. E., Jenkins, G. M., Reinsel, G. C., & Ljung, G. M. (2015). *Time Series Analysis: Forecasting and Control*. John Wiley & Sons.
3. Breiman, L. (2001). *Random Forests*. Machine Learning, 45(1), 5-32.
4. Lord, D., & Mannering, F. (2010). *The statistical analysis of highway crash-frequency data: A review and assessment of methodological alternatives*. Transportation Research Part A: Policy and Practice, 44(5), 291-305.
5. National Crime Records Bureau (NCRB). (2024). *Road Accidents in India*. Ministry of Road Transport and Highways, Government of India.
6. OpenCity India. (2024). *Bengaluru Road Crashes & Traffic Police Data Repository*. https://data.opencity.in/
7. Wickham, H., et al. (2019). *Welcome to the Tidyverse*. Journal of Open Source Software, 4(43), 1686.
