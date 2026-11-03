# UrbanPulse: Missing-Data Analysis and Treatment Rationale

## 1. Missingness Taxonomy and Empirical Observations

| Dataset | Column | Missing Count | Missing Pct | Treatment Strategy | Rationale |
|---|---|---|---|---|---|
| bengaluru_crashes_2020_2022 | Zone | 1 | 2.2% | Contextual Review / Complete Case Analysis | Exploratory variable with modest missingness. |
| bengaluru_crashes_2023 | Zone | 10 | 15.9% | Contextual Review / Complete Case Analysis | Exploratory variable with modest missingness. |
| bengaluru_crashes_2023 | Sub-division | 5 | 7.9% | Encode as Explicit 'Unknown' Category | Categorical administrative attribute: preserves missingness signal without fabricating location. |
| bengaluru_crashes_2024 | Zone | 10 | 15.4% | Contextual Review / Complete Case Analysis | Exploratory variable with modest missingness. |
| bengaluru_crashes_2024 | Sub-division | 5 | 7.7% | Encode as Explicit 'Unknown' Category | Categorical administrative attribute: preserves missingness signal without fabricating location. |
| bengaluru_traffic_violations | K.T.C Act Cases | 4 | 26.7% | Explicit Zero vs Not Applicable Distinction | Administrative enforcement data: system automation/towing not active in earlier years (valid structural zero/NA). |
| bengaluru_traffic_violations | K.T.C Act Fine | 4 | 26.7% | Contextual Review / Complete Case Analysis | Exploratory variable with modest missingness. |
| bengaluru_registered_vehicles | Sl No | 4 | 15.4% | Contextual Review / Complete Case Analysis | Exploratory variable with modest missingness. |
| bengaluru_registered_vehicles | Type of Vehicle | 8 | 30.8% | Contextual Review / Complete Case Analysis | Exploratory variable with modest missingness. |
| utd19_traffic_flow_subset | error | 135374 | 54.9% | Contextual Review / Complete Case Analysis | Exploratory variable with modest missingness. |
| utd19_traffic_flow_subset | speed | 246802 | 100.0% | Training-Set Median Imputation with Missingness Indicator | Continuous traffic predictor: single loop detectors often lack speed. Preserves sample size without leakage. |
| utd19_detectors_public | pos | 45 | 0.2% | Contextual Review / Complete Case Analysis | Exploratory variable with modest missingness. |
| utd19_detectors_public | road | 1541 | 6.5% | Encode as Explicit 'Unknown' Category | Categorical administrative attribute: preserves missingness signal without fabricating location. |
| utd19_detectors_public | limit | 6632 | 28.1% | Contextual Review / Complete Case Analysis | Exploratory variable with modest missingness. |
| utd19_detectors_public | lanes | 4 | 0.0% | Contextual Review / Complete Case Analysis | Exploratory variable with modest missingness. |

## 2. Leakage-Free Imputation Protocol

- **Target Variable Protection**: Target values (`flow`, `total_crashes`) are NEVER imputed. Model evaluation is conducted strictly on observed target values to prevent synthetic inflation of accuracy.
- **Partition Boundary Integrity**: Feature statistics (e.g., median occupancy) are computed exclusively on the chronological training set and applied forward to validation and test sets.
- **Missingness Indicators**: Missing predictor values are flagged with explicit binary indicators (`_was_imputed`) to preserve pattern information.

## 3. Missingness Mechanism Limitation

Transportation sensor dropouts and administrative report transitions are influenced by operational cycles and detector health. Missing Completely at Random (MCAR) cannot be defensibly claimed without sensor diagnostics. Consequently, sensitivity analysis evaluates both complete case and median-imputed strategies.

## 4. Sensitivity Analysis Results

```
                                                strategy train_records_used
Complete_Case                     Complete Case Deletion              28351
Training_Median Training Median Imputation (Recommended)              28359
                test_records_used imputation_count   rmse   mae
Complete_Case                9454                0 13.641 6.402
Training_Median              9454                0 13.641 6.403
                                                                                        analytical_risk
Complete_Case   Discards valid observations during sensor degradation; potential sample selection bias.
Training_Median         Robust to outliers; strictly prevents data leakage from future/test partitions.
```

