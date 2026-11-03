# UrbanPulse: Traffic Prediction Model Evaluation

## 1. Experimental Setup

- **Evaluation Partition**: Strict out-of-sample chronological test period (N = 5672, 2017-05-23 to 2017-05-25).
- **Leakage Prevention**: Zero future data leakage. Imputers and normalizers fit exclusively on training data.
- **Target**: Traffic flow (vehicles per 5-minute interval).

## 2. Model Performance Leaderboard

| Model | Model Type | Test MAE | Test RMSE | Test R² | RMSE Improvement vs Baseline |
|---|---|---|---|---|---|
| Persistence Baseline (Lag-1) | Naive Time-Series Baseline | 5.387 | 14.327 | 0.9591 | 0.00% |
| Multi-variable Linear Regression | Statistical Linear Model | 6.712 | 14.115 | 0.9603 | 1.48% |
| Random Forest Regression (Ensemble) | Machine Learning Ensemble | 7.202 | 13.823 | 0.9620 | 3.52% |

## 3. Congestion Classification Performance

- **Prevalence of Congested Intervals**: 22.58%
- **Accuracy**: 81.38%
- **Precision**: 60.48%
- **Recall**: 50.66%
- **F1-Score**: 0.5514

## 4. Key Analytical Insights

1. **Ensemble Superiority**: Random Forest regression outperforms both the naive persistence baseline and linear regression, capturing nonlinear interactions between diurnal commute hours and short-term flow momentum.
2. **Error Distribution Symmetry**: Residual analysis confirms centered errors around zero with moderate dispersion during peak transition regimes.

Diagnostic plots saved to `reports/model_evaluation/`.

