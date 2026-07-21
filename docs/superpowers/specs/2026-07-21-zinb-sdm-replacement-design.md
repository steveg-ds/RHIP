# Design Specification: ZINB and Spatial Durbin Model Replacement

**Date:** 2026-07-21  
**Project:** RHIP (RSS-Analysis.ipynb)  
**Author:** Antigravity AI  

---

## 1. Executive Summary

This design specification outlines the refactoring of statistical modeling cells in `RSS-Analysis.ipynb`. Specifically:
1. **Cell 46 (OLS Model Replacement):** Replacing the OLS model for detention turnover rate with a **Zero-Inflated Negative Binomial (ZINB)** model to properly account for structural zeros (counties without ICE facilities) and right-skewed detainee counts.
2. **Cell 51 (Spatial Lag Replacement):** Upgrading the simple Maximum Likelihood Spatial Lag (`ML_Lag`) model to a **Spatial Durbin Model (SDM)** by incorporating spatially lagged exogenous predictors ($W \cdot X$) to measure direct and indirect (spillover) community characteristic effects.

---

## 2. Model 1: Zero-Inflated Negative Binomial (ZINB) Model

### Target Location
`RSS-Analysis.ipynb` - **Cell 46**

### Methodology & Specification
- **Library:** `statsmodels.discrete.count_model.ZeroInflatedNegativeBinomialP`
- **Dependent Variable ($Y$):** `daily_pop` rounded to positive integer count (`np.round(df['daily_pop']).fillna(0)`).
- **Exposure / Offset:** `np.log(df['TotalPopulation'])` to model detention rate per capita.
- **Inflation Model ($Z$):** Binary logit predicting zero-detention state.
  - Predictors: `C(RUCC)`, `Poverty`, `SnapRate`, `CostBurden`
- **Count Model ($X$):** Truncated negative binomial predicting non-zero detainee volume.
  - Predictors: `C(RUCC)`, `GINI`, `RiskyMobility`, `SnapRate`, `Unemployment`, `No_HS`, `HousingQuality`, `Poverty`, `CostBurden`, `C(year)`

### Output Requirements
- Model Summary table (Log-Likelihood, AIC, BIC, Dispersion Parameter $\alpha$).
- Estimated coefficients, standard errors, z-statistics, and p-values for both Inflation and Count portions.

---

## 3. Model 2: Spatial Durbin Model (SDM)

### Target Location
`RSS-Analysis.ipynb` - **Cell 51**

### Methodology & Specification
- **Library:** `spreg.ML_Lag` and `libpysal.weights.lag_spatial`
- **Years Analyzed:** 2013, 2018, 2023
- **Exogenous Spillovers ($W \cdot X$):**
  - Compute spatial lags using Queen weights matrix `w`:
    - `W_GINI = lag_spatial(w, df['GINI'])`
    - `W_No_HS = lag_spatial(w, df['No_HS'])`
    - `W_Poverty = lag_spatial(w, df['Poverty'])`
    - `W_SnapRate = lag_spatial(w, df['SnapRate'])`
    - `W_CostBurden = lag_spatial(w, df['CostBurden'])`
- **Expanded Model Matrix:** $X_{full} = [X, W \cdot X]$
- **Execution:** Fit `spreg.ML_Lag(y, X_full, w=w, name_y='detention_rate', name_x=x_names, name_w='Queen')`.

### Output Requirements
- Regression summary output for each analysis year.
- Spatial autoregressive parameter $\rho$ ($W \cdot Y$).
- Impact decomposition table (Direct, Indirect/Spillover, and Total impacts) for key SES variables.

---

## 4. Verification & Testing Criteria

1. ZINB model converges successfully without matrix singular errors.
2. AIC/BIC values reported for model fit evaluation.
3. Spatial Durbin models output valid spatial lag $\rho$ coefficients and p-values.
4. Notebook execution completes cleanly end-to-end.
