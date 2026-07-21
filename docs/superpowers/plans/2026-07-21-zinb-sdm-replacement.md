# ZINB and Spatial Durbin Model Replacement Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace OLS Turnover Rate model in Cell 46 with Zero-Inflated Negative Binomial (ZINB) and upgrade Maximum Likelihood Spatial Lag in Cell 51 to Spatial Durbin Model (SDM) in `RSS-Analysis.ipynb`.

**Architecture:** Refactor notebook analysis cells using `statsmodels.discrete.count_model.ZeroInflatedNegativeBinomialP` for zero-inflated detainee count modeling and `spreg.ML_Lag` with `libpysal.weights.lag_spatial` for spatial exogenous spillovers.

**Tech Stack:** Python 3.12, Jupyter, Pandas, GeoPandas, Statsmodels, PySAL / Spreg, Libpysal.

## Global Constraints

- Modify target cells in `RSS-Analysis.ipynb` using Jupyter MCP server tools.
- Preserves all preceding data cleaning and spatial join steps.
- Maintain consistent variable names (`daily_pop`, `TotalPopulation`, `detention_rate`, `RUCC`, `GINI`, `No_HS`, `Poverty`, `SnapRate`, `CostBurden`).

---

### Task 1: Replace OLS Turnover Model with ZINB in Cell 46

**Files:**
- Modify: `RSS-Analysis.ipynb:Cell 46`

**Interfaces:**
- Consumes: `df_county` / `df` aggregated dataframe from Cell 26 & 38.
- Produces: ZINB model fit summary, log-likelihood, and coefficient statistics.

- [ ] **Step 1: Write ZINB implementation snippet for Cell 46**

```python
import numpy as np
import statsmodels.api as sm
from statsmodels.discrete.count_model import ZeroInflatedNegativeBinomialP

# Prepare dataset for ZINB model
zinb_df = df_county.copy().dropna(subset=['TotalPopulation', 'GINI', 'RiskyMobility', 'SnapRate', 'Unemployment', 'No_HS', 'HousingQuality', 'Poverty', 'CostBurden', 'RUCC'])

# Target outcome: integer daily population
zinb_df['daily_pop_int'] = np.round(zinb_df['daily_pop'].fillna(0)).astype(int)

# Formula for Count model and Inflation model
count_formula = "daily_pop_int ~ C(RUCC) + GINI + RiskyMobility + SnapRate + Unemployment + No_HS + HousingQuality + Poverty + CostBurden + C(year)"
inflation_formula = "~ C(RUCC) + Poverty + SnapRate + CostBurden"

# Exposure offset log(TotalPopulation)
exposure = np.log(zinb_df['TotalPopulation'])

# Fit ZINB model
zinb_model = sm.ZeroInflatedNegativeBinomialP.from_formula(
    formula=count_formula,
    exog_infl=sm.df_to_exog(zinb_df, inflation_formula),
    data=zinb_df,
    offset=exposure,
    p=2
)
zinb_res = zinb_model.fit(maxiter=500, method='bfgs')
print(zinb_res.summary())
```

- [ ] **Step 2: Apply snippet to Cell 46 using Jupyter MCP `overwrite_cell_source`**

- [ ] **Step 3: Execute Cell 46 and verify convergence output**

- [ ] **Step 4: Commit changes**

```bash
git add RSS-Analysis.ipynb
git commit -m "feat(rss): replace OLS turnover model with ZINB model in cell 46"
```

---

### Task 2: Upgrade Spatial Lag to Spatial Durbin Model (SDM) in Cell 51

**Files:**
- Modify: `RSS-Analysis.ipynb:Cell 51`

**Interfaces:**
- Consumes: `w` Queen weights matrix, `df_county` filtered by year (2013, 2018, 2023).
- Produces: Spatial Durbin Model regression output with direct and indirect impact estimates.

- [ ] **Step 1: Write Spatial Durbin Model snippet for Cell 51**

```python
import libpysal
from spreg import ML_Lag

years = [2013, 2018, 2023]
ses_cols = ['GINI', 'RiskyMobility', 'SnapRate', 'Unemployment', 'No_HS', 'HousingQuality', 'Poverty', 'CostBurden']

for yr in years:
    sub = df_county[df_county['year'] == yr].copy().reset_index(drop=True)
    w_yr = libpysal.weights.Queen.from_dataframe(sub, use_index=False)
    w_yr.transform = 'R'
    
    # Compute spatial lags for exogenous SES variables (W * X)
    wx_data = []
    wx_names = []
    for col in ses_cols:
        w_col = libpysal.weights.lag_spatial(w_yr, sub[col].values)
        sub[f'W_{col}'] = w_col
        wx_data.append(w_col)
        wx_names.append(f'W_{col}')
        
    x_vars = ses_cols + wx_names
    X = sub[x_vars].values
    y = sub['detention_rate'].values.reshape(-1, 1)
    
    sdm_model = ML_Lag(y, X, w=w_yr, name_y='detention_rate', name_x=x_vars, name_w='Queen_W')
    print(f"\n=================== Year {yr} Spatial Durbin Model ===================")
    print(sdm_model.summary)
```

- [ ] **Step 2: Apply snippet to Cell 51 using Jupyter MCP `overwrite_cell_source`**

- [ ] **Step 3: Execute Cell 51 and verify SDM spatial lag rho & W_X coefficients**

- [ ] **Step 4: Commit changes**

```bash
git add RSS-Analysis.ipynb
git commit -m "feat(rss): upgrade spatial lag models to spatial durbin models in cell 51"
```

---

### Task 3: Save and Commit Notebook

- [ ] **Step 1: Save full notebook**
- [ ] **Step 2: Git commit updated notebook and push**

```bash
git add RSS-Analysis.ipynb
git commit -m "feat(rss): complete ZINB and Spatial Durbin model integration"
git push origin rss-analysis
```
