# Data Cleaning Report: ACS Processing

## Overview
Processed ACS data from 2023 for mobility, SNAP, employment, education, housing quality, poverty, and cost burden.

## Proposed Cleaning Steps

### 1. CostBurden Outliers
- **Issue**: `CostBurden` has a mean of 1.52 but a max of 505.0. 
- **Reason**: Small denominators in `RENT_TOTAL` (total occupied units) lead to extreme values when summing categorical counts. %% I don't understand what this means %%
- **Action**: 
    - Censor or remove tracts where `CostBurden > 3.0`.  %% update data cleaning process: if numerator or denominator == 0 →  %%
    - Filter out tracts with fewer than 10 occupied housing units.

### 2. Missing Values (NaN)
- **Issue**: `safe_div` returned `np.nan` for divisions by zero.
- **Counts**:
    - RiskyMobility: ~450 missing
    - CostBurden: ~480 missing
- **Action**: Impute with county-level means or remove if null count is high in specific regions. %% good idea %%

### 3. Housing Quality Metric
- **Issue**: Max value is 2.0 (sum of lack of plumbing + lack of kitchen). 
- **Note**: This double-counts households lacking both. 
- **Action**: Confirm if a binary "lacks at least one" metric is preferred, or normalize to 0-1 range. %% Check ACS variables. should be more variables in the table that will help get it right. %%

### 4. Negative Values / Bounds
- All other rates (Mobility, SNAP, etc.) are bounded [0, 1] as expected. No negative values found.

## Summary of Statistics
| Metric | Mean | Max |
|--------|------|-----|
| RiskyMobility | 0.17 | 1.00 |
| SnapRate | 0.13 | 1.00 |
| Unemployment | 0.05 | 1.00 |
| No_HS | 0.11 | 1.00 |
| HousingQuality| 0.04 | 2.00 |
| Poverty | 0.14 | 1.00 |
| CostBurden | 1.52 | 505.0 |
