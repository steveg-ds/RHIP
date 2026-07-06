---
name: working-with-pytidycensus
description: Use when fetching, processing, or querying US Census or American Community Survey (ACS) data using the pytidycensus library in Python.
---

# Working with pytidycensus

## Overview
`pytidycensus` is a Python port of R's `tidycensus` package, providing interfaces to retrieve US Census and ACS data as pandas DataFrames or geopandas GeoDataFrames.

## Key Differences from R tidycensus
1. **Wide Default**: Unlike R `tidycensus` (which defaults to tidy format), `pytidycensus` defaults to `output="wide"`.
2. **Forced Wide with Geometry**: Passing `geometry=True` internally forces `output="wide"`. Any `output="tidy"` argument is ignored.

## Output Schema Behaviors

### Tidy Output (`output="tidy"`, `geometry=False`)
Columns: `GEOID`, `NAME`, `variable`, `estimate`, `moe`.
- Renamed variables (e.g. `variables={"custom_name": "B19013_001"}`) map the values in the `variable` column to `"custom_name"` and `"custom_name_moe"`.

### Wide Output (`output="wide"` or `geometry=True`)
Columns: `GEOID`, `NAME`, geography columns (e.g., `state`, `county`), and separate columns for each variable.
- Default variable names are appended with `E` (Estimate) and `_moe` (Margin of Error). Example: `B19013_001E` and `B19013_001_moe`.
- Renamed variables (e.g. `variables={"custom_name": "B19013_001"}`) create columns named directly after the keys: `"custom_name"` (for estimate) and `"custom_name_moe"` (for margin of error).
- **No `'variable'` or `'estimate'` columns exist in wide output.**

## GEOID Structure & Spatial Aggregation
GEOID string formats segment as:
- **Digits 1-2**: State FIPS code
- **Digits 3-5**: County FIPS code (use `df['GEOID'].str[:5]` to extract the full 5-digit county FIPS)
- **Digits 6-11**: Census Tract
- **Digit 12**: Block Group parent
- **Digits 13-15**: Block

### Common Pattern: Tract to County Aggregation
To aggregate tract-level geometry and data up to the county level, extract the 5-digit county FIPS and use GeoPandas `dissolve`:
```python
# Extract county FIPS
df['COUNTYFP'] = df['GEOID'].str[:5]

# Dissolve tracts into counties, summing estimates
county_df = df.dissolve(
    by='COUNTYFP',
    aggfunc={'income_est': 'sum', 'pop_est': 'sum'}
)
```

## Quick Reference
```python
import pytidycensus as tc

# Setup API Key
tc.set_census_api_key("YOUR_API_KEY") # Or set CENSUS_API_KEY env var

# Get ACS Data (returns wide DataFrame by default)
df = tc.get_acs(
    geography="county",
    variables={"median_income": "B19013_001"},
    state="TX",
    year=2022
)
# Columns: ['GEOID', 'NAME', 'median_income', 'median_income_moe']
```

## Common Pitfalls
- **KeyError: 'variable' / 'estimate'**: Occurs when trying to query tidy-format columns on wide-format data (common when `geometry=True`).
- **ACS Variable Suffixes**: `get_acs` automatically appends `"E"` to variables if not present, and fetches the corresponding `"M"` variable.
- **National Geographies**: ZCTAs (`geography="zcta"`) cannot be filtered by `state`. Use the `zcta` parameter instead.
- **Block Group Limitations**: Block groups are not available for Data Profile (`DP`) or Subject (`S`) tables.
- **2020 1-Year ACS**: Not available due to COVID-19 response rates; calling it raises a `ValueError`.
