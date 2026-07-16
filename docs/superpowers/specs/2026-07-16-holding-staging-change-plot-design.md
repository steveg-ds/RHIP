# Design Specification: Holding/Staging Facility Changes per RUCC

## Overview
The goal is to create a new bar plot in the existing Jupyter Notebook (`RSS-Analysis.ipynb`) that visualizes the net change in the number of "Hold/Staging" detention facilities across different Rural-Urban Continuum Codes (RUCC) over two distinct time periods: 2013-2018 and 2018-2023.

## Data Preparation
1. **Source Data**: The existing `active` DataFrame (which already filters out facilities with `daily_pop == 0`).
2. **Filtering**: Subset the data to only include rows where `type_grouped == 'Hold/Staging'`.
3. **Aggregation**:
   - Group the subset by `RUCC` and `year`.
   - Count the number of unique facilities (`detention_facility_code`).
   - Unstack the `year` index to create distinct columns for `2013`, `2018`, and `2023`.
   - Fill any missing (`NaN`) values with `0` (meaning no facilities existed in that RUCC category for that year).
4. **Delta Calculation**:
   - Create a column `Change 2013-2018` calculated as `2018 - 2013`.
   - Create a column `Change 2018-2023` calculated as `2023 - 2018`.
   - Isolate these two change columns into a new plotting DataFrame.

## Visualization
1. **Plot Type**: Grouped Bar Chart (`kind='bar'`).
2. **Library**: `matplotlib.pyplot`.
3. **Axes**:
   - **X-axis**: RUCC codes (1 = most urban, 9 = most rural).
   - **Y-axis**: Net change in number of Hold/Staging facilities (can extend into negative values).
4. **Aesthetics & Readability**:
   - Draw a clear horizontal dashed line at `y=0` (`axhline`) to delineate growth vs. decline.
   - Use distinct colors for the two time periods (e.g., cool vs. warm, or sequential colormap) to easily differentiate the bars.
   - Add a descriptive title, axis labels, and a legend.
   - Apply a subtle `y`-axis grid (`plt.grid(axis='y', linestyle='--', alpha=0.7)`).
5. **Output Location**: Insert a new code cell directly following the current facility type line plot in `RSS-Analysis.ipynb`.

## Edge Cases Handled
- Missing RUCC codes for certain years (filled with 0).
- RUCC categories that have no Hold/Staging facilities in any year (will show as 0 change).

## Dependencies
- `pandas`
- `matplotlib.pyplot`
