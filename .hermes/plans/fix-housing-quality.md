# Fix HousingQuality Metric

- Current: `(LackPlumb + LackKitch) / PlumbTotal` -> Denominator mismatch.
- Fix:
  - Calc `PlumbRate = LackPlumb / PlumbTotal`
  - Calc `KitchRate = LackKitch / KitchTotal`
  - Metric = `(PlumbRate + KitchRate) > 0.05` (Binary)
- Outliers: Replace `NaN` with 0 or drop.
