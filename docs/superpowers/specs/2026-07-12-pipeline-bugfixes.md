# Pipeline Bugfix Specification

**Goal:** Fix derived variable bugs (No_HS, HousingQuality) and investigate root cause of ~43% ACS null merge in the RSS data pipeline.

**Scope:** 3 fixes to RSS-Processing.py, plus investigation of ACS merge failures and zero-population validation.

## Background

The RSS pipeline merges detention facility data with ACS demographics by (GEOID, year). ~43% of records have null ACS-derived variables. An audit identified two confirmed formula bugs and a need to investigate the merge failure root cause.

## Design Decisions

### GEOID stays as integer

GEOID is deliberately kept as integer throughout the pipeline. The `CensusDataLoader` supports multiple geography levels (tract, county, state) with varying FIPS code lengths, and integer is the appropriate canonical type. The high ACS null rate is **NOT** caused by zero-padding — investigate the actual cause.

### ACS Null Merge Investigation

The merge failure (1913/4419 null rows) likely stems from one or more of:
- **Coverage gap:** 110 GEOIDs in final data have zero matches in the ACS cache at any year — these counties may not have been included in the ACS fetches
- **Year mismatch:** Facility records in detention data may exist for years where ACS data wasn't fetched for that specific county
- **Spatial join misses:** Facilities not matched to a county (GEOID=0 or NaN) will fail to merge with ACS entirely

## Bugs to Fix

### Bug 1: No_HS Missing 12th-Grade-No-Diploma

`hs_grades` list at `RSS-Processing.py:199-203` includes GRADES_0 through GRADES_11 but omits GRADES_12 (12th grade, no diploma). ACS variable B15003_014E represents people who have not completed high school.

- [-] make sure that grade variables are validated before updating dictionaries

**Fix:** Add `"GRADES_12"` to the `hs_grades` list.

### Bug 2: HousingQuality Denominator Halved

Rephrased. The ACS table used for PLUMB_TOTAL (B25047) covers all housing units, and KITCH_TOTAL (B25051) also covers all housing units. These describe the same universe. Summing them doubles the denominator.

**Fix:** Use `np.maximum(PLUMB_TOTAL, KITCH_TOTAL)` instead of sum, or switch to separate plumbing/kitchen rates.

### CostBurden: Correct, no change

## Investigation Tasks

### Investigate ACS Merge Failure

Diagnose why (GEOID, year) merge produces 43% nulls. Check:
- Which GEOIDs in detention data have no matching ACS record (any year)
- Whether null records share a common GEOID pattern, year pattern, or state
- Whether the ACS cache actually contains those counties

### Validate Zero-Population Records

814/1452 county-years have daily_pop=midnight_pop=0. Check source FY*.csv to confirm legitimacy.

## Verification

After fixes:
1. No_HS mean slightly increases due to GRADES_12 inclusion
2. HousingQuality mean approximately doubles (~0.057)
3. ACS merge failure root cause documented
4. Zero-pop pattern confirmed or flagged
