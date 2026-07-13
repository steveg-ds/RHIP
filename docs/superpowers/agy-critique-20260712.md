# agy Implementation Critique — Pipeline Bugfixes

**Date:** 2026-07-12  
**agy model:** Gemini 3.1 Pro (High)  
**Tasks attempted:** No_HS fix, HousingQuality fix, ACS merge investigation, zero-pop validation

---

## ✅ What agy Got Right

### 1. No_HS — Added GRADES_12 ✓
- Successfully edited `RSS-Processing.py:199-203`, adding `"GRADES_12"` to the `hs_grades` list
- Verified with AST parsing that the change was correct

### 2. HousingQuality — Changed to np.maximum() ✓
- Successfully edited `RSS-Processing.py:229-232`, replacing `PLUMB_TOTAL + KITCH_TOTAL` with `np.maximum(PLUMB_TOTAL, KITCH_TOTAL)`
- Syntax verified clean

### 3. ACS Merge Investigation — Identified Connecticut Issue ✓
- Correctly spotted that Connecticut has non-matching GEOIDs between final (9110, 9180 = planning regions) and ACS cache (traditional county FIPS like 09001)
- Correct count: 1913 null rows (43.3%), 110 missing GEOIDs, 768 unmatched (GEOID,year) pairs

---

## ❌ What agy Got Wrong

### 1. CLAIMED: "10 states completely missing from ACS cache" — FALSE
agy claimed California and Arizona were missing. Reality: **42 states in ACS cache**, including CA and AZ. The `state` column is stored as FIPS codes (06=CA, 04=AZ), not state names. agy failed to check the FIPS `state` column and instead looked for a `STATE_NAME` column that doesn't exist in the ACS cache. This misdiagnosis wasted time.

### 2. CLAIMED: "Texas only has 2023 in ACS" — FALSE
agy's state-by-year analysis was incomplete/unverified. The ACS cache covers all 3 years (2013, 2018, 2023) for most states. No evidence of partial year coverage except for states that entered the pipeline late.

### 3. Wrote Diagnostic to Brain File — NOT TO PROJECT
Saved findings to `~/.gemini/antigravity-cli/brain/.../analysis_results.md` instead of a project-relative path. This is non-portable and wouldn't be committed.

### 4. Did NOT Run Zero-Pop Validation
Task 4 (zero-pop validation) was never attempted. agy returned from the ACS investigation and didn't proceed to the next task.

### 5. No File Edits Were Syntax-Checked Against the Running Code
agy didn't test whether `np.maximum().replace()` works correctly. (It does — NumPy ufuncs return ndarrays which have a `.replace()` alias? Actually — ndarrays don't have `.replace()`. This will **crash at runtime**.)

---

## ✅ Actually Fine: np.maximum().replace() Works

I flagged this as a potential runtime crash, but `np.maximum(pandas_series, pandas_series)` returns a **pandas Series**, not a bare numpy ndarray. So `.replace(0, np.nan)` is valid and will work correctly. No fix needed.

---

## 🔍 ACS Merge Failure — Verified Root Causes

### Cause 1: Connecticut Planning Region GEOIDs
- Census 2022 geometry files use new CT planning regions (GEOID 9110, 9180) instead of traditional county FIPS (09001-09015)
- ACS cache (fetched 2013, 2018, 2023 via tidycensus) uses the old county FIPS
- Result: 12 CT records (all years) never merge — *all* CT records are null

### Cause 2: 4-Digit GEOIDs (Integer Truncation)
- 861 final rows, 492 ACS rows have 4-digit GEOIDs due to int parsing dropping leading zeros
- This creates false mismatches: GEOID 8001 (final) vs 08001 (ACS) won't match
- User explicitly wants GEOID to stay as int, so this is accepted behavior — but it does contribute to merge failures

### Cause 3: 110 Unique GEOIDs Missing from ACS Altogether
- These likely represent detention facilities in counties that were in the facilities CSV but not covered by the ACS fetch (possibly non-standard counties, pseudo-counties, or territories)
- Need to identify which 110 GEOIDs these are and whether they represent real ACS coverage gaps

### Cause 4: (GEOID,year) Pair Mismatches
- 768 out of 2220 final pairs don't exist in ACS
- Major contributors: CT issue × 3 years × 2 GEOIDs, truncated GEOIDs, and missing GEOIDs

---

## Remaining Work / Unresolved Items

### 1. np.maximum().replace() — Confirmed Working
Verified: `np.maximum(pd_series, pd_series)` returns a pandas Series, so `.replace(0, np.nan)` works correctly. No fix needed.

### 2. Delete Old Final CSV and Re-run
Must delete `.data/RSS_ANALYSIS_FINAL.csv` and uncomment line 256 in RSS-Processing.py to regenerate.

### 3. GRADES_12 Fix — Need to Verify at Pipeline Level
Confirm that adding GRADES_12 changes No_HS mean measurably (expected: ~0.152 → ~0.162).

### 4. Zero-Pop Validation Complete — 601 Exclusively Zero Facilities
Validation showed 601 facilities are zero-pop across ALL 3 years sampled. This is likely legitimate (hospitals, juvenile centers, BOP facilities not used for ICE detention in those years). Not a pipeline bug.

### 5. Connecticut GEOID Mismatch — Architectural Issue
Need to decide: should the spatial join use a year whose geometry matches what tidycensus returns for ACS? Or should the geometry be pinned to pre-2022 CT county definitions?

### 6. Write to CSV Output is Commented Out
`RSS-Processing.py:256` has `# detention_final.to_csv(OUT_PATH, index=False)` — uncomment to generate output.
