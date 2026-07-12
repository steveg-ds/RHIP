# Pipeline Bugfix Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use `subagent-driven-development` or `executing-plans` to implement this plan task-by-task. Tasks use checkbox syntax.
> **Execution method:** One interactive `agy` session using `/cavecrew` to delegate sub-tasks.

**Goal:** Fix No_HS and HousingQuality formula bugs, investigate ACS merge failure root cause, validate zero-pop records.

**Architecture:** Patch RSS-Processing.py (2 formula fixes) + run investigation scripts to diagnose ACS merge issues and zero-pop patterns. GEOID stays integer — no zero-padding changes.

**Tech Stack:** Python, Pandas, NumPy, Agy CLI, CaveCrew

## Global Constraints

- GEOID remains integer throughout — do NOT change GEOID dtype handling
- ACS cache at `.data/RSS_ACS_DATA.csv` is read-only (diagnose, don't delete)
- Final output: `.data/RSS_ANALYSIS_FINAL.csv`
- Re-run pipeline after fixes to verify

---

### Task 1: Validate GRADES Variables Before Any Changes

**Files:** Read-only: `RSS-Processing.py:98-101` (GRADES variable mapping), `.data/RSS_ACS_DATA.csv`

- [ ] **Validate the GRADES variable-to-column mapping**

The pipeline fetches EDUCATION_VARS where GRADES maps to ACS codes B15003_002E through B15003_016E:
```python
EDUCATION_VARS = {
    "EDU_TOTAL": "B15003_001E",
    "GRADES": [f"B15003_{num:03d}E" for num in range(2, 17)],
}
```

This auto-generates column names `GRADES_0` through `GRADES_14`:
- B15003_002E = GRADES_0 (less than 1st grade)
- B15003_003E = GRADES_1 ... up to B15003_014E = GRADES_12 (12th grade, no diploma)
- B15003_015E = GRADES_13 (high school graduate — correctly excluded from hs_grades)
- B15003_016E = GRADES_14 (some college — correctly excluded)

Run a quick check that the fetched data has all expected columns:
```bash
cd /mnt/ssd/Projects/RHIP
python3 << 'EOF'
import pandas as pd
acs = pd.read_csv('.data/RSS_ACS_DATA.csv')
grade_cols = [c for c in acs.columns if c.startswith('GRADES_')]
print(f"Found {len(grade_cols)} GRADES columns: {sorted(grade_cols)}")
print(f"GRADES_12 present: {'GRADES_12' in grade_cols}")
for c in sorted(grade_cols):
    print(f"  {c}: non-null={acs[c].notna().sum()}, mean={acs[c].mean():.0f}")
EOF
```

- [ ] **Verify No_HS formula uses the right columns**

```bash
cd /mnt/ssd/Projects/RHIP
python3 << 'EOF'
final = pd.read_csv('.data/RSS_ANALYSIS_FINAL.csv')
hs_cols = [c for c in final.columns if c.startswith('GRADES_')]
print(f"GRADES columns in final: {sorted(hs_cols)}")
print(f"Current mean No_HS: {final.No_HS.mean():.4f}")

if 'GRADES_12' in final.columns:
    grades_0_to_12 = [c for c in hs_cols if c <= 'GRADES_12']
    test_val = (final[grades_0_to_12].sum(axis=1) / final.EDU_TOTAL.replace(0, float('nan'))).mean()
    print(f"Test mean with GRADES_12: {test_val:.4f}")
EOF
```

---

### Task 2: Fix No_HS Numerator (Add GRADES_12)

**Files:**
- Modify: `RSS-Processing.py:199-203`

- [ ] **Fix hs_grades list**

```python
# Change from:
hs_grades = [
    "GRADES_0", "GRADES_1", "GRADES_2", "GRADES_3", "GRADES_4",
    "GRADES_5", "GRADES_6", "GRADES_7", "GRADES_8", "GRADES_9",
    "GRADES_10", "GRADES_11",
]

# Change to:
hs_grades = [
    "GRADES_0", "GRADES_1", "GRADES_2", "GRADES_3", "GRADES_4",
    "GRADES_5", "GRADES_6", "GRADES_7", "GRADES_8", "GRADES_9",
    "GRADES_10", "GRADES_11", "GRADES_12",
]
```

- [ ] **Verify** — `grep "GRADES_12" RSS-Processing.py` should show it in hs_grades

---

### Task 3: Fix HousingQuality Denominator

**Files:**
- Modify: `RSS-Processing.py:229-232`

- [ ] **Change denominator from sum to max()**

```python
# Change from:
detention_final["HousingQuality"] = (
    (detention_final["LACK_PLUMBING"] + detention_final["LACK_KITCHEN"])
    / (detention_final["PLUMB_TOTAL"] + detention_final["KITCH_TOTAL"]).replace(0, np.nan)
)

# Change to:
detention_final["HousingQuality"] = (
    (detention_final["LACK_PLUMBING"] + detention_final["LACK_KITCHEN"])
    / np.maximum(detention_final["PLUMB_TOTAL"], detention_final["KITCH_TOTAL"]).replace(0, np.nan)
)
```

- [ ] **Verify syntax** — `python -c "import ast; ast.parse(open('RSS-Processing.py').read()); print('OK')"`

---

### Task 4: Investigate ACS Merge Failure

**Files:** Read-only: `.data/RSS_ACS_DATA.csv`, `.data/RSS_ANALYSIS_FINAL.csv`

- [ ] **Run investigation script**

```bash
cd /mnt/ssd/Projects/RHIP
python3 << 'EOF'
import pandas as pd
import numpy as np

final = pd.read_csv('.data/RSS_ANALYSIS_FINAL.csv')
acs = pd.read_csv('.data/RSS_ACS_DATA.csv')

print(f"=== DATA OVERVIEW ===")
print(f"Final records: {len(final)}")
print(f"ACS records: {len(acs)}")
print(f"Final GEOID type: {final.GEOID.dtype}")
print(f"ACS GEOID type: {acs.GEOID.dtype}")

# Check GEOID overlap
final_geoids = set(final.GEOID.unique())
acs_geoids = set(acs.GEOID.unique())
print(f"Unique GEOIDs in final: {len(final_geoids)}")
print(f"Unique GEOIDs in ACS: {len(acs_geoids)}")
missing = final_geoids - acs_geoids
print(f"GEOIDs in final NOT in ACS cache: {len(missing)}")
if missing:
    print(f"  Sample: {sorted(missing)[:10]}")

# Check null rows
acs_cols = ['RiskyMobility', 'SnapRate', 'Unemployment', 'No_HS', 'HousingQuality', 'Poverty', 'CostBurden']
existing = [c for c in acs_cols if c in final.columns]
null_rows = final[existing].isna().all(axis=1)
null_rate = null_rows.mean()
print(f"\n=== NULL ANALYSIS ===")
print(f"All-ACS-null rows: {null_rows.sum()} / {len(final)} ({100*null_rate:.1f}%)")

# What do null rows look like?
null_df = final[null_rows]
print(f"\nNull GEOID range: {null_df.GEOID.min()} - {null_df.GEOID.max()}")
print(f"Null year distribution:")
print(null_df.year.value_counts().sort_index())

# Check years present
print(f"\n=== YEAR COVERAGE ===")
print(f"Years in final: {sorted(final.year.unique())}")
print(f"Years in ACS: {sorted(acs.year.unique())}")

# (GEOID,year) pairs in final that don't exist in ACS
final_pairs = set(zip(final.GEOID, final.year))
acs_pairs = set(zip(acs.GEOID, acs.year))
unmatched = final_pairs - acs_pairs
print(f"(GEOID,year) pairs in final NOT in ACS: {len(unmatched)} / {len(final_pairs)} ({100*len(unmatched)/len(final_pairs):.1f}%)")
EOF
```

- [ ] **Dig into root cause of missing GEOIDs**

```bash
cd /mnt/ssd/Projects/RHIP
python3 << 'EOF'
import pandas as pd

final = pd.read_csv('.data/RSS_ANALYSIS_FINAL.csv')
acs = pd.read_csv('.data/RSS_ACS_DATA.csv')

final_geoids = set(final.GEOID.unique())
acs_geoids = set(acs.GEOID.unique())
missing = final_geoids - acs_geoids

# Show details of missing GEOIDs
missing_df = final[final.GEOID.isin(missing)]
print("=== MISSING GEOID DETAILS ===")
print(f"Records with missing GEOIDs: {len(missing_df)}")
print(f"\nBy state:")
print(missing_df.groupby('state_name').size().sort_values(ascending=False))
print(f"\nBy year:")
print(missing_df.groupby('year').size())
print(f"\nSample (first 20):")
print(missing_df[['detention_facility_code', 'county_name', 'state_name', 'GEOID', 'year']].head(20).to_string())

# Also check what GEOIDs are in ACS cache
print(f"\n=== ACS CACHE GEOID CHECK ===")
print(f"ACS GEOID range: {acs.GEOID.min()} - {acs.GEOID.max()}")
print(f"ACS GEOID dtype: {acs.GEOID.dtype}")
short = acs[acs.GEOID.astype(str).str.len() < 5]
print(f"4-digit GEOIDs in ACS: {len(short)}")
if len(short) > 0:
    print(f"  Sample: {short.GEOID.head().tolist()}")

short_final = final[final.GEOID.astype(str).str.len() < 5]
print(f"4-digit GEOIDs in final: {len(short_final)}")
if len(short_final) > 0:
    print(f"  Sample: {short_final.GEOID.head().tolist()}")
EOF
```

- [ ] **Check spatial join misses**

```bash
cd /mnt/ssd/Projects/RHIP
python3 << 'EOF'
import pandas as pd
final = pd.read_csv('.data/RSS_ANALYSIS_FINAL.csv')

# If GEOID=0 or NaN exists, spatial join failed for those facilities
zero_geoid = final[final.GEOID == 0]
print(f"GEOID=0 records: {len(zero_geoid)}")
if len(zero_geoid) > 0:
    print(zero_geoid[['detention_facility_code', 'county_name', 'state_name', 'year']].head(10).to_string())

null_geoid = final[final.GEOID.isna()]
print(f"\nGEOID=NaN records: {len(null_geoid)}")
EOF
```

- [ ] **Report findings** — consolidate into a clear diagnosis

---

### Task 5: Validate Zero-Population Records

**Files:** Read-only: `ice-detention-trends/facilities/by_fiscal_year/FY2013.csv`, FY2018.csv, FY2023.csv

- [ ] **Run zero-pop validation**

```bash
cd /mnt/ssd/Projects/RHIP
python3 << 'SCRIPT'
import pandas as pd
from pathlib import Path

data_dir = Path("ice-detention-trends/facilities/by_fiscal_year")
facilities = pd.read_csv("ice-detention-trends/metadata/facilities.csv")

years = [2013, 2018, 2023]
results = []

for yr in years:
    fp = data_dir / f"FY{yr}.csv"
    df = pd.read_csv(fp)
    by_fac = df.groupby("detention_facility_code").agg(
        daily_pop=("daily_pop", "sum"),
        midnight_pop=("midnight_pop", "sum")
    ).reset_index()
    by_fac["fiscal_year"] = yr
    results.append(by_fac)

all_data = pd.concat(results, ignore_index=True)

zero_pops = all_data[(all_data.daily_pop == 0) & (all_data.midnight_pop == 0)]
print(f"=== ZERO POP VALIDATION ===")
print(f"Total facility-years: {len(all_data)}")
print(f"Zero-pop facility-years: {len(zero_pops)} ({100*len(zero_pops)/len(all_data):.1f}%)")

by_year = zero_pops.groupby("fiscal_year").size()
print(f"\nBy year:")
print(by_year.to_string())

# Merge facility metadata
zp_merged = zero_pops.merge(facilities[["detention_facility_code", "state"]], on="detention_facility_code", how="left")
by_state = zp_merged.groupby("state").size().sort_values(ascending=False)
print(f"\nBy state (top 15):")
print(by_state.head(15).to_string())

# Check if any facility is ALWAYS zero
fac_zero_count = zero_pops.groupby("detention_facility_code").size()
always_zero = fac_zero_count[fac_zero_count == len(years)]
print(f"\nFacilities with zero pop ALL years: {len(always_zero)}")
if len(always_zero) > 0:
    az = facilities[facilities.detention_facility_code.isin(always_zero.index)]
    print(az[["detention_facility_code", "detention_facility_name", "state", "county"]].head(10).to_string())

# Check if facility has ANY non-zero records
all_fac_codes = set(all_data.detention_facility_code.unique())
zero_fac_codes = set(zero_pops.detention_facility_code.unique())
nonzero_fac_codes = set(all_data.loc[~all_data.index.isin(zero_pops.index), "detention_facility_code"].unique())
print(f"\nFacilities with at least one non-zero year: {len(nonzero_fac_codes)}")
exclusively_zero = zero_fac_codes - nonzero_fac_codes
print(f"Facilities EXCLUSIVELY zero-pop: {len(exclusively_zero)}")
if len(exclusively_zero) > 0:
    ez = facilities[facilities.detention_facility_code.isin(exclusively_zero)]
    print(ez[["detention_facility_code", "detention_facility_name", "state"]].to_string())
SCRIPT
```

- [ ] **Report findings** — is this legitimate or a bug?

---

### Task 6: Re-run Pipeline and Verify

- [ ] **Re-run pipeline**

```bash
cd /mnt/ssd/Projects/RHIP
# Uncomment the output line in RSS-Processing.py if needed (line 256)
# Then run:
python RSS-Processing.py
```

- [ ] **Verify No_HS and HousingQuality changed as expected**

```bash
cd /mnt/ssd/Projects/RHIP
python3 << 'EOF'
import pandas as pd
df = pd.read_csv('.data/RSS_ANALYSIS_FINAL.csv')
print(f"Total: {len(df)}")
print(f"No_HS mean: {df.No_HS.mean():.4f} (expect ~0.13-0.17, slightly higher than before)")
print(f"HousingQuality mean: {df.HousingQuality.mean():.4f} (expect ~0.05, about 2x previous)")
print(f"CostBurden mean: {df.CostBurden.mean():.4f} (expect ~0.30, unchanged)")

acs_cols = ['RiskyMobility', 'SnapRate', 'Unemployment', 'No_HS', 'HousingQuality', 'Poverty', 'CostBurden']
null_rate = df[acs_cols].isna().all(axis=1).mean()
print(f"All-ACS-null rate: {100*null_rate:.1f}%")
EOF
```

---

### Task 7: Commit

- [ ] **Commit changes**

```bash
cd /mnt/ssd/Projects/RHIP
git add RSS-Processing.py
git commit -m "fix: add GRADES_12 to No_HS, use max() for HousingQuality denominator"
```
