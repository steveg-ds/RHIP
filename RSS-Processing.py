"""
dev.py — RSS Analysis data pipeline.
Script version of RSS-Analysis.ipynb.
Loads detention data, ACS demographics, RUCC codes; merges & derives vars.
"""

import os
import sys
from pathlib import Path

import geopandas as gpd
import numpy as np
import pandas as pd
from dotenv import load_dotenv

# Ensure project root is on path so utils import
sys.path.insert(0, str(Path(__file__).parent))
from utils import CensusDataLoader, ERSDataLoader

load_dotenv()

# ── Config ─────────────────────────────────────────────────────────────────
YEARS = [2013, 2018, 2023]
CACHE_PATH = Path(".data/RSS_ACS_DATA.csv")
DATA_DIR = Path("ice-detention-trends/facilities/by_fiscal_year")
METADATA_DIR = Path("ice-detention-trends/metadata")

# ── Init loaders ───────────────────────────────────────────────────────────
acs_loader = CensusDataLoader(api_key=os.getenv("CENSUS_API_KEY"))
ers_loader = ERSDataLoader(continental=True)

# ── 1. Load detention data ─────────────────────────────────────────────────
print("Loading detention data...")
dfs = []
for yr in YEARS:
    fp = DATA_DIR / f"FY{yr}.csv"
    if not fp.exists():
        print(f"  WARN: {fp} not found, skipping")
        continue
    df = pd.read_csv(fp)
    df["fiscal_year"] = yr
    dfs.append(df)

detention_data = pd.concat(dfs, ignore_index=True)

# Group by facility and year (some facilities have multiple rows per year)
detention_data = detention_data.groupby(
    ["detention_facility_code", "fiscal_year"]
).agg({"daily_pop": "sum", "midnight_pop": "sum"}).reset_index()

# Load facility metadata
facilities = pd.read_csv(METADATA_DIR / "facilities.csv")
detention_data = detention_data.merge(
    facilities[[
        "detention_facility_code", "detention_facility_name",
        "latitude", "longitude", "state", "county"
    ]],
    on="detention_facility_code",
    how="left",
)

# ── 2. Facility-level geometry → spatial join to counties ──────────────────
print("Spatial join facilities to counties...")
facilities_gdf = gpd.GeoDataFrame(
    facilities,
    geometry=gpd.points_from_xy(facilities.longitude, facilities.latitude),
    crs="EPSG:4326",
)

county_geo = acs_loader.collect_geometry_data(geometry_type="counties", year=2022)
county_geo["GEOID"] = county_geo["GEOID"].astype(int)

gdf_facilities = gpd.sjoin(
    facilities_gdf,
    county_geo[["GEOID", "NAMELSAD", "STATE_NAME", "geometry"]],
    how="left",
    predicate="within",
)

gdf_facilities = gdf_facilities.drop_duplicates(subset="detention_facility_code")
gdf_facilities = gdf_facilities[["detention_facility_code", "GEOID", "NAMELSAD", "STATE_NAME"]]
gdf_facilities = gdf_facilities.rename(columns={"NAMELSAD": "county_name", "STATE_NAME": "state_name"})

n_matched = gdf_facilities.GEOID.notna().sum()
print(f"  Facilities with county match: {n_matched} / {len(gdf_facilities)}")

# ── 3. ACS variables ───────────────────────────────────────────────────────
MOBILITY_VARS = {
    "MOBILITY_TOTAL": "B07012_002E",
    "SAME_HOUSE": "B07012_006E",
    "MOVED_SAME_COUNTY": "B07012_010E",
    "MOVED_DIFF_COUNTY": "B07012_014E",
    "MOVED_DIFF_STATE": "B07012_018E",
    "MOVED_ABROAD": "B07012_022E",
}
SNAP_VARS = {"SNAP_TOTAL": "B22003_001E", "RECEIVING": "B22003_002E"}
EMPLOYMENT_VARS = {"LABOR_FORCE": "B23025_002E", "UNEMPLOYED": "B23025_005E"}
EDUCATION_VARS = {
    "EDU_TOTAL": "B15003_001E",
    "GRADES": [f"B15003_{num:03d}E" for num in range(2, 17)],
}
QUALITY_VARS = {
    "PLUMB_TOTAL": "B25047_001E",
    "LACK_PLUMBING": "B25047_003E",
    "KITCH_TOTAL": "B25051_001E",
    "LACK_KITCHEN": "B25051_003E",
}
POVERTY_VARS = {"POV_TOTAL": "B17001_001E", "POVERTY": "B17001_002E"}
MORTGAGE_VARS = {
    "MORT_TOTAL": "B25091_001E",
    "MORT_30_35": "B25091_008E",
    "MORT_35_40": "B25091_009E",
    "MORT_40_50": "B25091_010E",
    "MORT_50_PLUS": "B25091_011E",
    "NO_MORT_30_35": "B25091_019E",
    "NO_MORT_35_40": "B25091_020E",
    "NO_MORT_40_50": "B25091_021E",
    "NO_MORT_50_PLUS": "B25091_022E",
}
RENT_VARS = {
    "RENT_TOTAL": "B25070_001E",
    "RENT_30_35": "B25070_007E",
    "RENT_35_40": "B25070_008E",
    "RENT_40_50": "B25070_009E",
    "RENT_50_PLUS": "B25070_010E",
}
MISC_VARS = {"GINI": "B19083_001E"}

# Flatten all var dicts into one lookup
all_vars_dict: dict[str, str] = {}
for d in [
    MOBILITY_VARS, SNAP_VARS, EMPLOYMENT_VARS, EDUCATION_VARS,
    QUALITY_VARS, POVERTY_VARS, MORTGAGE_VARS, RENT_VARS, MISC_VARS,
]:
    for key, val in d.items():
        if isinstance(val, list):
            for i, item in enumerate(val):
                all_vars_dict[f"{key}_{i}"] = item
        else:
            all_vars_dict[key] = val

# ── 4. Fetch ACS data (with caching) ───────────────────────────────────────
if CACHE_PATH.exists():
    print("Loading cached ACS data from", CACHE_PATH)
    acs_data = pd.read_csv(CACHE_PATH)
else:
    print("Fetching ACS data from Census API...")
    acs_data = acs_loader.fetch_multiple_years(
        years=YEARS,
        variables=all_vars_dict,
        geography="county",
    )
    CACHE_PATH.parent.mkdir(exist_ok=True)
    acs_data.to_csv(CACHE_PATH, index=False)
    print(f"Saved ACS data to {CACHE_PATH}")

acs_data["GEOID"] = pd.to_numeric(acs_data["GEOID"], errors="coerce").astype("Int64")

# ── 5. Fetch RUCC codes ────────────────────────────────────────────────────
print("Fetching RUCC codes...")
rucc_data = ers_loader.collect_rucc_data()
rucc_data["GEOID"] = rucc_data["GEOID"].astype(int)

# ── 6. Merge everything ────────────────────────────────────────────────────
print("Merging datasets...")

# Step 6a: Attach facility GEOID
gdf_facilities["GEOID"] = gdf_facilities["GEOID"].fillna(0).astype(int)
gdf_facilities = gdf_facilities[gdf_facilities["GEOID"] > 0]

detention_final = detention_data.merge(
    gdf_facilities[["detention_facility_code", "GEOID", "county_name", "state_name"]],
    on="detention_facility_code",
    how="inner",
)

print(f"  Detention records with county match: {len(detention_final)}")

# Step 6b: Merge RUCC
detention_final = detention_final.merge(
    rucc_data[["GEOID", "RUCC"]],
    on="GEOID",
    how="left",
)

# Step 6c: Rename fiscal_year → year for ACS merge
detention_final = detention_final.rename(columns={"fiscal_year": "year"})

# Step 6d: Merge ACS demographics
detention_final = detention_final.merge(
    acs_data,
    on=["GEOID", "year"],
    how="left",
)

# ── 7. Derive ACS variables ────────────────────────────────────────────────
print("Deriving ACS variables...")

hs_grades = [
    "GRADES_0", "GRADES_1", "GRADES_2", "GRADES_3", "GRADES_4",
    "GRADES_5", "GRADES_6", "GRADES_7", "GRADES_8", "GRADES_9",
    "GRADES_10", "GRADES_11", "GRADES_12",
]
cost_cols = [
    "MORT_30_35", "MORT_35_40", "MORT_40_50", "MORT_50_PLUS",
    "NO_MORT_30_35", "NO_MORT_35_40", "NO_MORT_40_50", "NO_MORT_50_PLUS",
    "RENT_30_35", "RENT_35_40", "RENT_40_50", "RENT_50_PLUS",
]


detention_final["RiskyMobility"] = (
    detention_final["MOVED_DIFF_COUNTY"] + detention_final["MOVED_DIFF_STATE"]
    + detention_final["MOVED_ABROAD"]
) / detention_final["MOBILITY_TOTAL"].replace(0, np.nan)

detention_final["SnapRate"] = (
    detention_final["RECEIVING"] / detention_final["SNAP_TOTAL"].replace(0, np.nan)
)

detention_final["Unemployment"] = (
    detention_final["UNEMPLOYED"] / detention_final["LABOR_FORCE"].replace(0, np.nan)
)

detention_final["No_HS"] = (
    detention_final[hs_grades].sum(axis=1)
    / detention_final["EDU_TOTAL"].replace(0, np.nan)
)

detention_final["HousingQuality"] = (
    (detention_final["LACK_PLUMBING"] + detention_final["LACK_KITCHEN"])
    / np.maximum(detention_final["PLUMB_TOTAL"], detention_final["KITCH_TOTAL"]).replace(0, np.nan)
)

detention_final["Poverty"] = (
    detention_final["POVERTY"] / detention_final["POV_TOTAL"].replace(0, np.nan)
)

detention_final["CostBurden"] = (
    detention_final[cost_cols].sum(axis=1)
    / (detention_final["MORT_TOTAL"] + detention_final["RENT_TOTAL"]).replace(0, np.nan)
)

# ── 8. Final column selection ──────────────────────────────────────────────
keep_cols = [
    "detention_facility_code", "county_name", "state_name", "year",
    "GEOID", "RUCC",
    "daily_pop", "midnight_pop", 
    "RiskyMobility", "SnapRate", "Unemployment", "No_HS",
    "HousingQuality", "Poverty", "CostBurden",
]
detention_final = detention_final[keep_cols]
detention_final = detention_final.rename(columns={"RUCC": "rucc_code"})

# ── 9. Output ──────────────────────────────────────────────────────────────
OUT_PATH = Path(".data/RSS_ANALYSIS_FINAL.csv")
detention_final.to_csv(OUT_PATH, index=False)

print(f"\nDone. {len(detention_final)} records written to {OUT_PATH}")
print(f"  GEOID dtype: {detention_final.GEOID.dtype}")
print(f"  Columns: {list(detention_final.columns)}")
print(detention_final.head())