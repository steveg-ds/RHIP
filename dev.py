from os import getenv

import numpy as np
import pandas as pd
from dotenv import load_dotenv

from utils import CensusDataLoader

load_dotenv()

acs_loader = CensusDataLoader(api_key=getenv("CENSUS_API_KEY"))

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

all_vars_dict = {}
for d in [
    MOBILITY_VARS,
    SNAP_VARS,
    EMPLOYMENT_VARS,
    EDUCATION_VARS,
    QUALITY_VARS,
    POVERTY_VARS,
    MORTGAGE_VARS,
    RENT_VARS,
    MISC_VARS,
]:
    for key, val in d.items():
        if isinstance(val, list):
            for i, item in enumerate(val):
                all_vars_dict[f"{key}_{i}"] = item
        else:
            all_vars_dict[key] = val

acs_data = acs_loader.fetch_multiple(variables=all_vars_dict)


def safe_div(num, den):
    return np.where(den == 0, np.nan, num / den)


# Rates
mobility_cols = [
    "MOVED_SAME_COUNTY",
    "MOVED_DIFF_COUNTY",
    "MOVED_DIFF_STATE",
    "MOVED_ABROAD",
]
acs_data["RiskyMobility"] = safe_div(
    acs_data[mobility_cols].sum(axis=1), acs_data["MOBILITY_TOTAL"]
)
acs_data["SnapRate"] = safe_div(acs_data["RECEIVING"], acs_data["SNAP_TOTAL"])
acs_data["Unemployment"] = safe_div(acs_data["UNEMPLOYED"], acs_data["LABOR_FORCE"])
edu_grades = [f"GRADES_{i}" for i in range(15)]
acs_data["No_HS"] = safe_div(acs_data[edu_grades].sum(axis=1), acs_data["EDU_TOTAL"])
acs_data["HousingQuality"] = safe_div(
    acs_data["LACK_PLUMBING"] + acs_data["LACK_KITCHEN"], acs_data["PLUMB_TOTAL"]
)
acs_data["Poverty"] = safe_div(acs_data["POVERTY"], acs_data["POV_TOTAL"])

mort_cols = [
    "MORT_30_35",
    "MORT_35_40",
    "MORT_40_50",
    "MORT_50_PLUS",
    "NO_MORT_30_35",
    "NO_MORT_35_40",
    "NO_MORT_40_50",
    "NO_MORT_50_PLUS",
]
rent_cols = ["RENT_30_35", "RENT_35_40", "RENT_40_50", "RENT_50_PLUS"]
acs_data["MortgageRisk"] = acs_data[mort_cols].sum(axis=1)
acs_data["RentRisk"] = acs_data[rent_cols].sum(axis=1)
acs_data["CostBurden"] = safe_div(
    acs_data["MortgageRisk"] + acs_data["RentRisk"],
    acs_data["RENT_TOTAL"] + acs_data["MORT_TOTAL"],
)

# Cleanup
raw_cols = [c for c in all_vars_dict.keys() if c != "GINI"]
acs_data = acs_data.drop(columns=raw_cols, errors="ignore")
print("Processing Complete.")
print(acs_data.head())
