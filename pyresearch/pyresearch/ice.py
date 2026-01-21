import pandas as pd
import os
import glob


def merge_ice_data(data_path: str) -> pd.DataFrame:
    print("Processing facilities data")
    facilities = pd.read_csv(
        f"{data_path}/ice-detention-trends/metadata/facilities.csv"
    )
    facilities.drop(columns=["address", "city", "zip"], inplace=True)

    facilities.rename(columns={"FIPS": "GEOID"}, inplace=True)
    facilities.drop(columns=["detention_facility_name", "state"], inplace=True)
    facilities.sort_values("county", inplace=True)

    print("Processing state detention data")
    state_dir = f"{data_path}/ice-detention-trends/facilities/by_state"
    file_pattern = os.path.join(state_dir, "*.csv")
    csvs = glob.glob(file_pattern)
    state_data = pd.concat(
        [pd.read_csv(csv) for csv in csvs], ignore_index=True, sort=False
    )
    state_data["date"] = pd.to_datetime(state_data["date"])

    df = pd.merge(
        state_data,
        facilities,
        on="detention_facility_code",
        how="left",
    )
    df["year"] = df["date"].dt.year

    df["daily_change"] = df["daily_pop"] - df["midnight_pop"]

    return df
