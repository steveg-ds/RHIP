import pandas as pd


def collect_rucc_data(data_path: str) -> pd.DataFrame:
    data = pd.read_csv(f"{data_path}/RUCC.csv")
    data.drop(columns=["Population_2020", "Description"], inplace=True)
    data.rename(columns={"RUCC_2023": "RUCC"}, inplace=True)
    data["County_Name"] = data["County_Name"].str.replace("County", "").str.strip()
    data["County_Name"] = data["County_Name"].str.replace("Parish", "").str.strip()

    data.rename(columns={"State": "state", "County_Name": "county"}, inplace=True)

    return data
