# from pydantic import Field
from pydantic.dataclasses import dataclass
import pandas as pd
from .utils import MODEL_CONFIG


@dataclass(config=MODEL_CONFIG)
class DataProcessor:

    API_KEY: str
    """Census API Key"""
    DATA_PATH: str
    """Path to where data is stored"""

    def process_rucc_data(self) -> pd.DataFrame:
        data = pd.read_csv(f"{self.DATA_PATH}/RUCC.csv")
        data.drop(columns=["Population_2020", "Description"], inplace=True)
        data.rename(columns={"RUCC_2023": "RUCC"}, inplace=True)
        data["County_Name"] = data["County_Name"].str.replace("County", "")
        data["County_Name"] = data["County_Name"].str.replace("Parish", "")
        data["County_Name"] = data["County_Name"].str.strip()
        data.rename({"State": "state", "County_Name": "county"}, inplace=True)

        return data

    # Get census data for each state

    # C17002_001E: count of ratio of income to poverty in the past 12 months (total)
    # C17002_002E: count of ratio of income to poverty in the past 12 months (< 0.50)
    # C17002_003E: count of ratio of income to poverty in the past 12 months (0.50 - 0.99)
    # B01003_001E: total population
    # Sources: https://api.census.gov/data/2019/acs/acs5/variables.html

    def collect_acs_data(geo: bool = False):
        def get_state_data(
            geography: Literal["state", "county", "tract"],
            state: str,
            variables: Dict[str, str],
            geo: bool = True,
        ):
            try:
                return tc.get_acs(
                    geography=geography,
                    variables=variables,
                    state=state,
                    year=2023,
                    geometry=geo,
                )
            except Exception as e:
                print(f"{state}, {e}")
            else:
                return pd.DataFrame()

        vars = {
            "pov_under_50": "C17002_002E",
            "pov_under_100": "C17002_003E",
            "total_pop": "B01003_001E",
        }

        with ThreadPoolExecutor(max_workers=10) as executor:
            futures = []
            states = list(set(rucc["state"].values.tolist()))
            for state in states:
                # Submit the task to the executor. It returns a Future object.
                future = executor.submit(
                    get_state_data, "county", state, vars, geo=True
                )
                futures.append(future)
        census = pd.concat(
            [state_result.result() for state_result in futures],
            ignore_index=True,
            sort=False,
        )
        census["poverty_rate"] = (
            census[["pov_under_50", "pov_under_100"]].sum(axis=1) / census["total_pop"]
        )

    # uncomment next 2 lines to keep MOE columns
    cols_to_drop = [col for col in list(census.columns) if "_moe" in col]
    cols_to_drop.extend(["pov_under_50", "pov_under_100"])
    census.drop(columns=cols_to_drop, inplace=True)
    census["GEOID"] = pd.to_numeric(census["GEOID"], downcast="integer")

    return census


if __name__ == "__main__":
    import os
    from dotenv import load_dotenv

    load_dotenv()
    acs = DataProcessor(API_KEY=os.getenv("CENSUS_KEY"), DATA_PATH=os.getenv("DATA"))
    print(acs)
