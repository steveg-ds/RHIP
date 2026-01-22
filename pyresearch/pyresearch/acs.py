# Get census data for each state
import pandas as pd
import pytidycensus as tc
from concurrent.futures import ThreadPoolExecutor
from typing import Dict, Literal, List, Union


def collect_acs_data(
    API_KEY: str,
    geography: Literal["state", "county", "tract"],
    year: int,
    states: Union[pd.Series, List[str]],
    vars: Dict[str, str],
    max_workers: int = 10,
    geometry: bool = False,
    moe: bool = False,
):
    def get_state_data(
        geography: str,
        year: int,
        state: str,
        variables: Dict[str, str],
        geo: bool,
    ):
        try:
            return tc.get_acs(
                geography=geography,
                variables=variables,
                state=state,
                year=year,
                geometry=geo,
            )
        except Exception as e:
            print(f"{state}, {e}")

        return pd.DataFrame()

    pd.set_option("future.no_silent_downcasting", True)

    tc.set_census_api_key(API_KEY)


    with ThreadPoolExecutor(max_workers=max_workers) as executor:
        futures = []
        for state in states:
            future = executor.submit(
                get_state_data,
                geography=geography,
                state=state,
                year=year,
                variables=vars,
                geo=geometry,
            )
            futures.append(future)
    data = pd.concat(
        [result.result() for result in futures],
        ignore_index=True,
        sort=False,
    )
    if moe is False:
        cols_to_drop = [col for col in list(data.columns) if "_moe" in col]
        data.drop(columns=cols_to_drop, inplace=True)

    data["GEOID"] = pd.to_numeric(data["GEOID"], downcast="integer")

    return data
