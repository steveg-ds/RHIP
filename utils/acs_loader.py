import pandas as pd
from pydantic import BaseModel, Field, ConfigDict, model_validator
from typing import List, Optional, ClassVar, Any, Dict, Union
import pytidycensus as tc
import requests
import io
import concurrent.futures
import math
import os
import contextlib


class CensusDataLoader(BaseModel):
    """
    Loads and provides access to US Census Bureau data,
    integrating with RUCA codes for rural/urban classification.
    """
    year: int = Field(default=2024, ge=2010, le=2024)
    """Year to collect Census data for"""

    states: Optional[List[str]] = None
    """List of state FIPS codes or abbreviations to restrict
    Census data queries to. Defaults to all continental states if
    `continental` is True"""

    api_key: Optional[str] = None
    """US Census Bureau API key"""

    model_config = ConfigDict(arbitrary_types_allowed=True)

    RUCA: Optional[pd.DataFrame] = None
    """RUCA classification data table"""

    continental: bool = True
    """set to False to get all states and territories"""

    NON_CONTINENTAL: ClassVar[set] = {
        "Alaska", "Hawaii", "American Samoa", "Guam",
        "Commonwealth of the Northern Mariana Islands",
        "Puerto Rico", "United States Virgin Islands",
    }

    def model_post_init(self, __context: Any) -> None:
        """
        Initializes the model, sets the Census API key, and loads RUCA data.
        """
        if self.api_key:
            try:
                tc.set_census_api_key(self.api_key)
            except ValueError:
                pass
        if self.RUCA is None:
            self.RUCA = self._load_ruca()

    @model_validator(mode='after')
    def set_default_states(self) -> 'CensusDataLoader':
        """
        Sets default states from the RUCA dataset if no states are specified.
        """
        if (self.states is None and self.RUCA is not None
                and not self.RUCA.empty):
            self.states = self.RUCA["state_name"].unique().tolist()
        return self

    def _load_ruca(self) -> pd.DataFrame:
        """
        Loads RUCA data directly from the USDA website, performs cleanup,
        and filters for continental US states.
        """
        ruca_url = ("https://www.ers.usda.gov/media/5443/2020-rural-urban-"
                    "commuting-area-codes-census-tracts.csv?v=48133")
        response = requests.get(ruca_url)
        response.raise_for_status()  # Raise HTTPError for bad responses

        ruca_df = pd.read_csv(io.StringIO(response.text), encoding='utf-8')

        # Rename columns for consistency
        ruca_df = ruca_df.rename(columns={
            "StateName20": "state_name",
            "RUCA2010": "ruca_code"
        })

        if self.continental:
            # Filter out non-continental US states
            ruca_df = ruca_df[~ruca_df["state_name"].isin(
                list(self.NON_CONTINENTAL))].copy()

        return ruca_df

    def fetch(self, variables: Union[List[str], Dict[str, str]],
              states: Optional[List[str]] = None,
              std_out: bool = False) -> pd.DataFrame:
        """Fetches census variables using config states and year."""
        if isinstance(variables, dict):
            fetch_vars = variables
        else:
            fetch_vars = sorted(list(set(variables)))

        fetch_states = states if states is not None else self.states

        if not std_out:
            with open(os.devnull, "w") as f:
                with contextlib.redirect_stdout(f):
                    return tc.get_acs(
                        geography="tract",
                        variables=fetch_vars,
                        state=fetch_states,
                        year=self.year
                    )
        else:
            return tc.get_acs(
                geography="tract",
                variables=fetch_vars,
                state=fetch_states,
                year=self.year
            )

    def fetch_multiple(self, variables: Union[List[str], Dict[str, str]],
                       max_workers: int = 10,
                       std_out: bool = False) -> pd.DataFrame:
        """
        Fetches census variables using multithreading to process states in parallel.

        Args:
            variables: A list or dictionary of variable IDs to fetch.
            max_workers: Max number of threads for parallel processing.
            std_out: Whether to print stdout messages.

        Returns:
            A pandas DataFrame with concatenated results from all queries.
        """
        if isinstance(variables, dict):
            fetch_vars = variables
        else:
            fetch_vars = sorted(list(set(variables)))
        
        # NOTE: self.states will always be set in the model validator so no need to handle instances where states not provided.
        states_list = self.states if self.states is not None else []

        all_results = []
        
        def run_fetch():
            with concurrent.futures.ThreadPoolExecutor(max_workers=max_workers) as executor:
                future_to_state = {
                    executor.submit(self.fetch, fetch_vars, [state], True)
                    for state in states_list
                }
                for future in concurrent.futures.as_completed(future_to_state):
                    try:
                        result = future.result()
                        all_results.append(result)
                    except Exception as exc:
                        if std_out:
                            print(f'State query generated an exception: {exc}')

        if not std_out:
            with open(os.devnull, "w") as f:
                with contextlib.redirect_stdout(f):
                    run_fetch()
        else:
            run_fetch()
        
        if not all_results:
            return pd.DataFrame()  # Return empty DataFrame if no results

        return pd.concat(all_results, axis=0)


if __name__ == "__main__":
    from dotenv import load_dotenv
    from os import getenv

    load_dotenv()

    poverty_vars = [
        "B17001_001E",  # Total
        "B17001_002E",  # Income in the past 12 months
        # below poverty level
    ]

    x = CensusDataLoader(api_key=getenv("CENSUS_API_KEY"))
    print(x.fetch(poverty_vars).head())

    print("\nTesting fetch_multiple:")

    try:
        result_multiple = x.fetch_multiple(poverty_vars, max_workers=2)
        print(result_multiple.head())
    except Exception as e:
        print(f"An error occurred during fetch_multiple: {e}")
