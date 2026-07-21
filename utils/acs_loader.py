import concurrent.futures
import contextlib
import io
from typing import Any, ClassVar, Literal

import geopandas as gpd
import numpy as np
import pandas as pd

import warnings
with warnings.catch_warnings():
    warnings.filterwarnings("ignore", message="Mapping functions unavailable", category=UserWarning)
    import pytidycensus as tc
from pydantic import BaseModel, ConfigDict, Field, field_validator, model_validator
from pygris import counties, tracts


class CensusDataLoader(BaseModel):
    """
    Loads and provides access to US Census Bureau data,
    integrating with RUCA codes for rural/urban classification.
    """

    api_key: str | None = None
    """US Census Bureau API key"""

    states: list[str] | None = Field(default=None, validate_default=True)
    """List of states to fetch data for"""

    model_config: ConfigDict = ConfigDict(arbitrary_types_allowed=True)

    @field_validator("states", mode="before")
    @classmethod
    def validate_states(cls, v: str | None) -> list[str]:
        """Auto-fetch states if missing."""
        if v is None or (isinstance(v, list) and not v):
            # Fetch states from Census API
            try:
                states_df = tc.get_acs(
                    geography="state",
                    variables={"Population": "B01001_001E"},
                    year=2022,
                )
                if states_df is not None and not states_df.empty:
                    # In tidycensus, state FIPS are in the "state" column
                    return states_df["state"].tolist()
            except Exception as e:
                print(f"Failed to auto-fetch states: {e}")
        return v

    @field_validator("api_key", mode="after")
    @classmethod
    def set_api_key(cls, v: str | None) -> str | None:
        """Sets the Census API key."""
        if v:
            try:
                tc.set_census_api_key(v)
            except ValueError:
                pass
        return v

    def fetch(
        self,
        variables: list[str] | dict[str, str],
        states: list[str] | None = None,
        std_out: bool = False,
        moe: bool = False,
        geography: Literal["tract", "county", "state"] = "county",
        year: int = 2022,
    ) -> pd.DataFrame:
        """Fetches census variables using config states and year."""

        if isinstance(variables, dict):
            fetch_vars = variables
        else:
            fetch_vars = sorted(list(set(variables)))

        if geography == "county" and states is None:
            state_param = None
        else:
            fetch_states = states if states is not None else self.states
            if not fetch_states:
                fetch_states = self.validate_states(None)
            state_param = list(fetch_states) if fetch_states else None

        if not std_out:
            with io.StringIO() as buf, contextlib.redirect_stdout(buf):
                df = tc.get_acs(
                    geography=geography,
                    variables=fetch_vars,
                    state=state_param,
                    year=year,
                )
        else:
            df = tc.get_acs(
                geography=geography, variables=fetch_vars, state=state_param, year=year
            )

        if not moe and not df.empty:
            moe_cols = [col for col in df.columns if "_moe" in col]
            df = df.drop(columns=moe_cols)

        if "GEOID" in df.columns:
            df["GEOID"] = df["GEOID"].astype(int)

        return df

    def _validate_years(self, years: list[int]) -> None:
        """Validates that years are provided in 5-year intervals."""
        if not years:
            raise ValueError("Years list cannot be empty.")

        sorted_years = sorted(years)
        for i in range(len(sorted_years) - 1):
            if sorted_years[i + 1] - sorted_years[i] != 5:
                raise ValueError(
                    f"Years must have a 5-year gap. Found {sorted_years[i]} and {sorted_years[i + 1]}"
                )

    def fetch_multiple(
        self,
        variables: list[str] | dict[str, str],
        max_workers: int = 10,
        std_out: bool = False,
        moe: bool = False,
        **kwargs: Any,
    ) -> pd.DataFrame:
        if isinstance(variables, dict):
            fetch_vars = variables
        else:
            fetch_vars = sorted(list(set(variables)))

        geography = kwargs.get("geography", "county")
        if geography == "county":
            return self.fetch(
                variables=fetch_vars,
                states=None,
                std_out=std_out,
                moe=moe,
                **kwargs,
            )

        all_results = []

        fetch_states = self.states
        if not fetch_states:
            fetch_states = self.validate_states(None)

        def run_fetch():
            with concurrent.futures.ThreadPoolExecutor(
                max_workers=max_workers
            ) as executor:
                future_to_state = {
                    executor.submit(
                        self.fetch,
                        fetch_vars,
                        states=[state],
                        std_out=True,
                        moe=moe,
                        **kwargs,
                    )
                    for state in fetch_states
                }
                for future in concurrent.futures.as_completed(future_to_state):
                    try:
                        result = future.result()
                        all_results.append(result)
                    except Exception as exc:
                        if std_out:
                            print(f"State query generated an exception: {exc}")

        if not std_out:
            with io.StringIO() as buf, contextlib.redirect_stdout(buf):
                run_fetch()
        else:
            run_fetch()

        if not all_results:
            return pd.DataFrame()  # Return empty DataFrame if no results

        return pd.concat(all_results, axis=0)

    def fetch_multiple_years(
        self,
        years: list[int],
        variables: list[str] | dict[str, str],
        max_workers: int = 10,
        std_out: bool = False,
        moe: bool = False,
        **kwargs: Any,
    ) -> pd.DataFrame:
        self._validate_years(years)

        all_dfs = []
        for year in years:
            df = self.fetch_multiple(
                variables=variables,
                max_workers=max_workers,
                std_out=std_out,
                moe=moe,
                year=year,
                **kwargs,
            )
            df["year"] = year  # Add a year column for identification
            all_dfs.append(df)

        if not all_dfs:
            return pd.DataFrame()

        return pd.concat(all_dfs, axis=0)

    def collect_geometry_data(
        self, geometry_type: Literal["tracts", "counties"] = "tracts", year: int = 2022
    ) -> gpd.GeoDataFrame:
        """Collects geometry using multithreading."""
        all_geometries = []

        if geometry_type == "tracts":
            func = tracts
        elif geometry_type == "counties":
            func = counties
        else:
            raise ValueError(f"Unknown geometry_type: {geometry_type}")

        def fetch_geometry_for_state(state):
            # pygris.counties/tracts with cb=True uses US-wide file if state=None
            # but here it is called per-state.
            return func(state=state, cb=True, cache=True, year=year)

        with concurrent.futures.ThreadPoolExecutor(max_workers=10) as executor:
            future_to_state = {
                executor.submit(fetch_geometry_for_state, state)
                for state in self.states or self.validate_states(None)
            }
            for future in concurrent.futures.as_completed(future_to_state):
                try:
                    result = future.result()
                    all_geometries.append(result)
                except Exception as exc:
                    print(f"Geometry query generated an exception: {exc}")

        if not all_geometries:
            return gpd.GeoDataFrame()
        result = gpd.GeoDataFrame(
            pd.concat(all_geometries, ignore_index=True, sort=False)
        )
        if not result.empty and result.crs is not None:
            result = result.to_crs("EPSG:4326")

        result["GEOID"] = result["GEOID"].astype(int)

        result.rename(columns={"NAMELSAD": "COUNTY", "STUSPS": "STATE"}, inplace=True)
        
        return result[["GEOID", "STATE", "COUNTY", 'geometry']]


if __name__ == "__main__":
    from os import getenv

    from dotenv import load_dotenv

    load_dotenv()

    poverty_vars = [
        "B17001_001E",  # Total
        "B17001_002E",  # Income in the past 12 months
    ]

    x = CensusDataLoader(api_key=getenv("CENSUS_API_KEY"))
    # This should now auto-fetch states
    print("Fetch result head:")
    print(x.fetch(poverty_vars).head())

    print("\nTesting fetch_multiple:")
    try:
        result_multiple = x.fetch_multiple(poverty_vars, max_workers=2)
        print("Multiple result head:")
        print(result_multiple.head())
    except Exception as e:
        print(f"An error occurred during fetch_multiple: {e}")


