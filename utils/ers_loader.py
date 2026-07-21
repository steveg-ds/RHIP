import io

import pandas as pd
import requests
from pydantic import BaseModel, ConfigDict


class ERSDataLoader(BaseModel):
    """
    Loads and provides access to Economic Research Service (ERS) data.
    """

    model_config = ConfigDict(arbitrary_types_allowed=True)

    # boolean attribute for filtering out non-continental geographies
    continental: bool = True

    NON_CONTINENTAL: set[str] = {
        "Alaska",
        "Hawaii",
        "American Samoa",
        "Guam",
        "Commonwealth of the Northern Mariana Islands",
        "Puerto Rico",
        "United States Virgin Islands",
    }

    def _load_csv(self, url: str, encoding: str = "latin-1") -> pd.DataFrame:
        """Helper to load and return a DataFrame from a URL."""
        response = requests.get(url)
        response.raise_for_status()
        return pd.read_csv(io.BytesIO(response.content), encoding=encoding)

    def collect_ruca_data(self) -> pd.DataFrame:
        """
        Loads RUCA data directly from the USDA website, performs cleanup,
        and filters for continental US states.
        """
        ruca_url = (
            "https://www.ers.usda.gov/media/5443/2020-rural-urban-"
            "commuting-area-codes-census-tracts.csv?v=48133"
        )
        df = self._load_csv(ruca_url)

        # Rename columns for consistency
        df = df.rename(
            columns={"StateName20": "state_name", "RUCA2010": "ruca_code"}
        )

        if self.continental:
            # Filter out non-continental US states
            df = df[
                ~df["state_name"].isin(list(self.NON_CONTINENTAL))
            ].copy()

        return df

    def collect_rucc_data(self) -> pd.DataFrame:
        """Loads and cleans RUCC data."""
        # Using a fixed URL or making it configurable
        url = "https://www.ers.usda.gov/media/5768/2023-rural-urban-continuum-codes.csv?v=65353"
        data = self._load_csv(url)

        # New data structure has 'Attribute' and 'Value' rows.
        # Need to pivot to get 'RUCC' and 'Population' as columns
        # Pivot table: FIPS, State, County_Name as index
        df = data.pivot(
            index=["FIPS", "State", "County_Name"], columns="Attribute", values="Value"
        ).reset_index()

        df.rename(columns={"RUCC_2023": "RUCC", "FIPS": "GEOID"}, inplace=True)
        df["GEOID"] = df["GEOID"].astype(int)

        # Clean county names
        df["County_Name"] = (
            df["County_Name"]
            .str.replace("County", "", regex=False)
            .str.replace("Parish", "", regex=False)
            .str.strip()
            .str.lower()
        )

        if self.continental:
            df = df[~df["State"].isin(list(self.NON_CONTINENTAL))].copy()

        # CT switched from 8 counties to 9 planning regions in 2022.
        # The RUCC dataset uses new planning region GEOIDs (9110–9190) while
        # Census/ACS data for 2013/2018 still uses old county GEOIDs (9001–9015).
        # Append crosswalk rows so old GEOIDs resolve to a RUCC value.
        _CT_CROSSWALK = {
            9001: ("CT", "fairfield",     "2"),  # Fairfield → Greater Bridgeport PR
            9003: ("CT", "hartford",      "1"),  # Hartford  → Capitol PR
            9005: ("CT", "litchfield",    "4"),  # Litchfield → Northwest Hills PR
            9007: ("CT", "middlesex",     "1"),  # Middlesex → Lower CT River Valley PR
            9009: ("CT", "new haven",     "2"),  # New Haven → South Central CT PR
            9011: ("CT", "new london",    "2"),  # New London → Southeastern CT PR
            9013: ("CT", "tolland",       "4"),  # Tolland   → Northeastern CT PR
            9015: ("CT", "windham",       "4"),  # Windham   → Northeastern CT PR
        }
        ct_rows = pd.DataFrame(
            [
                {
                    "GEOID": geoid,
                    "State": state,
                    "County_Name": name,
                    "RUCC": rucc,
                    "Description": None,
                    "Population_2020": None,
                }
                for geoid, (state, name, rucc) in _CT_CROSSWALK.items()
            ]
        )
        df = pd.concat([df, ct_rows], ignore_index=True)

        # Drop dissolved/non-existent FIPS (e.g. Bedford City VA 51515,
        # absorbed into Bedford County 51019 in 2013)
        _DISSOLVED_FIPS = {51515}
        df = df[~df["GEOID"].isin(_DISSOLVED_FIPS)].copy()

        return df

