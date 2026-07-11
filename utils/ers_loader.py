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

        # TODO: this function needs to apply the same continental bool logic as collect_ruca_data

        return df

