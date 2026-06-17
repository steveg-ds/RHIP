import pandas as pd
from pydantic import BaseModel, Field
from typing import List, Optional
import pytidycensus as tc

class CensusDataLoader(BaseModel):
    year: int = Field(default=2024, ge=2010, le=2026)
    states: Optional[List[str]] = None
    api_key: Optional[str] = None

    def model_post_init(self, __context) -> None:
        if self.api_key:
            try:
                tc.set_census_api_key(self.api_key)
            except ValueError:
                pass

    def fetch(self, variables: List[str]) -> pd.DataFrame:
        """Fetches census variables using config states and year."""
        # Deduplicate variables to avoid duplicate API requests
        deduped_vars = sorted(list(set(variables)))
        return tc.get_acs(
            geography="tract",
            variables=deduped_vars,
            state=self.states,
            year=self.year
        )

    def collect_ruca_data(self, continental=True) -> pd.DataFrame:
        """Downloads and cleans the 2020 Rural-Urban Commuting Area (RUCA) codes."""
        RUCA_URL: str = "https://www.ers.usda.gov/media/5443/2020-rural-urban-commuting-area-codes-census-tracts.csv?v=48133"
        cols_to_load = [
            'TractFIPS20', 'TractName20', 'CountyFIPS20', 'CountyName20', 
            'StateFIPS20', 'StateName20', 'PrimaryRUCA'
        ]
        dtypes = {
            'TractFIPS20': str,
            'CountyFIPS20': str,
            'StateFIPS20': str,
            'StateName20': 'category',
            'CountyName20': 'category',
            'PrimaryRUCA': 'int8'
        }
        ruca: pd.DataFrame = pd.read_csv(
            RUCA_URL, 
            encoding='latin1', 
            usecols=cols_to_load, 
            dtype=dtypes
        )
        ruca.columns = ruca.columns.str.replace('20$', '', regex=True)
        ruca.rename(columns={"PrimaryRUCA": "RUCA"}, inplace=True)
        
        non_continental = {
            'Alaska', 'Hawaii', 'American Samoa', 'Guam', 
            'Commonwealth of the Northern Mariana Islands', 'Puerto Rico', 
            'United States Virgin Islands'
        }
        if continental is True:
            return ruca[~ruca['StateName'].isin(non_continental)].reset_index(drop=True)
        return ruca



