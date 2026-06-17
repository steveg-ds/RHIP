import pandas as pd
from pydantic import BaseModel, Field
from typing import List, Optional

class CensusConfig(BaseModel):
    year: int = Field(default=2024, ge=2010, le=2026)
    states: List[str] = Field(default_factory=list)
    api_key: Optional[str] = None

class BaseCensusDataLoader:
    def __init__(self, config: CensusConfig):
        self.config = config
        self._raw_data: Optional[pd.DataFrame] = None
        if self.config.api_key:
            import pytidycensus as tc
            tc.set_census_api_key(self.config.api_key)

    def fetch(self, variables: List[str]) -> pd.DataFrame:
        if self._raw_data is None:
            import pytidycensus as tc
            self._raw_data = tc.get_acs(
                geography="tract",
                variables=variables,
                state=self.config.states,
                year=self.config.year
            )
        return self._raw_data
