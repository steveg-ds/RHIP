import pandas as pd
from pydantic import BaseModel, Field
from typing import List, Optional
import pytidycensus as tc

class CensusConfig(BaseModel):
    year: int = Field(default=2024, ge=2010, le=2026)
    states: Optional[List[str]] = None
    api_key: Optional[str] = None

class BaseCensusDataLoader:
    def __init__(self, config: CensusConfig):
        self.config = config
        self._cache = {}  # dict mapping variables tuple to DataFrame
        if self.config.api_key:
            tc.set_census_api_key(self.config.api_key)

    def fetch(self, variables: List[str]) -> pd.DataFrame:
        var_tuple = tuple(sorted(list(set(variables))))
        if var_tuple not in self._cache:
            self._cache[var_tuple] = tc.get_acs(
                geography="tract",
                variables=variables,
                state=self.config.states,
                year=self.config.year
            )
        return self._cache[var_tuple].copy()
