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
        deduped_vars = sorted(list(set(variables)))
        return tc.get_acs(
            geography="tract",
            variables=deduped_vars,
            state=self.states,
            year=self.year
        )
