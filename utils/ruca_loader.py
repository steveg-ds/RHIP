import pandas as pd
from pydantic.dataclasses import dataclass
from typing import ClassVar, Set


@dataclass
class RucaDataLoader:
    """Downloads and cleans USDA Rural-Urban Commuting Area (RUCA) codes."""

    URL: ClassVar[str] = (
        "https://www.ers.usda.gov/media/5443/"
        "2020-rural-urban-commuting-area-codes-census-tracts.csv?v=48133"
    )
    NON_CONTINENTAL: ClassVar[Set[str]] = {
        "Alaska", "Hawaii", "American Samoa", "Guam",
        "Commonwealth of the Northern Mariana Islands", "Puerto Rico",
        "United States Virgin Islands",
    }

    continental: bool = True

    def load(self) -> pd.DataFrame:
        """Returns a cleaned RUCA DataFrame, optionally filtered to continental US."""
        cols_to_load = [
            "TractFIPS20", "TractName20", "CountyFIPS20", "CountyName20",
            "StateFIPS20", "StateName20", "PrimaryRUCA",
        ]
        dtypes = {
            "TractFIPS20": str,
            "CountyFIPS20": str,
            "StateFIPS20": str,
            "StateName20": "category",
            "CountyName20": "category",
            "PrimaryRUCA": "int8",
        }
        ruca: pd.DataFrame = pd.read_csv(
            self.URL,
            encoding="latin1",
            usecols=cols_to_load,
            dtype=dtypes,
        )
        ruca.columns = ruca.columns.str.replace("20$", "", regex=True)
        ruca.rename(columns={"PrimaryRUCA": "RUCA"}, inplace=True)

        if self.continental:
            return ruca[~ruca["StateName"].isin(self.NON_CONTINENTAL)].reset_index(drop=True)
        return ruca
