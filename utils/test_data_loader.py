import pytest
from pydantic import ValidationError
from utils.data_loader import CensusDataLoader
import pandas as pd

def test_config_validation():
    loader = CensusDataLoader(year=2024, states=["01", "02"], api_key="test_key")
    assert loader.year == 2024
    assert loader.states == ["01", "02"]
    assert loader.api_key == "test_key"

    with pytest.raises(ValidationError):
        CensusDataLoader(year=1999, states=["01"])

def test_loader_fetch(monkeypatch):
    called_count = 0
    def mock_get_acs(geography, variables, state, year):
        nonlocal called_count
        called_count += 1
        return pd.DataFrame({"GEOID": ["01001020100"], "B17001_001E": [100]})

    import pytidycensus as tc
    monkeypatch.setattr(tc, "get_acs", mock_get_acs)

    loader = CensusDataLoader(year=2024, states=["01"], api_key="dummy")
    df = loader.fetch(["B17001_001E"])

    assert called_count == 1
    assert df.at[0, "B17001_001E"] == 100

def test_package_exports():
    from utils import CensusDataLoader
    assert CensusDataLoader is not None


def test_collect_ruca_data(monkeypatch):
    def mock_read_csv(filepath_or_buffer, *args, **kwargs):
        return pd.DataFrame({
            'TractFIPS20': ['01001020100'],
            'TractName20': ['Census Tract 201'],
            'CountyFIPS20': ['01001'],
            'CountyName20': ['Autauga County'],
            'StateFIPS20': ['01'],
            'StateName20': ['Alabama'],
            'PrimaryRUCA': [1]
        })

    monkeypatch.setattr(pd, "read_csv", mock_read_csv)

    loader = CensusDataLoader(year=2024)
    ruca = loader.collect_ruca_data()

    assert len(ruca) == 1
    assert ruca.at[0, "TractFIPS"] == "01001020100"
    assert ruca.at[0, "RUCA"] == 1


