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

