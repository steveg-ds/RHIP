import pytest
from pydantic import ValidationError
from utils.data_loader import CensusConfig, BaseCensusDataLoader
import pandas as pd

def test_config_validation():
    config = CensusConfig(year=2024, states=["01", "02"], api_key="test_key")
    assert config.year == 2024
    assert config.states == ["01", "02"]
    assert config.api_key == "test_key"

    with pytest.raises(ValidationError):
        CensusConfig(year=1999, states=["01"])

def test_loader_cache(monkeypatch):
    called_count = 0
    def mock_get_acs(geography, variables, state, year):
        nonlocal called_count
        called_count += 1
        return pd.DataFrame({"GEOID": ["01001020100"], "B17001_001E": [100]})

    import pytidycensus as tc
    monkeypatch.setattr(tc, "get_acs", mock_get_acs)

    config = CensusConfig(year=2024, states=["01"], api_key="a" * 40)
    loader = BaseCensusDataLoader(config)

    df1 = loader.fetch(["B17001_001E"])
    df2 = loader.fetch(["B17001_001E"])

    assert called_count == 1
    assert df1.equals(df2)

def test_loader_cache_separate_variables(monkeypatch):
    called_count = 0
    requested_vars = []
    def mock_get_acs(geography, variables, state, year):
        nonlocal called_count
        called_count += 1
        requested_vars.append(variables)
        return pd.DataFrame({"GEOID": ["01001020100"], variables[0]: [called_count]})

    import pytidycensus as tc
    monkeypatch.setattr(tc, "get_acs", mock_get_acs)

    config = CensusConfig(year=2024, states=["01"], api_key="a" * 40)
    loader = BaseCensusDataLoader(config)

    df1 = loader.fetch(["B17001_001E"])
    df2 = loader.fetch(["B17001_002E"])
    df3 = loader.fetch(["B17001_001E"])

    assert called_count == 2
    assert requested_vars == [["B17001_001E"], ["B17001_002E"]]
    assert df1.equals(df3)
    assert not df1.equals(df2)


def test_loader_cache_order_insensitivity(monkeypatch):
    called_count = 0
    def mock_get_acs(geography, variables, state, year):
        nonlocal called_count
        called_count += 1
        return pd.DataFrame({"GEOID": ["01001020100"], "val": [called_count]})

    import pytidycensus as tc
    monkeypatch.setattr(tc, "get_acs", mock_get_acs)

    config = CensusConfig(year=2024, states=["01"], api_key="a" * 40)
    loader = BaseCensusDataLoader(config)

    df1 = loader.fetch(["var1", "var2"])
    df2 = loader.fetch(["var2", "var1"])

    assert called_count == 1
    assert df1.equals(df2)


def test_loader_returns_copy(monkeypatch):
    def mock_get_acs(geography, variables, state, year):
        return pd.DataFrame({"GEOID": ["01001020100"], "val": [100]})

    import pytidycensus as tc
    monkeypatch.setattr(tc, "get_acs", mock_get_acs)

    config = CensusConfig(year=2024, states=["01"], api_key="a" * 40)
    loader = BaseCensusDataLoader(config)

    df1 = loader.fetch(["var1"])
    df2 = loader.fetch(["var1"])

    assert df1 is not df2
    df1.at[0, "val"] = 999
    assert df2.at[0, "val"] == 100

