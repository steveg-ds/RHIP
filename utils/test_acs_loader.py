import pytest
from pydantic import ValidationError
from utils.acs_loader import CensusDataLoader
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

def test_fetch_stdout_redirection(monkeypatch, capsys):
    def mock_get_acs(geography, variables, state, year):
        print("Fetching ACS data...")
        return pd.DataFrame({"GEOID": ["01001020100"], "B17001_001E": [100]})

    import pytidycensus as tc
    monkeypatch.setattr(tc, "get_acs", mock_get_acs)

    loader = CensusDataLoader(year=2024, states=["01"], api_key="dummy")
    
    # 1. Test with std_out=False (default)
    df = loader.fetch(["B17001_001E"], std_out=False)
    captured = capsys.readouterr()
    assert captured.out == ""  # stdout should be redirected/suppressed
    
    # 2. Test with std_out=True
    df = loader.fetch(["B17001_001E"], std_out=True)
    captured = capsys.readouterr()
    assert "Fetching ACS data..." in captured.out

def test_fetch_multiple_stdout_redirection(monkeypatch, capsys):
    def mock_get_acs(geography, variables, state, year):
        print("Fetching ACS multiple...")
        return pd.DataFrame({"GEOID": ["01001020100"], "B17001_001E": [100]})

    import pytidycensus as tc
    monkeypatch.setattr(tc, "get_acs", mock_get_acs)

    loader = CensusDataLoader(year=2024, states=["01", "02"], api_key="dummy")
    
    # 1. Test with std_out=False (default)
    df = loader.fetch_multiple(["B17001_001E"], max_workers=2, std_out=False)
    captured = capsys.readouterr()
    assert captured.out == ""  # suppressed
    
    # 2. Test with std_out=True
    df = loader.fetch_multiple(["B17001_001E"], max_workers=2, std_out=True)
    captured = capsys.readouterr()
    assert "Fetching ACS multiple..." in captured.out

def test_fetch_with_dict(monkeypatch):
    called_vars = None
    def mock_get_acs(geography, variables, state, year):
        nonlocal called_vars
        called_vars = variables
        return pd.DataFrame({"GEOID": ["01001020100"], "poverty_rate": [100]})

    import pytidycensus as tc
    monkeypatch.setattr(tc, "get_acs", mock_get_acs)

    loader = CensusDataLoader(year=2024, states=["01"], api_key="dummy")
    variables_dict = {"poverty_rate": "B17001_002E"}
    df = loader.fetch(variables_dict)

    assert called_vars == variables_dict
    assert df.at[0, "poverty_rate"] == 100

def test_package_exports():
    from utils import CensusDataLoader
    assert CensusDataLoader is not None


