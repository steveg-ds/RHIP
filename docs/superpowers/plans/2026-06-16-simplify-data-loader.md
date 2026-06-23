# Simplify Data Loader Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Combine `CensusConfig` and `BaseCensusDataLoader` into a single Pydantic-based `CensusDataLoader` class without caching, and update imports/usage.

**Architecture:** Combine config and loader into `CensusDataLoader(BaseModel)` inside `utils/data_loader.py`. Remove cache. Expose in `utils/__init__.py`. Update notebooks and tests.

**Tech Stack:** Python, Pydantic, Pandas, pytest

---

### Task 1: Refactor utils/data_loader.py

**Files:**
- Modify: `utils/data_loader.py`
- Modify: `utils/test_data_loader.py`

- [ ] **Step 1: Update utils/test_data_loader.py**

Replace tests with config validation and simple fetch mock test:

```python
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
```

- [ ] **Step 2: Run tests to verify failure**

Run: `pytest utils/test_data_loader.py -v`  
Expected: FAIL due to missing `CensusDataLoader` import or mismatched properties.

- [ ] **Step 3: Modify utils/data_loader.py**

Rewrite the file:

```python
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
            tc.set_census_api_key(self.api_key)

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
```

- [ ] **Step 4: Run tests to verify success**

Run: `pytest utils/test_data_loader.py -v`  
Expected: PASS

- [ ] **Step 5: Commit changes**

Run:
```bash
git add utils/data_loader.py utils/test_data_loader.py
git commit -m "feat: simplify to single CensusDataLoader class"
```

---

### Task 2: Update utils/__init__.py and notebooks

**Files:**
- Modify: `utils/__init__.py`
- Modify: `urban-science/Data Exploration.ipynb`
- Modify: `rural-detention/Data Exploration.ipynb`

- [ ] **Step 1: Modify utils/__init__.py**

Update exports:

```python
from .ruca import collect_ruca_data
from .data_loader import CensusDataLoader

__all__ = ["collect_ruca_data", "CensusDataLoader"]
```

- [ ] **Step 2: Update notebooks**

In both `urban-science/Data Exploration.ipynb` and `rural-detention/Data Exploration.ipynb`:
1. Change imports to:
```python
from utils import collect_ruca_data, CensusDataLoader
```
2. Change initialization cell to:
```python
loader = CensusDataLoader(year=YEAR, states=STATES, api_key=API_KEY)
# ... all_vars ...
raw_acs = loader.fetch(all_vars)
```

- [ ] **Step 3: Verify rendering**

Run Quarto:
```bash
quarto render "urban-science/Data Exploration.ipynb" --to html
quarto render "rural-detention/Data Exploration.ipynb" --to html
```
Expected: Success

- [ ] **Step 4: Commit changes**

Run:
```bash
git add utils/__init__.py "urban-science/Data Exploration.ipynb" "rural-detention/Data Exploration.ipynb"
git commit -m "refactor: update notebooks and exports to use CensusDataLoader"
```
