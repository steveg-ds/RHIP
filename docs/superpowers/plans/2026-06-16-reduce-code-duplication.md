# Reduce Code Duplication Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Create a generic `BaseCensusDataLoader` with `CensusConfig` validation and update both `urban-science` and `rural-detention` notebooks to use it, reducing duplicate fetching logic.

**Architecture:** Define Pydantic config model and BaseCensusDataLoader class in `utils/data_loader.py` that wraps `pytidycensus.get_acs` and handles state/caching.

**Tech Stack:** Python, Pandas, Pydantic, pytidycensus, pytest

---

### Task 1: Create generic data loader in utils/data_loader.py

**Files:**
- Create: `utils/data_loader.py`
- Create: `utils/test_data_loader.py`

- [ ] **Step 1: Write the tests for configuration and data loader**

Create `utils/test_data_loader.py` with Pydantic validation tests and fetching test:

```python
import pytest
from pydantic import ValidationError
from utils.data_loader import CensusConfig, BaseCensusDataLoader
import pandas as pd

def test_config_validation():
    # Test valid config
    config = CensusConfig(year=2024, states=["01", "02"], api_key="test_key")
    assert config.year == 2024
    assert config.states == ["01", "02"]
    assert config.api_key == "test_key"

    # Test invalid year
    with pytest.raises(ValidationError):
        CensusConfig(year=1999, states=["01"])

def test_loader_cache(monkeypatch):
    # Mock pytidycensus.get_acs
    called_count = 0
    def mock_get_acs(geography, variables, state, year):
        nonlocal called_count
        called_count += 1
        return pd.DataFrame({"GEOID": ["01001020100"], "B17001_001E": [100]})

    import pytidycensus as tc
    monkeypatch.setattr(tc, "get_acs", mock_get_acs)

    config = CensusConfig(year=2024, states=["01"], api_key="dummy")
    loader = BaseCensusDataLoader(config)

    # First fetch
    df1 = loader.fetch(["B17001_001E"])
    # Second fetch
    df2 = loader.fetch(["B17001_001E"])

    assert called_count == 1
    assert df1.equals(df2)
```

- [ ] **Step 2: Run test to verify it fails**

Run: `pytest utils/test_data_loader.py -v`  
Expected: FAIL with `ModuleNotFoundError` for `utils.data_loader`

- [ ] **Step 3: Write minimal implementation**

Create `utils/data_loader.py`:

```python
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
        """Fetches census variables using config states and year."""
        if self._raw_data is None:
            import pytidycensus as tc
            self._raw_data = tc.get_acs(
                geography="tract",
                variables=variables,
                state=self.config.states,
                year=self.config.year
            )
        return self._raw_data
```

- [ ] **Step 4: Run test to verify it passes**

Run: `pytest utils/test_data_loader.py -v`  
Expected: PASS

- [ ] **Step 5: Commit changes**

Run:
```bash
git add utils/data_loader.py utils/test_data_loader.py
git commit -m "feat: add BaseCensusDataLoader and config validation"
```

---

### Task 2: Expose classes in package level __init__.py

**Files:**
- Modify: `utils/__init__.py:1-2`

- [ ] **Step 1: Write test for package imports**

Create a test function in `utils/test_data_loader.py`:

```python
def test_package_exports():
    from utils import CensusConfig, BaseCensusDataLoader
    assert CensusConfig is not None
    assert BaseCensusDataLoader is not None
```

- [ ] **Step 2: Run test to verify it fails**

Run: `pytest utils/test_data_loader.py::test_package_exports -v`  
Expected: FAIL with `ImportError`

- [ ] **Step 3: Modify utils/__init__.py to export classes**

Update `utils/__init__.py`:

```python
from .ruca import collect_ruca_data
from .data_loader import CensusConfig, BaseCensusDataLoader
```

- [ ] **Step 4: Run test to verify it passes**

Run: `pytest utils/test_data_loader.py::test_package_exports -v`  
Expected: PASS

- [ ] **Step 5: Commit changes**

Run:
```bash
git add utils/__init__.py utils/test_data_loader.py
git commit -m "chore: expose CensusConfig and BaseCensusDataLoader in utils package"
```

---

### Task 3: Refactor urban-science/Data Exploration.ipynb

**Files:**
- Modify: `urban-science/Data Exploration.ipynb`

- [ ] **Step 1: Update cell 1 to import from new loader**

Replace imports block in Cell 1 to import `CensusConfig` and `BaseCensusDataLoader`.  
Original Cell 1:
```python
import pandas as pd
import numpy as np
from dotenv import load_dotenv
import pytidycensus as tc
from os import getenv
from concurrent.futures import ThreadPoolExecutor
from IPython.display import HTML, display
import matplotlib.pyplot as plt
import statsmodels.api as sm
from sklearn.metrics import confusion_matrix, accuracy_score, classification_report
from statsmodels.stats.outliers_influence import variance_inflation_factor
from pygris import tracts

import seaborn as sns
import matplotlib.pyplot as plt

from sklearn.model_selection import train_test_split
from sklearn.tree import DecisionTreeClassifier, plot_tree
import matplotlib.pyplot as plt
import sys
sys.path.append("..")
from utils import collect_ruca_data

load_dotenv()

API_KEY: str = getenv("CENSUS_KEY")

tc.set_census_api_key(API_KEY)
# ... Register markdown formatter ...
```

Modified Cell 1:
```python
import pandas as pd
import numpy as np
from dotenv import load_dotenv
import pytidycensus as tc
from os import getenv
from concurrent.futures import ThreadPoolExecutor
from IPython.display import HTML, display
import matplotlib.pyplot as plt
import statsmodels.api as sm
from sklearn.metrics import confusion_matrix, accuracy_score, classification_report
from statsmodels.stats.outliers_influence import variance_inflation_factor
from pygris import tracts

import seaborn as sns
import matplotlib.pyplot as plt

from sklearn.model_selection import train_test_split
from sklearn.tree import DecisionTreeClassifier, plot_tree
import matplotlib.pyplot as plt
import sys
sys.path.append("..")
from utils import collect_ruca_data, CensusConfig, BaseCensusDataLoader

load_dotenv()

API_KEY: str = getenv("CENSUS_KEY")
```

- [ ] **Step 2: Initialize loader in notebook**

Insert a cell after STATES initialization (Cell 5):
```python
config = CensusConfig(year=YEAR, states=STATES, api_key=API_KEY)
loader = BaseCensusDataLoader(config)

# Combine all 39 variables
all_vars = [
    # Rent
    "B25070_001E", "B25070_007E", "B25070_008E", "B25070_009E", "B25070_010E",
    # Owner
    "B25091_001E", "B25091_008E", "B25091_009E", "B25091_010E", "B25091_011E",
    "B25091_019E", "B25091_020E", "B25091_021E", "B25091_022E",
    # Poverty
    "B17001_001E", "B17001_002E",
    # Housing Quality
    "B25047_001E", "B25047_003E", "B25051_001E", "B25051_003E",
    # Education
    "B15003_001E",
    # Demographics
    "B03002_001E", "B03002_003E",
    # Gini
    "B19083_001E"
] + [f"B15003_{num:03d}E" for num in range(2, 17)]

raw_acs = loader.fetch(all_vars)
```

- [ ] **Step 3: Update individual indicator cells to use raw_acs instead of fetching**

- Cost Burden Cell:
  Change `cost_burden = tc.get_acs(...)` to `cost_burden = raw_acs.copy()`.
- Poverty Cell:
  Change `poverty = tc.get_acs(...)` to `poverty = raw_acs.copy()`.
- Housing Quality Cell:
  Change `housing_quality = tc.get_acs(...)` to `housing_quality = raw_acs.copy()`.
- Education Cell:
  Change `education = tc.get_acs(...)` to `education = raw_acs.copy()`.
- Demographics Cell:
  Change `demographics = tc.get_acs(...)` to `demographics = raw_acs.copy()`.
- Misc Cell:
  Change `misc = tc.get_acs(...)` to `misc = raw_acs.copy()`.

- [ ] **Step 4: Verify rendering and outputs**

Run Quarto render:
`quarto render "urban-science/Data Exploration.ipynb" --to html`  
Expected: HTML generated successfully.

- [ ] **Step 5: Commit changes**

Run:
```bash
git add "urban-science/Data Exploration.ipynb"
git commit -m "refactor: use BaseCensusDataLoader in urban-science notebook"
```

---

### Task 4: Refactor rural-detention/Data Exploration.ipynb

**Files:**
- Modify: `rural-detention/Data Exploration.ipynb`

- [ ] **Step 1: Update Cell 1 imports**

Modify Cell 1 imports to import `CensusConfig` and `BaseCensusDataLoader` from `utils`.

- [ ] **Step 2: Initialize loader**

Define STATES, YEAR, initialize `CensusConfig` and `BaseCensusDataLoader`, and fetch `all_vars` just like Task 3 Step 2.

- [ ] **Step 3: Update individual indicator cells**

Replace each `tc.get_acs(...)` with `raw_acs.copy()` just like Task 3 Step 3.

- [ ] **Step 4: Verify rendering and outputs**

Run Quarto render:
`quarto render "rural-detention/Data Exploration.ipynb" --to html`  
Expected: HTML generated successfully.

- [ ] **Step 5: Commit changes**

Run:
```bash
git add "rural-detention/Data Exploration.ipynb"
git commit -m "refactor: use BaseCensusDataLoader in rural-detention notebook"
```
