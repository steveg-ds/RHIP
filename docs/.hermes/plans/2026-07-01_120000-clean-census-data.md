# Census Data Cleaning Implementation Plan

> **For Hermes:** Use subagent-driven-development skill to implement this plan task-by-task.

**Goal:** Add `clean_data` method to `CensusDataLoader` in `utils/acs_loader.py` to handle outliers, low unit counts, and binary housing quality metrics.

**Architecture:** Add `clean_data(df)` method to `CensusDataLoader` class. Implement filtering logic (unit threshold, Z-score outlier removal) and feature engineering (housing quality flag).

**Tech Stack:** Python, Pandas, Pydantic.

---

### Task 1: Create test file for data cleaning

**Objective:** Create unit tests for data cleaning logic.

**Files:**
- Create: `tests/test_acs_loader_cleaning.py`

**Step 1: Create test file**

```python
import pandas as pd
import pytest
from utils.acs_loader import CensusDataLoader

def test_clean_data_low_units():
    # Test filtering logic for low unit counts
    pass
```

**Step 2: Commit**

```bash
git add tests/test_acs_loader_cleaning.py
git commit -m "test: add cleaning tests"
```

### Task 2: Implement `clean_data` method

**Objective:** Add `clean_data` method to `CensusDataLoader`.

**Files:**
- Modify: `utils/acs_loader.py`

**Step 1: Write minimal code**

```python
def clean_data(self, df: pd.DataFrame, unit_threshold: int = 10) -> pd.DataFrame:
    # 1. Low unit count filter
    # 2. Outlier removal (Z-score > 3)
    # 3. Binary housing quality logic
    return df
```

**Step 2: Run verification**

Run: `pytest tests/test_acs_loader_cleaning.py`
Expected: PASS

**Step 3: Commit**

```bash
git add utils/acs_loader.py
git commit -m "feat: implement clean_data method"
```

### Task 3: Integrate `clean_data` into `fetch`

**Objective:** Update `fetch` method to call `clean_data` automatically if enabled.

**Files:**
- Modify: `utils/acs_loader.py`

**Step 1: Modify `fetch`**

```python
def fetch(self, ..., clean: bool = False) -> pd.DataFrame:
    # ... existing fetch logic
    if clean:
        df = self.clean_data(df)
    return df
```

**Step 2: Verify**

Run tests.
Expected: PASS

**Step 3: Commit**

```bash
git add utils/acs_loader.py
git commit -m "feat: integrate clean_data in fetch"
```
