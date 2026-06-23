# Merge RUCA Functionality Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Merge the RUCA code collection functionality from `utils/ruca.py` into the `CensusDataLoader` class, remove `utils/ruca.py`, and update the notebooks.

**Architecture:** Add `collect_ruca_data()` method to `CensusDataLoader` in `utils/data_loader.py`. Remove `utils/ruca.py`. Expose only `CensusDataLoader` in `utils/__init__.py`. Update tests and notebooks.

**Tech Stack:** Python, Pydantic, Pandas, pytest

---

### Task 1: Add collect_ruca_data to CensusDataLoader

**Files:**
- Modify: `utils/data_loader.py`
- Modify: `utils/test_data_loader.py`

- [ ] **Step 1: Update utils/test_data_loader.py**

Add a test case for `collect_ruca_data`:

```python
def test_collect_ruca_data(monkeypatch):
    # Mock pd.read_csv to return a minimal RUCA dataframe
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
```

- [ ] **Step 2: Run tests to verify failure**

Run: `pytest utils/test_data_loader.py -v`  
Expected: FAIL due to missing `collect_ruca_data` method.

- [ ] **Step 3: Modify utils/data_loader.py**

Add `collect_ruca_data` method to `CensusDataLoader`:

```python
    def collect_ruca_data(self) -> pd.DataFrame:
        """Downloads and cleans the 2020 Rural-Urban Commuting Area (RUCA) codes."""
        RUCA_URL: str = "https://www.ers.usda.gov/media/5443/2020-rural-urban-commuting-area-codes-census-tracts.csv?v=48133"
        cols_to_load = [
            'TractFIPS20', 'TractName20', 'CountyFIPS20', 'CountyName20', 
            'StateFIPS20', 'StateName20', 'PrimaryRUCA'
        ]
        dtypes = {
            'TractFIPS20': str,
            'CountyFIPS20': str,
            'StateFIPS20': str,
            'StateName20': 'category',
            'CountyName20': 'category',
            'PrimaryRUCA': 'int8'
        }
        ruca: pd.DataFrame = pd.read_csv(
            RUCA_URL, 
            encoding='latin1', 
            usecols=cols_to_load, 
            dtype=dtypes
        )
        ruca.columns = ruca.columns.str.replace('20$', '', regex=True)
        ruca.rename(columns={"PrimaryRUCA": "RUCA"}, inplace=True)
        
        non_continental = {
            'Alaska', 'Hawaii', 'American Samoa', 'Guam', 
            'Commonwealth of the Northern Mariana Islands', 'Puerto Rico', 
            'United States Virgin Islands'
        }
        ruca = ruca[~ruca['StateName'].isin(non_continental)].reset_index(drop=True)
        return ruca
```

- [ ] **Step 4: Run tests to verify success**

Run: `pytest utils/test_data_loader.py -v`  
Expected: PASS

- [ ] **Step 5: Commit changes**

Run:
```bash
git add utils/data_loader.py utils/test_data_loader.py
git commit -m "feat: add collect_ruca_data to CensusDataLoader"
```

---

### Task 2: Remove utils/ruca.py and update package exports

**Files:**
- Delete: `utils/ruca.py`
- Modify: `utils/__init__.py`
- Modify: `utils/test_data_loader.py`

- [ ] **Step 1: Update package exports test**

In `utils/test_data_loader.py`, remove `collect_ruca_data` from imports and asserts in `test_package_exports`.

- [ ] **Step 2: Modify utils/__init__.py**

Update to export only `CensusDataLoader`:

```python
from .data_loader import CensusDataLoader

__all__ = ["CensusDataLoader"]
```

- [ ] **Step 3: Delete utils/ruca.py**

Run: `git rm utils/ruca.py`

- [ ] **Step 4: Run tests to verify success**

Run: `pytest utils/test_data_loader.py -v`  
Expected: PASS

- [ ] **Step 5: Commit changes**

Run:
```bash
git add utils/__init__.py utils/test_data_loader.py
git commit -m "chore: remove ruca.py and update package exports"
```

---

### Task 3: Update notebooks to use combined loader for RUCA

**Files:**
- Modify: `UrbanScience/Data Exploration.ipynb`
- Modify: `RuralDetention/Data Exploration.ipynb`

- [ ] **Step 1: Update notebooks**

In both `UrbanScience/Data Exploration.ipynb` and `RuralDetention/Data Exploration.ipynb`:
1. Change imports in Cell 1:
Remove `collect_ruca_data` from `from utils import ...` imports (only import `CensusDataLoader`).
2. Move RUCA fetch cell:
In the RUCA fetch cell (Cell 4):
Remove the check `if "ruca" not in locals(): ruca = collect_ruca_data()`.
Instead, move the RUCA fetch logic to occur AFTER the `loader` is initialized.
Change the fetch call to:
```python
if "ruca" not in locals():
    ruca = loader.collect_ruca_data()
```
*Note: This means the loader initialization cell must be moved to run BEFORE the RUCA fetch cell.*

- [ ] **Step 2: Verify rendering**

Run Quarto:
```bash
quarto render "UrbanScience/Data Exploration.ipynb" --to html
quarto render "RuralDetention/Data Exploration.ipynb" --to html
```
Expected: Success

- [ ] **Step 3: Commit changes**

Run:
```bash
git add "UrbanScience/Data Exploration.ipynb" "RuralDetention/Data Exploration.ipynb"
git commit -m "refactor: update notebooks to fetch RUCA via CensusDataLoader"
```
