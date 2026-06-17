# Design Spec: Generic Census Data Loader

**Date:** 2026-06-16  
**Author:** Antigravity  
**Status:** Draft  

---

## 1. Goal

Create a generic and reusable `BaseCensusDataLoader` with `CensusConfig` validation in `utils/data_loader.py`. This class will handle API configuration, generic data fetching, and caching, leaving variable definition and specific metric calculation to the notebooks or project-specific subclasses.

---

## 2. Architecture & Design Details

### 2.1 Shared Module: `utils/data_loader.py`

#### `CensusConfig` (Pydantic Model)
Validates configuration parameter values.
- `year`: `int` (default `2024`, must be >= 2010)
- `states`: `List[str]` (required)
- `api_key`: `Optional[str]` (default `None`)

#### `BaseCensusDataLoader` (Class)
- `__init__(config: CensusConfig)`: Initializes the loader and registers the Census API key.
- `fetch(variables: List[str]) -> pd.DataFrame`: Batches requests, queries the Census API using `pytidycensus.get_acs` for the configured states and year, and caches/returns the raw DataFrame.

### 2.2 Notebook Usage

In both `urban-science/Data Exploration.ipynb` and `rural-detention/Data Exploration.ipynb`:
1. Define the 39 ACS variables.
2. Initialize `CensusConfig` and `BaseCensusDataLoader`.
3. Call `loader.fetch(variables)` to retrieve all data in a single network request.
4. Calculate the metrics (`CostBurden`, `PovertyRate`, etc.) locally using the returned DataFrame.
5. Merge with RUCA codes.

This removes 6 separate network requests in each notebook and replaces them with 1 single cached request, while keeping `utils/data_loader.py` fully generic.

---

## 3. Files to Create / Modify

### Create: `utils/data_loader.py`
Defines `CensusConfig` and `BaseCensusDataLoader`.

### Modify: `utils/__init__.py`
Expose the generic loader:
```python
from .data_loader import CensusConfig, BaseCensusDataLoader
```

### Modify: `urban-science/Data Exploration.ipynb`
Update data loading cells to use the generic loader.

### Modify: `rural-detention/Data Exploration.ipynb`
Update data loading cells to use the generic loader.

---

## 4. Verification Plan

1. **Equivalence Check:** Verify that the resulting dataset and calculations match the original notebook results exactly.
2. **Notebook Rendering:** Ensure Quarto can render both notebooks with the new loader.
