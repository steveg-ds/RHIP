# Design Spec: Abstract & Reusable Census Data Loader

**Date:** 2026-06-16  
**Author:** Antigravity  
**Status:** Draft  

---

## 1. Goal

Create a highly reusable, abstract base class `BaseCensusDataLoader` using Pydantic for configuration. Project-specific classes will inherit from this base class to define their specific variable sets and metric calculations, reducing code duplication in the `urban-science` and `rural-detention` notebooks.

---

## 2. Architecture & Design Details

### 2.1 Shared Module: `utils/data_loader.py`

#### `CensusConfig` (Pydantic Model)
Validates runtime configuration for Census API requests.
- `year`: `int` (default `2024`)
- `states`: `List[str]` (required)
- `api_key`: `Optional[str]` (default `None`)

#### `BaseCensusDataLoader` (Base Class)
Implements generic caching, API key management, and data retrieval:
- `__init__(config: CensusConfig)`: Configures the Census API key.
- `fetch(variables: List[str]) -> pd.DataFrame`: Batches requests for variables, fetches them in one call, and caches results.

#### `ThesisCensusDataLoader` (Subclass of `BaseCensusDataLoader`)
Inherits from `BaseCensusDataLoader` and implements thesis-specific variables and calculations:
- `fetch_all_data()`: Calls parent `fetch()` with the combined 39 thesis variables.
- Helper methods for calculations:
  - `get_poverty()`
  - `get_cost_burden()`
  - `get_housing_quality()`
  - `get_education()`
  - `get_demographics()`
  - `get_gini()`
- `get_merged_dataset(ruca_df)`: Combines all calculated metrics and merges them with the RUCA DataFrame.

---

## 3. Files to Create / Modify

### Create: `utils/data_loader.py`
Contains:
- `CensusConfig`
- `BaseCensusDataLoader`
- `ThesisCensusDataLoader`

### Modify: `utils/__init__.py`
Expose the classes:
```python
from .data_loader import CensusConfig, BaseCensusDataLoader, ThesisCensusDataLoader
```

### Modify: `urban-science/Data Exploration.ipynb`
Initialize `ThesisCensusDataLoader` and call `get_merged_dataset(ruca)`.

### Modify: `rural-detention/Data Exploration.ipynb`
Initialize `ThesisCensusDataLoader` and call `get_merged_dataset(ruca)`.

---

## 4. Verification Plan

1. **Numeric Equivalence Verification:**
   - Execute a validation script comparing the original notebook-merged DataFrame against the new subclass-merged DataFrame.
2. **Notebook Rendering:**
   - Run both notebooks using Quarto to ensure they execute end-to-end without errors.
