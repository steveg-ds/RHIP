# Data Loader Refactor & Multi-Year Collection Plan

**Goal:** Modularize ERS data, implement validated multi-year collection, and resolve tract data TODOs.

---

### Task 1: Create `utils/ers_loader.py`
**Objective:** Extract RUCA/RUCC logic into a dedicated loader.

**Files:**
- Create: `utils/ers_loader.py`
- Modify: `utils/acs_loader.py` (Remove `collect_rucc_data`, `_load_ruca`)

**Step 1: Move methods**
Move `collect_rucc_data` and `_load_ruca` to `ERSDataLoader` class in `utils/ers_loader.py`.

**Step 2: Commit**
```bash
git add utils/ers_loader.py utils/acs_loader.py
git commit -m "refactor: extract ERSDataLoader"
```

### Task 2: Implement Multi-Year Collection with Validation
**Objective:** Add `fetch_multiple_years` with robust year validation.

**Files:**
- Modify: `utils/acs_loader.py`

**Step 1: Add validation logic**
Use a class-level validator or a helper:
```python
def _validate_years(self, years: list[int]) -> None:
    # Use diffs on sorted list to ensure >= 5 gap
    if any(b - a < 5 for a, b in zip(sorted(years), sorted(years)[1:])):
        raise ValueError("Years must be at least 5 years apart.")
```

**Step 2: Add `fetch_multiple_years`**
```python
def fetch_multiple_years(self, variables, years: list[int], ...) -> pd.DataFrame:
    self._validate_years(years)
    # Collect data for each year using fetch_multiple, then concat
```

**Step 3: Commit**
```bash
git add utils/acs_loader.py
git commit -m "feat: add multi-year fetch with validation"
```

### Task 3: Resolve `collect_tract_data` TODO
**Objective:** Parallelize geometry collection.

**Files:**
- Modify: `utils/acs_loader.py`

**Step 1: Implement threading**
[Verify `ThreadPoolExecutor` is used for tract boundary collection.]

**Step 2: Commit**
```bash
git add utils/acs_loader.py
git commit -m "fix: parallelize tract data collection"
```
