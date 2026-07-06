# Basedpyright/Typing Fix Plan for acs_loader.py

**Goal:** Resolve typing warnings in `utils/acs_loader.py`.

**Approach:** 
1. Fix `collect_tract_data` implementation which references undefined `tracts` and `acs_loader`.
2. Clean up typing and imports.

**Tasks:**

### Task 1: Fix `collect_tract_data` typing/imports
- **Objective:** Fix `collect_tract_data` implementation to be valid and typed.
- **Files:** `utils/acs_loader.py`
- **Steps:**
    1. Import `geopandas` inside method or at top (consistent style).
    2. Reference `self.states` instead of `acs_loader.states`.
    3. Ensure `tc.get_geography` usage matches `pytidycensus` API.

### Task 2: Fix remaining type hints
- **Objective:** Ensure all method signatures match `pandas` and `pydantic` expectations.
- **Files:** `utils/acs_loader.py`
- **Steps:**
    1. Verify `fetch` and `fetch_multiple` return types.
    2. Check `model_post_init` context argument type.

### Task 3: Validation
- **Objective:** Run `basedpyright` check.
- **Steps:**
    1. Run `basedpyright utils/acs_loader.py` in terminal.
