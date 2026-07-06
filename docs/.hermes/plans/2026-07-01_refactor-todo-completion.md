# Refactor & TODO Completion Plan

**Goal:** Clean up `CensusDataLoader`, resolve `acs_loader.py` TODOs, clarify documentation.

---

### Task 1: Refactor API Key Management
**Objective:** Replace `model_post_init` with `field_validator`.
**Files:** `utils/acs_loader.py`

- Add `@field_validator('api_key', mode='after')` to set `tc.set_census_api_key`.
- Remove `model_post_init`.
- Commit: `feat: replace model_post_init with field_validator`

### Task 2: Implement State Auto-Fetch
**Objective:** Populate `states` before model init.
**Files:** `utils/acs_loader.py`

- Add `@field_validator('states', mode='before')` to handle `None` values.
- If `None`, fetch via `tc.get_acs` (total population) and extract state list.
- Make `states` non-optional in model schema to stop type warnings.
- Commit: `feat: auto-fetch missing states`

### Task 3: Improve Reliability
**Objective:** Add logging/timeouts where critical.
**Files:** `utils/acs_loader.py`

- Update `collect_geometry_data` thread with basic `print`/`logging` on failure.
- Add `timeout=300` to all `tc.get_acs` calls to prevent hanging.
- Commit: `fix: add timeouts and logging`

### Task 4: Documentation
**Objective:** Clear docstrings for unclear logic.
**Files:** `./utils/*.py`

- Add docstrings to methods lacking them.
- Document complex logic steps. Skip trivial/filler comments.
- Commit: `docs: improve utility docs`


