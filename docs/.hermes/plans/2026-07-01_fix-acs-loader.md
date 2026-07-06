# Fixes Plan: acs_loader.py

**Goal:** Resolve code review risks.

**Tasks:**

### Task 1: `collect_tract_data` logging
- **Objective:** Add error logging to `fetch_tracts_for_state` thread.
- **Files:** `utils/acs_loader.py`
- **Cmd:** `python3 -c "import logging; print(logging.getLogger().handlers)"`

### Task 2: `fetch` timeout
- **Objective:** Set `tc.get_acs(..., timeout=300)`.
- **Files:** `utils/acs_loader.py`

### Task 3: API Key validation
- **Objective:** Raise `ValueError` if `api_key` missing.
- **Files:** `utils/acs_loader.py`
- **Steps:** 
    1. Add `field_validator` for `api_key`.
    2. Remove `model_post_init` side effects.
