# Repository Cleanup & Organization Plan

**Goal:** Clean root directory, structure project, and enforce git hygiene.

---

### Task 1: Repository Purge
**Objective:** Remove temporary scripts, caches, and artifacts.

**Files/Folders:**
- Delete: `debug_rucc.py`, `search_total_pop.py`, `.pytest_cache/`, `rhip_utils.egg-info/`, `.superpowers/`

**Step 1: Delete artifacts**
```bash
rm debug_rucc.py search_total_pop.py
rm -rf .pytest_cache rhip_utils.egg-info .superpowers
```

**Step 2: Commit**
```bash
git add .
git commit -m "chore: remove junk scripts and caches"
```

### Task 2: Project Restructuring
**Objective:** Move scattered files into organized subdirectories.

**Structure Changes:**
- `RuralDetention/` (Drafts/Notes) -> `/docs`
- `*.ipynb` -> `/notebooks`

**Step 1: Execute moves**
```bash
mkdir -p docs notebooks
mv "RuralDetention/Draft Abstract.md" docs/
mv RuralDetention/DataProcessing.ipynb notebooks/
rm -rf RuralDetention
```

**Step 2: Commit**
```bash
git add docs notebooks
git commit -m "refactor: reorganize project structure"
```

### Task 3: Git Hygiene
**Objective:** Prevent future clutter.

**Files:**
- Modify: `.gitignore`

**Step 1: Update .gitignore**
Add:
```
.pytest_cache/
*.pyc
__pycache__/
*.egg-info/
.DS_Store
.env
```

**Step 2: Commit**
```bash
git add .gitignore
git commit -m "chore: update gitignore"
```
