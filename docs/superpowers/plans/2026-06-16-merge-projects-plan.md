# Merge Projects Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Merge RuralDetention and UrbanScience projects into the RHIP project repository, deduplicating the shared RUCA utility code.

**Architecture:** Use subdirectories for each project under RHIP, sharing a root-level `utils/` directory. Append parent path in notebooks to load shared code.

**Tech Stack:** Python, Jupyter, git

---

### Task 1: Clean and Prepare Directory Structure

**Files:**
- Create: `utils/__init__.py`
- Modify: `rural-detention/.git` (delete)

- [ ] **Step 1: Backup and clean old RHIP/rural-detention**
  Keep `initial_data_exploration.pdf` from `rural-detention/`. Remove the invalid `.git` worktree file and `.gitignore` inside `rural-detention/` to prepare the directory for merge.
  Run:
  ```bash
  mv rural-detention/initial_data_exploration.pdf .
  rm -rf rural-detention/
  mkdir -p rural-detention
  mv initial_data_exploration.pdf rural-detention/
  ```

- [ ] **Step 2: Create utils directory**
  Create the shared utils folder structure.
  Run:
  ```bash
  mkdir -p utils
  ```

- [ ] **Step 3: Create utils init**
  Write `utils/__init__.py` to expose the ruca collection function.
  Code for `utils/__init__.py`:
  ```python
  from .ruca import collect_ruca_data
  ```

- [ ] **Step 4: Commit**
  Run:
  ```bash
  git add utils/__init__.py
  git commit -m "refactor: prepare directories for merge"
  ```

---

### Task 2: Migrate Shared Utilities

**Files:**
- Create: `utils/ruca.py`

- [ ] **Step 1: Copy ruca utility**
  Copy `ruca.py` from `RuralDetention/utils/ruca.py` to `RHIP/utils/ruca.py`.
  Run:
  ```bash
  cp ../RuralDetention/utils/ruca.py utils/ruca.py
  ```

- [ ] **Step 2: Verify shared utility executes**
  Run the script to verify it successfully downloads and processes RUCA codes.
  Run:
  ```bash
  python utils/ruca.py
  ```
  Expected output:
  `Total # of census tracts: 84414`
  `# of continental census tracts: 82673`

- [ ] **Step 3: Commit**
  Run:
  ```bash
  git add utils/ruca.py
  git commit -m "feat: migrate shared ruca utility"
  ```

---

### Task 3: Migrate RuralDetention Files & Update Imports

**Files:**
- Create/Copy: `rural-detention/` files
- Modify: `rural-detention/Data Exploration.ipynb`

- [ ] **Step 1: Copy RuralDetention files**
  Copy files from `Projects/RuralDetention` into `RHIP/rural-detention/`, excluding `.venv/` and `utils/`.
  Run:
  ```bash
  rsync -av --exclude='.venv' --exclude='utils' ../RuralDetention/ rural-detention/
  ```

- [ ] **Step 2: Update notebook imports**
  Modify `rural-detention/Data Exploration.ipynb` to append the parent directory `..` to `sys.path` before importing from `utils`.
  Locate cell containing `from utils import collect_ruca_data` (around line 87) and change it to:
  ```python
  import sys
  sys.path.append("..")
  from utils import collect_ruca_data
  ```

- [ ] **Step 3: Commit**
  Run:
  ```bash
  git add rural-detention/
  git commit -m "feat: migrate rural-detention files and update imports"
  ```

---

### Task 4: Migrate UrbanScience Files & Update Imports

**Files:**
- Create/Copy: `urban-science/` files
- Modify: `urban-science/Data Exploration.ipynb`

- [ ] **Step 1: Copy UrbanScience files**
  Copy files from `Projects/UrbanScience` into `RHIP/urban-science/`, excluding `.git`, `.venv`, `.agent`, `.antigravitycli`, `.quarto`, and `utils/`.
  Run:
  ```bash
  rsync -av --exclude='.git' --exclude='.venv' --exclude='.agent' --exclude='.antigravitycli' --exclude='.quarto' --exclude='utils' ../UrbanScience/ urban-science/
  ```

- [ ] **Step 2: Update notebook imports**
  Modify `urban-science/Data Exploration.ipynb` to append the parent directory `..` to `sys.path` before importing from `utils`.
  Locate cell containing `from utils import collect_ruca_data` (around line 96) and change it to:
  ```python
  import sys
  sys.path.append("..")
  from utils import collect_ruca_data
  ```

- [ ] **Step 3: Commit**
  Run:
  ```bash
  git add urban-science/
  git commit -m "feat: migrate urban-science files and update imports"
  ```

---

### Task 5: Consolidate Dependencies & Gitignore

**Files:**
- Create: `requirements.txt`
- Modify: `.gitignore`

- [ ] **Step 1: Create requirements.txt**
  Create root requirements combining all project dependencies.
  Code for `requirements.txt`:
  ```
  pandas
  numpy
  pytidycensus
  python-dotenv
  matplotlib
  statsmodels
  scikit-learn
  tabulate
  ```

- [ ] **Step 2: Update .gitignore**
  Append virtual envs, data checkpoints, and document output patterns to `RHIP/.gitignore`.
  Run:
  Verify `.venv` and python patterns are already covered in `.gitignore`. Add `urban-science/` ignored patterns if any (e.g. `.quarto/`).

- [ ] **Step 3: Commit**
  Run:
  ```bash
  git add requirements.txt .gitignore
  git commit -m "config: consolidate requirements and gitignore"
  ```

---

### Task 6: Cleanup and Verification

**Files:**
- None (deletion of old folders)

- [ ] **Step 1: Verify git status**
  Verify everything in `RHIP` is committed clean.
  Run:
  ```bash
  git status
  ```

- [ ] **Step 2: Delete old folders**
  Remove `Projects/RuralDetention` and `Projects/UrbanScience`.
  Run:
  ```bash
  rm -rf ../RuralDetention
  rm -rf ../UrbanScience
  ```
