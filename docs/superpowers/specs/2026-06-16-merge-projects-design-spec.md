# Spec: Merge Projects into RHIP Directory

- **Date**: 2026-06-16
- **Status**: Approved

## 1. Context & Goals
The RuralDetention and UrbanScience research projects share overlapping methodologies, datasets (specifically the Rural-Urban Commuting Area (RUCA) codes), and helper code. Consolidating them under the `RHIP` project repository will improve code reuse, reduce duplicate dependencies/utilities, and simplify management.

## 2. Target Directory Structure
The files will be organized as follows inside the `RHIP` project directory:
```
RHIP/
├── docs/
│   └── superpowers/
│       └── specs/
│           └── 2026-06-16-merge-projects-design.md
├── rural-detention/
│   ├── AnalysisV1.ipynb
│   ├── Data Exploration.ipynb
│   ├── Draft Abstract.md
│   ├── RSS 2025_Jails Paper_v2.md
│   └── Rural Detention.md
├── urban-science/
│   ├── Data Exploration.docx
│   ├── Data Exploration.ipynb
│   ├── Data Exploration.md
│   ├── Data Exploration.pdf
│   ├── Data-Exploration.pdf
│   ├── Journal Information.md
│   ├── UrbanScience.md
│   ├── analysis_results_diversity.txt
│   ├── mlr_coefficient_trajectories.png
│   └── ruca_violin_panel.png
├── utils/
│   ├── __init__.py
│   └── ruca.py
├── thesis-code/   (Existing R scripts)
├── README.md
├── requirements.txt
└── .gitignore
```

## 3. Shared Code Refactoring
- The utility script `utils/ruca.py` will be placed in `RHIP/utils/ruca.py`.
- Subproject notebooks will resolve `utils` by appending the parent directory (`..`) to the Python system path:
  ```python
  import sys
  sys.path.append("..")
  from utils import collect_ruca_data
  ```

## 4. Dependencies
Create a root-level `requirements.txt` combining all python requirements:
- pandas
- numpy
- pytidycensus
- python-dotenv
- matplotlib
- statsmodels
- scikit-learn
- tabulate

## 5. Execution Plan
1. **Clean Worktrees**: Remove the old, broken git-worktree pointer in `RHIP/rural-detention`.
2. **Setup Directories**: Create `RHIP/rural-detention`, `RHIP/urban-science`, and `RHIP/utils`.
3. **Move Utilities**: Copy the identical `ruca.py` and `__init__.py` from `RuralDetention/utils` to `RHIP/utils`.
4. **Migrate Subprojects**:
   - Copy relevant files from `RuralDetention` to `RHIP/rural-detention/` (excluding `.venv`, `utils/`).
   - Copy relevant files from `UrbanScience` to `RHIP/urban-science/` (excluding `.venv`, `.git`, `.agent`, `utils/`, `.antigravitycli`, `.quarto`).
5. **Update Imports**: Edit the Jupyter notebooks `Data Exploration.ipynb` in both subfolders to include parent directory in python path for `utils` imports.
6. **Consolidate Gitignore**: Update `RHIP/.gitignore` to ignore `.venv`, `.quarto`, etc.
7. **Clean up**: After verification, delete the original directories `Projects/RuralDetention` and `Projects/UrbanScience`.
