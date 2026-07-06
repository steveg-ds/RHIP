# Neovim Icon Rendering Troubleshoot Plan

**Goal:** Fix missing/broken icon rendering in Neovim.

**Context:** `init.lua` expects `vim.g.have_nerd_font = true`. Plugins `which-key`, `mini.icons`, and `mini.statusline` depend on this variable.

**Steps:**

### Task 1: Verify Font Configuration
- **Objective:** Confirm Nerd Font is installed and configured in the terminal emulator.
- **Steps:**
    1. Ask user: "Is a Nerd Font set as your terminal emulator's primary font?" (e.g., FiraCode Nerd Font, JetBrainsMono Nerd Font).
    2. Check if specific Nerd Font glyphs render in terminal: `echo -e "\uf17c"` (Linux icon).

### Task 2: Validate Neovim Detection
- **Objective:** Ensure Neovim sees the font config.
- **Steps:**
    1. Open Neovim.
    2. Run `:lua print(vim.g.have_nerd_font)`.
    3. Run `:lua print(vim.o.guifont)` (if gui).

### Task 3: Plugin Diagnostics
- **Objective:** Check `mini.icons` and `nvim-web-devicons` state.
- **Steps:**
    1. Run `:checkhealth`.
    2. Run `:lua require('mini.icons').setup()` in command line to test.

### Task 4: Git-based Workflow
- **Objective:** Track configuration changes.
- **Steps:**
    1. Make changes to `~/.config/nvim/init.lua` as needed.
    2. `git add ~/.config/nvim/init.lua`
    3. `git commit -m "fix: troubleshoot icon rendering"`
