# Project Rules

## Neovim Configuration Guardrails

- **Verify Lua Module Names**: When writing or updating Neovim plugin configurations (specifically `require('plugin-name')` calls):
  - Do not assume the module name is identical to the plugin's repository name.
  - Plugins with hyphens in the repo name (e.g., `virtual-types.nvim`) often use hyphenless names (e.g., `virtualtypes`) or underscores in Lua.
  - Always verify the module name by checking the installed plugin files (e.g., listing `~/.local/share/nvim/lazy/<plugin-name>/lua/`) before writing the config.
