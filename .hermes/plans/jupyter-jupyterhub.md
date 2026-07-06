# JupyterHub setup plan

- Commit changes to main.
- Create worktree: `../worktree-jupyter`.
- Deploy JupyterHub (no-auth).
- Expose via Tailscale.

## Steps
1. Commit current state.
2. `git worktree add ../worktree-jupyter`.
3. Launch JupyterLab in worktree.
4. Bind 0.0.0.0.
5. `tailscale serve --bg 8888`.
