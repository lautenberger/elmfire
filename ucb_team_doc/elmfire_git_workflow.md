# ELMFIRE Git Workflow (Team Standard)

## Roles & repos
- **Upstream (canonical):** Chris’s main repo — `ELMFIRE`
- **Team fork:** `elmfire_berkeley`
- **Personal forks:** each teammate’s fork of `ELMFIRE`

We’ll use these **remote names** consistently:
- `upstream` → Chris’s repo (canonical)
- `origin` → *your* personal fork (the repo you cloned)
- `team` → `elmfire_berkeley` (shared team fork)

> Tip: If anyone already uses different names, align them to avoid confusion.

---

## One-time setup (per developer)

1. **Clone your personal fork**
   ```bash
   git clone https://github.com/<you>/ELMFIRE.git
   cd ELMFIRE
   ```

2. **Add remotes**
   ```bash
   # Chris's canonical repo
   git remote add upstream https://github.com/<chris-org-or-user>/ELMFIRE.git

   # Team shared fork
   git remote add team https://github.com/<team-org>/elmfire_berkeley.git
   ```

3. **Verify remotes**
   ```bash
   git remote -v
   # expected:
   # origin   https://github.com/<you>/ELMFIRE.git (fetch/push)
   # upstream https://github.com/<chris>/ELMFIRE.git (fetch)
   # team     https://github.com/<team>/elmfire_berkeley.git (fetch/push)
   ```

> SSH is fine too; just use `git@github.com:...`.  
> If `upstream`/`team` already exist but point elsewhere, update:
> `git remote set-url upstream <url>` / `git remote set-url team <url>`.

---

## Daily workflow

### A) Keep your local & personal fork synced with Chris (upstream)

1. **Fetch latest from upstream**
   ```bash
   git fetch upstream
   ```

2. **Update your local main to match upstream main**
   ```bash
   git checkout main
   git reset --hard upstream/main
   ```

3. **Push the updated main to your personal fork**
   ```bash
   git push origin main --force
   ```
   > This keeps your fork’s `main` identical to Chris’s `main`.  
   > **Only force-push your personal fork** (never the team repo).

*(If your default branch is `master`, substitute accordingly.)*

---

### B) Create a work branch (in your personal fork)

1. **Branch from the up-to-date main**
   ```bash
   git checkout -b <yourname>/<feature-or-topic> main
   ```

2. **Commit as you work**
   ```bash
   # edit files...
   git add -A
   git commit -m "Meaningful message"
   git push -u origin <yourname>/<feature-or-topic>
   ```

---

### C) Publish your changes to the *team* repo in a branch under your name

> Goal: mirror your personal work onto a branch inside `elmfire_berkeley` for review/integration.

1. **Sync team’s main locally (optional but recommended)**
   ```bash
   git fetch team
   ```

2. **Create an integration branch based on the team’s main**
   ```bash
   git checkout -b <yourname>/<feature-or-topic>-team team/main
   ```

3. **Bring your personal branch commits into the team branch**

   **Option 1 — Merge (preserves history “as-is”)**
   ```bash
   git merge --no-ff origin/<yourname>/<feature-or-topic>
   ```

   **Option 2 — Rebase (linear, cleaner history)**
   ```bash
   git rebase team/main origin/<yourname>/<feature-or-topic>
   # After a successful rebase, move those commits onto the team branch:
   git checkout <yourname>/<feature-or-topic>-team
   git rebase origin/<yourname>/<feature-or-topic>
   ```
   Resolve any conflicts as they appear (`git status`, edit, `git add`, then `git rebase --continue`).

4. **Push the branch to the team repo**
   ```bash
   git push team <yourname>/<feature-or-topic>-team
   ```

5. **Open a Pull Request** (PR) **within the team repo**
   - **Base:** `elmfire_berkeley:main`
   - **Compare:** `elmfire_berkeley:<yourname>/<feature-or-topic>-team`
   - Add reviewers, link issues, and summarize the change.

---

## Keeping your feature branch up-to-date (while PR is open)

When Chris’s `main` moves forward:

```bash
# Update local main from upstream
git fetch upstream
git checkout main
git reset --hard upstream/main

# Rebase your personal branch
git checkout <yourname>/<feature-or-topic>
git rebase main
git push -f origin <yourname>/<feature-or-topic>

# Update the team branch the same way
git checkout <yourname>/<feature-or-topic>-team
git rebase team/main
git push -f team <yourname>/<feature-or-topic>-team
```

> Rebase keeps a clean history. If your team prefers merge commits, replace the `rebase` steps with `git merge main` / `git merge team/main` and push without `-f`.

---

## Conflict resolution tips

- See what’s conflicting:
  ```bash
  git status
  ```
- After fixing files:
  ```bash
  git add <file1> <file2> ...
  # if rebasing:
  git rebase --continue
  # if merging:
  git commit
  ```
- If a rebase goes sideways:
  ```bash
  git rebase --abort
  ```
- Visualize history:
  ```bash
  git log --oneline --graph --decorate --all
  ```

---

## Branch naming & protection (recommended)

- **Naming:** `<name>/<topic>` (e.g., `yiren/windninja-fixes`)
- **Protect team `main`:**
  - Require PRs
  - Require status checks to pass (CI/tests)
  - Restrict force pushes
- **Protect personal branches on team repo:** optional, but avoid force-pushing to `team` unless you’re fixing your own just-pushed branch.

---

## Quick checklist (per teammate)

1. Add `upstream` and `team` remotes (one-time).
2. Sync `main` from `upstream` regularly.
3. Work in `origin:<name>/<topic>`.
4. Create `team:<name>/<topic>-team` from `team/main`.
5. Merge/rebase your personal work into the team branch.
6. Push to `team` and open a PR against `team/main`.

---

## Common FAQs

- **“Should I open a PR from my personal fork to the team repo?”**  
  Prefer **PRs within the team repo** (branch in `elmfire_berkeley`) for shared visibility and CI.

- **“When do I use `--force`?”**  
  Only when updating **your own personal fork** or **your own branch** on the team repo after a rebase. Never force-push `team/main`.

- **“What if I started from an old base?”**  
  Rebase your feature branch onto the latest `upstream/main` before pushing to `team` to minimize conflicts.

---
