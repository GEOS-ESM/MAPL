---
name: github-workflow
description: GitHub workflow, commit conventions, and PR process for MAPL
compatibility: opencode
---

## What I Do

Provide comprehensive Git and GitHub workflow guidance for MAPL development, including:
- Critical rules about commits and pushes
- Branch strategy and naming conventions
- Commit message formatting
- Pull request process and checklist
- Code review expectations

## When to Use Me

Use this skill when:
- Starting new work on MAPL
- Creating commits or pull requests
- Unsure about branch strategy
- Preparing for code review
- Contributing to MAPL for the first time

## CRITICAL RULES

### ⚠️ THREE ABSOLUTE RULES ⚠️

1. **DO NOT COMMIT WITHOUT PERMISSION**
2. **DO NOT PUSH TO GITHUB WITHOUT PERMISSION**
3. **FIRST LINE OF GIT COMMIT MUST BE < 50 CHARACTERS**

These rules exist to maintain code quality and prevent accidental breaking changes.

## Branch Strategy

### Base Branches

**IMPORTANT:** Always clarify which base branch to use before starting work!

#### develop

**Purpose:** Legacy MAPL (MAPL 2.x) maintenance and development

**Use for:**
- Bug fixes for MAPL 2.x
- Features for current production version
- Maintenance and stability improvements

**Status:** Stable, production-ready code

#### release/MAPL-v3

**Purpose:** MAPL v3 pre-release development

**Use for:**
- New features for MAPL v3
- MAPL v3 specific improvements
- Next major version development

**Status:** Active development, pre-release

### Feature Branch Naming Convention

All development work happens on feature branches that follow this pattern:

```
feature/#ISSUE-description
bugfix/#ISSUE-description
hotfix/#ISSUE-description
```

Where:
- `#ISSUE` is the GitHub issue number (e.g., `#4392`)
- `description` is a brief kebab-case description

**Examples:**
```
feature/#4392-add-opencode-agent-skills
bugfix/#4388-vector-basis-kind-default
feature/#4376-support-rotated-vectors
hotfix/#4390-memory-leak-gridcomp
```

### Workflow: Create Feature Branch

**Step 1: Create GitHub issue FIRST**

Before creating a branch, you need an issue number:

```bash
# Create issue via gh CLI
gh issue create --title "Add support for rotated vectors"

# Or create via web interface
# https://github.com/GEOS-ESM/MAPL/issues/new
```

**Step 2: Checkout base branch**

```bash
# For MAPL 2.x work
git checkout develop
git pull origin develop

# For MAPL v3 work  
git checkout release/MAPL-v3
git pull origin release/MAPL-v3
```

**Step 3: Create feature branch**

```bash
# Using issue number from Step 1
git checkout -b feature/#4376-support-rotated-vectors
```

**Step 4: Do your work, create commits**

```bash
# Make changes
git add <files>
git commit -m "Add rotated vector support"

# More changes
git add <files>
git commit -m "Add tests for rotated vectors"
```

**Step 5: Push (ONLY WITH PERMISSION)**

```bash
git push -u origin feature/#4376-support-rotated-vectors
```

## Commit Message Format

### First Line (< 50 characters - MANDATORY)

**Format:** Imperative mood, concise summary

**Good examples:**
```
Add dark mode toggle to settings
Fix memory leak in GridComp
Refactor VerticalRegridder for clarity
Update documentation for ExtData
Remove deprecated HistoryV1 code
```

**Bad examples:**
```
Added support for the new dark mode feature in settings (too long!)
fix (too vague)
WIP (not descriptive)
Updated code (what code? how?)
```

**Imperative mood means:** Command form, as if giving an instruction
- ✓ "Add feature" not "Added feature" or "Adding feature"
- ✓ "Fix bug" not "Fixed bug" or "Fixes bug"
- ✓ "Refactor code" not "Refactored code"

### Optional Detailed Description

After first line, add blank line, then details:

```
Add rotated vector support to regridding

The regridding framework now supports vector fields defined
in rotated coordinate systems. This is required for regional
climate models that use rotated pole grids.

Implementation includes:
- New RotatedVectorTransform class
- Coordinate transformation matrices
- Tests for common rotation angles
- Documentation updates
```

### Commit Message Template

```
<verb> <what> in <where> (< 50 chars total)

<blank line>

<optional detailed explanation>
<why this change is needed>
<how it's implemented>
<any caveats or special considerations>
```

## Creating a Pull Request

### Prerequisites

Before creating PR:
- [ ] All commits follow message format (first line < 50 chars)
- [ ] Code follows MAPL coding standards (see `fortran-style` skill)
- [ ] Error handling is complete (see `mapl-error-handling` skill)
- [ ] Tests added/updated as appropriate
- [ ] Documentation updated if needed
- [ ] **CHANGELOG.md updated** (required for PRs to `develop` branch)
- [ ] Have permission to push

### Create PR via GitHub CLI

```bash
# Make sure feature branch is pushed
git push -u origin feature/#4376-support-rotated-vectors

# Create PR
gh pr create \
  --base develop \
  --title "Add rotated vector support to regridding" \
  --body "Fixes #4376"
```

Or via web interface: https://github.com/GEOS-ESM/MAPL/pulls

### PR Description Template

Pull requests should include:

```markdown
## Types of change(s)
- [ ] Bug fix (non-breaking change which fixes an issue)
- [ ] New feature (non-breaking change which adds functionality)
- [ ] Breaking change (fix or feature that would cause existing functionality to change)
- [ ] Trivial change (affects only documentation or cleanup)
- [ ] Refactor (no functional changes, no api changes)

## Checklist
- [ ] Tested this change with a run of GEOSgcm
- [ ] Ran the Unit Tests (`make tests` or `ctest`)

## Description

Brief description of what this PR does and why.

## Related Issue

Fixes #ISSUE_NUMBER
```

### PR Title Format

**Format:** Same as commit message - concise, imperative, < 50 chars if possible

**Examples:**
```
Add rotated vector support to regridding
Fix memory leak in GridComp initialization
Refactor VerticalRegridder for performance
Update ExtData documentation
```

## Updating CHANGELOG.md

**CRITICAL:** All PRs to the `develop` branch MUST include a CHANGELOG.md entry.

### CHANGELOG Format

MAPL follows [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) format with these sections:

- **Added** - New features
- **Changed** - Changes to existing functionality
- **Deprecated** - Soon-to-be removed features
- **Removed** - Removed features
- **Fixed** - Bug fixes
- **Security** - Security fixes

### Where to Add Entry

Add your change under the `[Unreleased]` section in the appropriate category:

```markdown
## [Unreleased]

### Fixed
- Your bug fix here

### Added

- Your new feature here
- Another feature

### Changed

- Your change here
```

### CHANGELOG Entry Guidelines

**Good entries:**
```markdown
### Added
- Added rotated vector support to regridding framework
- Added MAPL_GetPartition() implementation with unit tests

### Fixed
- Fixed memory leak in GridComp initialization
- Fixed 32-bit integer overflow in QSsort

### Changed
- Update components.yaml
  - ESMA_env v5.17.0
    - Update to Baselibs 8.24.0
```

**Bad entries:**
```markdown
### Added
- Updated code (too vague)
- Bug fix (which bug? belongs in Fixed)
- Made changes (what changes?)
```

### Entry Format

**Simple changes:**
```markdown
- Added support for rotated vectors in regridding
```

**Complex changes with details:**
```markdown
- Update `components.yaml`
  - `ESMA_env` v5.17.0
    - Update to Baselibs 8.24.0
      - ESMF v9.0.0b08
      - GFE v1.22.0
```

**Multiple related changes:**
```markdown
- Added implementation for `mapl_GetPartition()` with unit tests
- Added backwards compatibility with non-CF dimensionless vertical coordinate in ExtData2G
- Added logic in History to check for consistent History and averaging coupler alarms
```

### When to Update CHANGELOG

**Update CHANGELOG when:**
- Adding new functionality
- Fixing bugs
- Changing existing behavior
- Removing features
- Updating dependencies (like components.yaml)

**Don't update CHANGELOG for:**
- Documentation-only changes (unless significant)
- Internal refactoring with no user-visible changes
- CI/build configuration (unless it affects developers)
- Whitespace/formatting fixes

### CHANGELOG Update Workflow

```bash
# 1. Make your code changes
git add src/MyFile.F90

# 2. Update CHANGELOG.md
# Edit CHANGELOG.md under [Unreleased] section
vi CHANGELOG.md

# 3. Commit together
git add CHANGELOG.md
git commit -m "Add rotated vector support to regridding

Implements support for vector fields in rotated coordinate systems.
Required for regional climate models using rotated pole grids."

# 4. Push and create PR
git push -u origin feature/#1234-rotated-vectors
gh pr create --base develop
```

**IMPORTANT:** Include CHANGELOG changes in the same commit as your code changes, or in a dedicated CHANGELOG commit if your PR has multiple commits.

## Code Review Process

### What Reviewers Check

From the Code Review Checklist:

**Error Handling:**
- [ ] Return `_SUCCESS` at end of procedures
- [ ] Use of `_RC` macro where appropriate
- [ ] Always check iostat for read/write operations
- [ ] All allocations checked with `_STAT`

**Optional Variables:**
- [ ] KeywordEnforcer used for growable list of optional arguments

**Code Quality:**
- [ ] Procedure not too long (consider breaking up)
- [ ] Clear, descriptive variable names
- [ ] Not too many arguments (consider using derived type)
- [ ] No "feature envy" (procedure accessing many fields of another type)

**Thread Safety:**
- [ ] No module variables (except parameters)
- [ ] No saved variables in procedures
- [ ] Private state in component workspace

**Style:**
- [ ] Follows MAPL Fortran coding standards
- [ ] Appropriate indentation (3 spaces)
- [ ] Modern operators (`>`, `==` not `.gt.`, `.eq.`)
- [ ] `implicit none` in all modules

### Responding to Review Comments

**Be responsive:**
- Address all comments (resolve or discuss)
- Make requested changes promptly
- Push updates to same branch

**Be collaborative:**
- Code review is about code quality, not personal criticism
- Ask questions if feedback is unclear
- Explain your reasoning if you disagree (respectfully)

**Update PR:**
```bash
# Make requested changes
git add <files>
git commit -m "Address review comments

- Fix error handling in process_data
- Rename confusing variable names
- Add missing assertions"

# Push updates (with permission)
git push
```

## Common Workflows

### Syncing with Base Branch

Keep your feature branch up to date with base branch:

```bash
# On your feature branch
git fetch origin

# Merge base branch into feature branch
git merge origin/develop  # or origin/release/MAPL-v3

# Resolve any conflicts
git add <resolved-files>
git commit -m "Merge develop into feature branch"

# Push (with permission)
git push
```

### Fixing Commit Messages

**If you haven't pushed yet:**

```bash
# Fix most recent commit message
git commit --amend

# This opens editor to modify message
# Save and close to update
```

**If you already pushed:** Be very careful with amend/rebase after pushing. Discuss with team first.

### Splitting Large Changes

Large PRs are hard to review. Consider splitting:

**Option 1: Multiple PRs in sequence**
```bash
# PR 1: Refactoring prep work
feature/#4380-refactor-regridder-prep

# PR 2: New feature (depends on PR 1)
feature/#4376-add-rotated-vectors
```

**Option 2: Multiple commits in one PR**

Organize commits logically in single PR:
```
commit 1: Refactor existing code for clarity
commit 2: Add new RotatedVectorTransform class
commit 3: Integrate into regridding framework
commit 4: Add tests
commit 5: Update documentation
```

## Merge Process

**DO NOT merge your own PR without approval!**

After approval:
1. Ensure CI/CD checks pass
2. Squash or merge as appropriate (discuss with team)
3. Delete feature branch after merge

**Typical merge:** Maintainer merges approved PRs

## Advanced: Working with develop and release/MAPL-v3

### Flow Direction

Changes flow from develop to release/MAPL-v3:

```
develop → release/MAPL-v3
```

**This means:**
- Bug fixes on develop will eventually flow to release/MAPL-v3
- Features specific to MAPL v3 stay on release/MAPL-v3
- Critical hotfixes may need to be applied to both branches

### Cherry-picking Between Branches

Sometimes need to apply same fix to both branches:

```bash
# Fix merged to develop
# Now apply to release/MAPL-v3

git checkout release/MAPL-v3
git cherry-pick <commit-hash-from-develop>
git push  # With permission
```

**Use sparingly - usually develop flows to v3 naturally**

## Troubleshooting

### "diverged from origin"

**Problem:** Local and remote branches have different histories

**Solution:**
```bash
git fetch origin
git status  # See how many commits ahead/behind

# If you haven't pushed problematic commits
git reset --hard origin/<branch-name>  # CAUTION: loses local commits

# If coordination needed with team
# Discuss before force pushing or resetting
```

### Accidentally Committed to develop

**Problem:** Made commits directly on develop instead of feature branch

**Solution:**
```bash
# Create feature branch from current develop
git checkout -b feature/#ISSUE-emergency-fix

# Reset develop to match origin
git checkout develop
git reset --hard origin/develop

# Continue work on feature branch
git checkout feature/#ISSUE-emergency-fix
```

### Want to Combine Multiple Commits

**Before pushing:**

```bash
# Interactive rebase last 3 commits
git rebase -i HEAD~3

# Mark commits to squash in editor
# Save and close
```

**After pushing:** Discuss with team first (requires force push)

## Quick Reference

### Starting New Work

```bash
# 1. Create GitHub issue first (get #ISSUE number)
gh issue create --title "Your feature description"

# 2. Checkout base branch
git checkout develop  # or release/MAPL-v3
git pull origin develop

# 3. Create feature branch
git checkout -b feature/#ISSUE-description

# 4. Do work, commit with proper messages (< 50 char first line)
git add <files>
git commit -m "Add feature description"

# 5. Push (with permission)
git push -u origin feature/#ISSUE-description

# 6. Create PR
gh pr create --base develop
```

### Commit Message Quick Check

```bash
# Check first line length
git log --oneline -1 | wc -c  # Should be < 52 (50 + newline + SHA)

# View last commit message
git log -1

# Amend if needed (before pushing)
git commit --amend
```

## Related Skills

- **`fortran-style`** - Coding standards for code review
- **`mapl-error-handling`** - Error handling review checklist
- **Code Review Checklist** - Available on MAPL wiki

## Summary

**Critical Reminders:**
1. ⚠️ DO NOT commit without permission
2. ⚠️ DO NOT push without permission
3. ⚠️ First line of commit < 50 characters
4. Always create GitHub issue first (get issue number)
5. Use correct base branch (develop vs release/MAPL-v3)
6. Follow feature branch naming: `feature/#ISSUE-description`
7. Write clear, imperative commit messages
8. Fill out PR template completely
9. Respond to code review feedback
10. Wait for approval before merging

**When in doubt:** Ask the team! Better to clarify than to cause problems.
