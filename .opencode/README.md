# OpenCode agents and commands for MAPL3

A short guide to the agents, commands and helper scripts used for
agent-assisted MAPL work in this checkout. Some live in this repo and
some in your home directory; the table says which.

## Where things live

| What | Location | Shared via repo? |
|---|---|---|
| `reviewer`, `triage` agents | `.opencode/agents/` | yes |
| `/review-task`, `/opsx-*` commands | `.opencode/commands/` | yes |
| Skills (`mapl-build`, `mapl-testing`, ...) | `.opencode/skills/` | yes |
| `build-fixer`, `ice-reproducer` agents | `~/.config/opencode/agents/` | no, copy them |
| `/build-fix`, `/nag-*` commands | `~/.config/opencode/commands/` | no, copy them |
| Model tier table | `~/.config/opencode/models.md` | no, copy it |
| `with-nag`, `with-gfortran`, `with-intel`, `model-for` | `~/.local/bin/` | no, copy them |

Repo instructions for agents are in `AGENTS.md` (top level) and
`superstructure/generic/AGENTS.md`.

## Helper scripts

### `with-nag` / `with-gfortran` / `with-intel`

Each agent shell call is a fresh shell, so `module load` never carries
over to the next call. The wrappers purge modules, load one toolchain,
and run one command:

```bash
with-nag cmake --build nag -j 10 --target build-tests > nag/build.log 2>&1; tail -3 nag/build.log
with-gfortran ctest --test-dir gfortran -L ESSENTIAL -j 6 > gfortran/ctest.log 2>&1; tail -15 gfortran/ctest.log
```

They load the **unversioned** stack modules (`nag-stack`,
`gfortran-stack`, `ifx-stack`). Each machine's module default must be
the version MAPL needs. In particular, newer NAG releases have a
compiler bug that breaks MAPL, so `nag-stack` must default to 7.2.41.
With Lmod, set the default with a `.modulerc.lua` in the module
directory:

```lua
-- ~/modulefiles/Core/nag-stack/.modulerc.lua
module_version("nag-stack/7.2.41", "default")
```

Check with `with-nag nagfor -V` (should report Build 7241). Intel
(`ifx-stack`) is only available on bucy. All three wrappers share
`~/.local/bin/with-toolchain.sh`; copy it along with them.

### `model-for`: model tiers

Agents and commands name a model **tier** (`opus`, `sonnet`, `haiku`,
`gemini`, `gemini-flash`, `gpt-sol`, `gpt-luna`), not a model ID,
because each provider spells model IDs differently.
`~/.config/opencode/models.md` maps tiers to IDs for each provider.

```bash
model-for --list                 # active provider and its mapping
model-for sonnet                 # e.g. nasa_litellm/claude-sonnet-5-5
echo copilot > ~/.config/opencode/active-provider   # switch provider (nasa_litellm | copilot | asksage)
OPENCODE_MODEL_PROVIDER=asksage model-for haiku     # one-off override
```

A `-` in the table means the provider has no equivalent (AskSage has
no Opus or Gemini); `model-for` then fails instead of guessing.

## Agents

| Agent | Tier | Edits? | Use for |
|---|---|---|---|
| `build-fixer` | sonnet | yes (no commits) | Fixing compile/link errors for one vendor |
| `triage` | haiku | no | First look at a failing build or ctest |
| `reviewer` | opus (selectable) | no | Reviewing a committed plan task |
| `ice-reproducer` | (pinned, older) | yes | Minimal reproducers for compiler ICEs |
| `cavecrew-*` | haiku / session model | varies | Terse investigate/build/review helpers (caveman plugin) |

### `build-fixer`: `/build-fix [nag|gfortran|intel] [build-dir]`

Loops build, find first error, minimal fix, rebuild. Defaults: vendor
`nag`, build dir named after the vendor. `/nag-fix [dir]` is a
shortcut for `/build-fix nag`.

- May fix production or test sources, but never weakens or deletes test
  checks to make things compile.
- Stops and reports on an internal compiler error, a recurring error,
  an uncertain fix, a fix that would break another vendor, or a missing
  module.
- Test failures after a successful build go to `triage`.
- Never commits, pushes, or switches compiler versions.

### `triage`

Read-only first-pass investigation. Give it the build dir and the
failing test names or log. It reads logs and code, may rerun single
tests, and reports **suspected** root causes. The main session should
verify them before acting. It is cheap, so use it before reading large
logs in the main session.

### `reviewer`: `/review-task <label> [tier|provider/model] [focus]`

Read-only review of a task from
`docs/graph/plans/separation-of-concerns.md`. Task agents leave their
work **uncommitted**, with a proposed message in `.git/PLAN_TASK_MSG`,
so you can review the working tree yourself (e.g. magit status). The
reviewer reads `git diff HEAD` itself, or the task's commits (via their
`Plan-Task: <label>` trailer) if already committed, so it never sees
the author's own summary of the work.

```text
/review-task B3-Units                        # opus (default)
/review-task B3-Units gemini                 # cross-family second opinion
/review-task B3-Units gpt-sol "focus on MIRROR handling"
```

It reports a verdict (accept / accept with follow-ups / rework
needed) and findings by severity with file:line. Fixes go into the
working tree; then commit with `git commit -aF .git/PLAN_TASK_MSG`
(after `git add` for new files) or tell the author session "commit". Run reviews from
a separate subagent like this, not by switching models in the author's
session: the reviewer starts with no knowledge of the author's
reasoning, and it can't edit anything by accident.

## Other commands

- `/nag-cmake`, `/nag-build`, `/nag-build-tests`, `/nag-ctest`,
  `/nag-install`, `/nag-rebuild`: inline NAG build steps using
  `with-nag`; optional build dir argument (default `nag`).
- `/opsx-propose`, `/opsx-apply`, `/opsx-update`, `/opsx-sync`,
  `/opsx-archive`, `/opsx-explore`: OpenSpec change workflow.

## Conventions worth knowing

- Build and test with NAG first (compiles faster), then gfortran. Use
  `ctest -L ESSENTIAL`.
- Run builds in the main session with output to a log; show only the
  tail. Never stream a full log into the conversation.
- Source comments explain *why*. No `openspec/changes/...` paths, task
  numbers, or change history (see `AGENTS.md`).
- Model assignments per task (who authors, who reviews) are in the
  plan's "Model assignment" section.
