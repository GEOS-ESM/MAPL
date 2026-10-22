---
description: "Review a committed plan task. Usage: /review-task <label> [tier|provider/model] [focus...]  (tier default: opus; see ~/.config/opencode/models.md)"
subagent: false
---

Launch the `reviewer` subagent to review Plan-Task `$1`.

Model to launch it with (resolved from tier "$2", default opus, on the
active provider):

!`model-for "$2" opus 2>&1`

If the line above is a full `provider/model` ID, pass exactly that as the
subagent's model. If it is an error message, stop and show it to me; do
not pick a model yourself.

Pass the reviewer only the label and any extra focus given here: "$3".
Do not summarize the work for it; it reads the commits itself.

When it returns, show me its report unchanged, then state which model
actually ran. Do not act on the findings until I say so.
