---
description: "Review a committed plan task. Usage: /review-task <label> [opus|gemini|gpt-sol|<provider/model>] [focus...]"
subagent: false
---

Launch the `reviewer` subagent to review Plan-Task `$1`.

Model selection (second argument: "$2"):
- empty or `opus`: launch it with no model override (its default is Opus).
- `gemini`: launch it with model `nasa_litellm/gemini-3.1-pro-preview`.
- `gpt-sol`: launch it with model `nasa_litellm/gpt-5.6-sol`.
- anything containing `/`: use it verbatim as the model.
- anything else: stop and ask me which model I meant.

Pass the reviewer only the label and any extra focus given here: "$3".
Do not summarize the work for it; it reads the commits itself.

When it returns, show me its report unchanged, then state which model
actually ran. Do not act on the findings until I say so.
