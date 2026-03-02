# Multi-Session Implementation Strategy

**For:** MAPL3 Vertical Alignment Feature  
**Date:** February 13, 2026

---

## Why Multi-Session Approach?

As you correctly noted, AI agents can experience "context window senility" over long sessions. Breaking work into discrete, focused sessions provides:

1. **Clear checkpoints** - Each session ends with working, committable code
2. **Better focus** - 2-3 hour chunks are more productive than marathons
3. **Easy recovery** - Can resume from any point without full context
4. **Reduced risk** - Smaller units of change, easier to debug/revert

---

## Session Structure

⚠️ **CRITICAL RULE FOR AGENTS: Complete ONE task at a time and STOP for user review before proceeding to the next task.**

### Session 1: Foundation (Tasks 1-2)
**Goal:** Add coordinate_direction and vertical_alignment data structures

**Tasks:**
- Task 1: Add `coordinate_direction` to VerticalGrid base class and concrete implementations
  - **STOP after Task 1 for review before starting Task 2**
- Task 2: Add `vertical_alignment` to VariableSpec and VerticalGridAspect

**Deliverable:** New fields exist, can be set/get, unit tests pass, code compiles

**Time Estimate:** 1.5 days of focused work

**Commit Message Example:**
```
Add vertical alignment foundation (Tasks 1-2)

- Add coordinate_direction to VerticalGrid (default: downward)
- Add vertical_alignment to VariableSpec/VerticalGridAspect (default: with_grid)
- Add alignment resolution logic
- Unit tests for get/set and resolution
```

---

### Session 2: Degenerate Case (Tasks 3-4)
**Goal:** Handle same-grid copy/flip (immediate value)

**Tasks:**
- Task 3: Implement degenerate case in VerticalRegridTransform
  - **STOP after Task 3 for review before starting Task 4**
- Task 4: Update VerticalGridAspect matching logic

**Deliverable:** Same grid with different alignments works correctly

**Time Estimate:** 1.5 days

**Commit Message Example:**
```
Implement degenerate case copy/flip (Tasks 3-4)

- Detect same-grid case in VerticalRegridTransform
- Simple copy when alignments match
- Copy-flip when alignments differ
- Update aspect matching to allow same grid, different alignment
- Unit tests for degenerate cases
```

---

### Session 3: Full Regridding (Task 5)
**Goal:** Complete vertical regridding with alignment flips

**Tasks:**
- Task 5: Implement full regridding with pre/post flips

**Deliverable:** Different grids with different alignments work correctly

**Time Estimate:** 2-3 days

**Commit Message Example:**
```
Implement full vertical regridding with alignment (Task 5)

- Flip src to grid coordinates if needed
- Compute interpolation in grid coordinates
- Flip output to dst alignment if needed
- Support 0, 1, or 2 flips
- Update VerticalLinearMap for bidirectional support (if needed)
- Unit tests for all alignment combinations
```

---

### Session 4: Integration (Tasks 6-8)
**Goal:** Polish and complete the feature

**Tasks:**
- Task 6: ExtData YAML configuration support
  - **STOP after Task 6 for review**
- Task 7: Integration test scenarios
  - **STOP after Task 7 for review**
- Task 8: Documentation

**Deliverable:** Feature complete, documented, ready for release

**Time Estimate:** 2.5-3 days

**Commit Message Example:**
```
Complete vertical alignment integration (Tasks 6-8)

- ExtData YAML parser supports vertical_alignment field
- Integration test scenarios for all cases
- User documentation and release notes
- Feature complete for MAPL3 release
```

---

## How to Resume a Session

### At the End of Each Session

1. **Commit your work:**
   ```bash
   git add <modified files>
   git commit -m "Descriptive message with task number"
   ```

2. **Make notes:**
   - What tasks were completed
   - Any discoveries or deviations from plan
   - What's next

3. **Optional:** Push to remote for backup

### At the Start of Next Session

**IMPORTANT:** To help the agent understand context quickly, use this template:

**Template for Starting a Session:**
```
I'm implementing the MAPL3 vertical alignment feature.
The plan is in .opencode/plans/vertical-alignment-implementation.md

Current Status:
- Completed: [list completed tasks, e.g., "Tasks 1-2"]
- Starting: Task [number]

Task [number] Summary:
[Brief description from plan]
Files: [main files involved]
Goal: [what this task accomplishes]
```

**Example for Starting Task 1:**
```
I'm implementing the MAPL3 vertical alignment feature.
The plan is in .opencode/plans/vertical-alignment-implementation.md

Current Status:
- Completed: None (first task)
- Starting: Task 1

Task 1: Add coordinate_direction to VerticalGrid
Files: 
- vertical_grid/VerticalGrid.F90
- vertical_grid/BasicVerticalGrid.F90
- generic3g/vertical/FixedLevelsVerticalGrid.F90
- generic3g/vertical/ModelVerticalGrid.F90

Goal: Add coordinate_direction field to VerticalGrid base class and all 
concrete implementations. Default value should be "downward" (GEOS convention).
Need to add get/set methods and update constructors.
```

**Shorter Version (if agent seems familiar):**
```
Working on MAPL3 vertical alignment.
Plan: .opencode/plans/vertical-alignment-implementation.md
Status: Starting Task 1 (add coordinate_direction to VerticalGrid)
Please read the plan file and help me implement Task 1.
```

**What Happens Next:**

1. **Agent reads the plan file** - Gets full context from .opencode/plans/vertical-alignment-implementation.md
2. **Agent orients itself** - Reads relevant code files, checks test status
3. **We continue** - Start working on the specific task

**CRITICAL: Work on ONE Task at a Time**

⚠️ **DO NOT proceed to the next task without explicit user confirmation.**

After completing a task:
1. Stop and summarize what was done
2. Show test results
3. Ask: "Task [X] complete. Should I proceed to Task [X+1], or do you want to review/adjust first?"
4. Wait for user response before continuing

This allows for:
- Course correction if approach needs adjustment
- Review of implementation choices
- Validation that task is truly complete
- Discussion of any issues discovered

---

## What I Can Do to Resume Context

When you start a new session, I can:

- **Read files** - See what's been implemented
- **Search code** - Find related changes
- **Review git log** - Understand what's been done
- **Run tests** - Verify current state
- **Explore codebase** - Re-familiarize if needed

You don't need to re-explain everything - just tell me which task you're working on.

---

## Context Management Best Practices

### Keep Sessions Focused
- **Good:** 2-3 hours on a specific task
- **Avoid:** 8-hour marathon trying to complete everything

### Commit Frequently
- After each logical unit of work
- Before trying something experimental
- When tests pass

### Update the Plan
- If you discover the approach needs adjustment
- When effort estimates were wrong
- When priorities change

### Use Git History
- Commit messages are breadcrumbs
- I can read them to understand progress
- Branch if trying alternatives

---

## Weekend Work Suggestion

If you're working this holiday weekend, good starting point:

**Saturday:** Session 1 (Tasks 1-2) - Foundation
- Self-contained, good stopping point
- Can verify with tests before moving on

**Sunday/Monday:** Session 2 (Tasks 3-4) - Degenerate Case
- Builds on Saturday's foundation
- Delivers immediate value
- Still relatively simple

**Reassess next week:** 
- Continue with Session 3 (full regridding)
- Or handle other MAPL3 priorities

---

## What to Do If You Get Stuck

1. **Commit what you have** - Even if incomplete
2. **Start new session** - Tell me:
   - "Working on Task X, ran into issue Y"
   - Show me the code/error
3. **I can help:** 
   - Review your implementation
   - Suggest alternatives
   - Debug the issue

---

## Signs It's Time for a New Session

- You've been working 3+ hours
- You've completed a logical unit
- You're getting fatigued
- Something unexpected came up
- Tests are passing and you have committable code

---

## Emergency: If You Lose This Document

1. **Check:** `.opencode/plans/vertical-alignment-implementation.md`
2. **Check:** `.opencode/plans/multi-session-strategy.md`  
3. **Git log:** Commit messages have task numbers
4. **Start new session:** Tell me "I was working on vertical alignment, need to understand current state" - I can read the code and figure it out

---

## Key Principle

**Each session should end with working, committable code.**

Not necessarily "feature complete", but:
- Compiles without errors
- Tests pass (even if new tests not all written yet)
- No broken functionality
- Can be safely committed to version control

This way you always have a stable checkpoint to return to.

---

## Questions to Ask Yourself Each Session

**At Start:**
1. What task am I working on?
2. What's the goal for today?
3. How will I know I'm done?

**At End:**
1. Can I commit this?
2. Do tests pass?
3. What should I do next time?

---

## Summary

- **4 sessions** covering 8 tasks over ~10 days
- **Each session** = focused work on 1-3 related tasks
- **Each deliverable** = working, committable code
- **Easy resume** = just tell me which task, I'll orient myself
- **Flexible** = adjust plan as you discover things

This approach maximizes productivity while minimizing risk of context loss or accumulating broken code.

---

Have a great holiday weekend, and good luck with the implementation! 🎉
