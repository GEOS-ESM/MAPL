# Error Code Inventory: Named Error Code Catalog

This inventory records semantic grouping decisions before representative macro sites
receive canonical codes. Similar wording is a review signal, not an automatic merge
decision.

Canonical catalog entries may declare context fields. Site migrations use `_CTX`
variants to provide values such as file paths; they do not duplicate catalog prose.

## Review Rules

For each source message, reviewers classify:

- Failure condition and category
- Expected recovery or caller action
- Context needed to distinguish source sites
- Whether existing return/status semantics differ

Merge only when these dimensions are equivalent. Preserve site-specific context as
structured diagnostic fields. Record rejected near-matches rather than silently
discarding them.

## Inventory Status

| Source site | Existing message/condition | Candidate group | Decision | Notes |
|---|---|---|---|---|
| `base/SimpleBundleMod.F90` | Missing field or index | Bundle lookup failure | Approved: code 13 | Preserve field/name context |
| `mp_utils/Partition.F90` | Invalid partition argument | Partition argument validation | Approved: code 14 | Preserve `n`, `k`, and `min_extent` context |
| `superstructure/generic/UserSetServices.F90` | Unsupported DSO name | DSO validation | Approved: code 15 | Preserve name context; distinct from missing DSO |
| `superstructure/generic/vertical/FixedLevelsVerticalGrid.F90` | Unsupported dimension or aspect | Vertical-grid validation | Approved: code 16 | Preserve configuration context |
| `include/MAPL_private_state.h` | Private state missing or duplicate | Private-state lifecycle | Approved: code 17 | Preserve state-name context |

## Approved Merge Decisions

- Partition constraints share code 14 because they represent one invalid-argument
  category and differ only in supplied field values. They are not merged with bundle,
  DSO, or vertical-grid validation because recovery actions differ.
- Bundle missing field/index messages share code 13 because both represent failed bundle
  lookup; field and bundle names remain context values.
- Private-state creation and lookup share code 17 because both represent invalid
  private-state lifecycle; operation and state name remain context values.
- Similar words such as “unsupported type” are not merged across unrelated field,
  state, DSO, and vertical-grid domains.

## Full Scan Record

Raw source scan on 2026-08-14 found 314 `_ASSERT`, `_FAIL`, or `_VERIFY` references
across 100 MAPL-owned source/test files, excluding build output, specifications, and
generator files. Five representative groups are migrated above; all remaining sites
remain legacy and require future inventory rows before migration.

## Completion Rules

- Every migrated `_ASSERT` and `_FAIL` site has one inventory row.
- Every canonical group lists all source sites, canonical code, rationale, and retained
  context fields.
- Every rejected near-match records why separate codes remain necessary.
- Remaining legacy sites are listed with migration status and compatibility diagnostic.
- Full scan count and excluded paths are recorded above; remaining sites stay legacy
  until individually classified.
