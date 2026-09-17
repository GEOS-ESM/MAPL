# 16. Ordinary Inout Items

Status: `[DEFERRED]` — explicitly set aside for the time being at the spec
author's request. Retained below as reference for whenever this is picked
back up; nothing in this document should be treated as scheduled or
in-scope for the current implementation pass, in addition to (not instead
of) the pre-existing `[SPECULATIVE]` caveat below.

Original status: `[SPECULATIVE]` — "emerging possibility," explicitly not a
solved problem. Do not implement beyond the direct-alias special case
without further design work.

## 16.1 Proposed model

An ordinary (non-callback) inout item MAY be supported through two
`DependencyNetwork`s:

```
Forward network:  owner value -> transforms -> borrower value
Return network:   borrower value -> reverse transforms -> owner value
```

**Proposed sequencing:**

1. Before borrower execution: update the borrower value through the
   forward network.
2. Borrower executes (its own `MethodGraphNode`/`TransformGraphNode` runs).
3. After borrower execution: update the owner value through the return
   network.

## 16.2 What is settled

**REQ-INOUT-001.** The direct-alias case (owner and borrower share the same
underlying ESMF payload via `ESMF_NamedAlias`, no grid/units/precision
mismatch) requires no transforms in either direction. This degenerate case
is settled and follows directly from REQ-EXT-003's no-op principle.

## 16.3 What is explicitly NOT settled

The following are open and MUST NOT be assumed resolved:

- **Revision authority under two producers.** REQ-DEP-008 limits each
  `StateItemNode` to one producer *per network*. An inout owner value has
  a producer in the forward network's *source* role but is also the
  *target* of the return network's reverse transform. Which network's
  write "wins" if both could plausibly run in the same update cycle, and
  how revision comparison should treat a value that is legitimately
  written from two different network contexts, is unresolved.
- **Lazy direction selection.** The text raises the possibility that
  revision semantics could eventually allow choosing, lazily, whether to
  propagate forward or return-direction updates first/at all, based on
  which side actually changed. This requires further investigation before
  any implementation attempt; no algorithm is specified here.
- **Recursion.** If a borrower is itself an owner of a further inout
  relationship (chained borrowing), recursion behavior is unspecified.
- **Authority rules in general.** Who is allowed to initiate a
  forward/return cycle, and under what conditions, is unspecified.

**REQ-INOUT-002.** Any implementation of non-trivial (non-direct-alias)
inout support MUST be preceded by an explicit design addendum resolving
the above points. This document intentionally does not provide enough
detail to implement the general case.
