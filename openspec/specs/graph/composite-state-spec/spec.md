# Composite State Spec Specification

## Purpose

Provides a recursive declaration mechanism for a state's shape — named
members, each a leaf item or a further nested state — so that a
component-declared composite export/import has structure a coupler can
reason about, instead of being an opaque, manually-populated state.

## Requirements

### Requirement: Composite declaration is a tree of named members
A composite declaration SHALL let a component declare a set of named
members, where each member is unambiguously either a leaf item
declaration (an ordinary single Field/FieldBundle declaration, unchanged
from today's single-item declaration) or another composite declaration
nested one level deeper. A member SHALL NOT be both at once.

#### Scenario: Leaf member is declared and retrieved
- **WHEN** a composite declaration declares a member as a leaf item
- **THEN** that member is retrievable by name afterward, reporting itself
  as a leaf, with the declared leaf item's own properties intact

#### Scenario: Nested member is declared and retrieved
- **WHEN** a composite declaration declares a member as a nested
  composite declaration
- **THEN** that member is retrievable by name afterward, reporting itself
  as nested, with the declared nested composite's own members intact

#### Scenario: A member is never ambiguous between leaf and nested
- **WHEN** any declared member of a composite declaration is queried
- **THEN** exactly one of "is a leaf" / "is nested" is true for it, never
  both and never neither

### Requirement: Member names are unique within one composite declaration level
A member name SHALL be unique within the composite declaration it is
declared on. The same name MAY be reused at a different nesting level
(a different composite declaration) without conflict.

#### Scenario: Duplicate member name at the same level is rejected
- **WHEN** a second member is declared with a name already used by an
  existing member of the same composite declaration
- **THEN** the declaration is rejected and the original member is
  unchanged

#### Scenario: Same name at different levels does not conflict
- **WHEN** a composite declaration and one of its nested members each
  declare a member using the same name
- **THEN** both declarations succeed independently, since they belong to
  different composite declarations

### Requirement: Composite declarations nest to arbitrary depth
The declaration mechanism SHALL impose no fixed limit on nesting depth: a
nested member's own composite declaration MAY itself declare further
nested members.

#### Scenario: Multiple levels of nesting are declarable
- **WHEN** a composite declaration declares a nested member, which itself
  declares a further nested member
- **THEN** all levels are independently retrievable by walking the
  membership from the top down

### Requirement: A composite declaration carries the same top-level identity as an ordinary declared item
A composite declaration SHALL carry a name and a state intent
(import/export/internal), the same identity information an ordinary
single-item declaration carries, so it can be registered as one of a
component's declared items alongside ordinary leaf declarations.

#### Scenario: Composite declaration is registerable like an ordinary declared item
- **WHEN** a component registers a composite declaration as one of its
  declared items, alongside ordinary leaf declarations
- **THEN** the composite declaration's own name and state intent are
  retrievable the same way an ordinary declared item's are

### Requirement: A composite declaration carries no item-level characteristics of its own
Beyond its top-level name and state intent, a composite declaration
SHALL carry no item-level characteristics (such as units, typekind,
geometry, or vertical grid) of its own. Such characteristics remain
declared independently on each leaf member, exactly as an ordinary
single-item declaration declares them today; a composite declaration is
purely a named grouping of members, not itself a characterizable item.

#### Scenario: Composite declaration exposes no characteristic properties
- **WHEN** a composite declaration is queried for any item-level
  characteristic (units, typekind, geometry, vertical grid)
- **THEN** no such property exists on the composite declaration itself —
  only its individual leaf members carry characteristics

#### Scenario: Leaf members keep independent characteristics under a shared composite
- **WHEN** two leaf members are declared under the same composite
  declaration with different characteristics (for example, different
  units)
- **THEN** each leaf member's own declared characteristics are retrieved
  independently, with neither influenced by the other or by the
  composite declaration that groups them
