## MODIFIED Requirements

### Requirement: Existing extensions are reused before creating a new one
Before creating a new extension item for a given export, the system
SHALL search the extensions already created from that export, anywhere
in the hierarchy, for one whose payload already matches what the new
import requires, and reuse it instead of creating a duplicate. The
search SHALL NOT depend on which component declared the connection that
created the existing extension, or on which component declares the new
one.

#### Scenario: Second importer needing the same variant reuses the first extension
- **WHEN** a second import, needing the same variant of an export as an
  already-resolved first import, is resolved
- **THEN** the second import is wired to the same extension item the
  first import uses, and no duplicate extension item or transform step
  is created

#### Scenario: Reuse across declaring components
- **WHEN** a parent declares a connection from a child's export to one
  import that needs a conversion, and a grandparent declares a
  connection from the same export to another import that needs the same
  conversion
- **THEN** both imports are wired to the same extension item, and only
  one transform step for that conversion exists in the hierarchy

#### Scenario: Different variant still creates a new extension
- **WHEN** a second import needs a different variant of the same export
  than any extension already created from that export
- **THEN** the system creates a new extension item for the second
  import without disturbing any existing extension already wired to
  other importers

## ADDED Requirements

### Requirement: Expression evaluations and collections are reused hierarchy-wide
An expression evaluation over a given set of inputs and goal
characteristics, and a callback or wildcard collection for a given
import, SHALL each be built at most once in the hierarchy. A later
resolution that needs the same evaluation or collection SHALL reuse it,
whichever component declares that resolution.

#### Scenario: Same expression evaluation reused by two declaring components
- **WHEN** two connections declared on different components need the
  same evaluation of one expression export with the same goal
  characteristics
- **THEN** both are wired to the same evaluation result item, and one
  expression transform exists for it

#### Scenario: Collection resolved twice is built once
- **WHEN** resolution for one wildcard or callback import runs again,
  from the same or a different declaring component
- **THEN** the existing collection is reused and no second collection
  is created
