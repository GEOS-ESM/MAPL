# internal-checkpoint Specification

## Purpose

Provide explicit phase-aware hooks for future in-memory checkpoint operations without changing established netCDF restart processing.

## Requirements

### Requirement: Distinct internal restart phases

MAPL SHALL define distinct generic phase names for internal restart reads and internal restart writes, separate from existing initialization restart and ordinary restart phases.

The new phases SHALL NOT be added to `GENERIC_INIT_PHASE_SEQUENCE`, whose existing order SHALL remain unchanged.

#### Scenario: Internal read phase is identifiable
- **WHEN** MAPL dispatches an internal restart read callback
- **THEN** current phase SHALL identify operation as internal restart read rather than existing netCDF restart read

#### Scenario: Internal write phase is identifiable
- **WHEN** MAPL dispatches an internal restart write callback
- **THEN** current phase SHALL identify operation as internal restart write rather than existing netCDF restart write

### Requirement: Generic restart entry points support internal phases

MAPL generic grid components SHALL attach restart read and write entry points to the corresponding internal restart phases.

#### Scenario: Internal read callback is registered
- **WHEN** generic grid component services are configured
- **THEN** existing read restart entry point SHALL be registered for internal restart read phase

#### Scenario: Internal write callback is registered
- **WHEN** generic grid component services are configured
- **THEN** existing write restart entry point SHALL be registered for internal restart write phase

### Requirement: Restart dispatch observes current phase

Outer meta-component restart procedures SHALL retrieve current phase from their ESMF grid component before selecting restart behavior.

#### Scenario: Read dispatch receives internal phase
- **WHEN** outer meta-component read restart procedure runs under internal restart read phase
- **THEN** procedure SHALL select the explicit internal branch and perform an in-memory checkpoint read as described in the in-memory checkpoint read requirement

#### Scenario: Write dispatch receives internal phase
- **WHEN** outer meta-component write restart procedure runs under internal restart write phase
- **THEN** procedure SHALL select the explicit internal branch and perform an in-memory checkpoint write as described in the in-memory checkpoint write requirement

### Requirement: In-memory checkpoint storage

`OuterMetaComponent` SHALL hold an in-memory checkpoint state that can store at most one snapshot at a time, containing separate nested states for the import, export, and internal states of the component.

#### Scenario: Storage is retained across restart calls
- **WHEN** a component instance completes an in-memory checkpoint write
- **THEN** the stored snapshot SHALL remain available to a subsequent in-memory checkpoint read on the same component instance without requiring any file I/O

#### Scenario: A new write replaces the prior snapshot
- **WHEN** an in-memory checkpoint write occurs on a component instance that already holds a stored snapshot
- **THEN** the previously stored snapshot SHALL be replaced by the new one, and only the most recent snapshot SHALL be retrievable afterward

### Requirement: In-memory checkpoint write performs deep copy

When the internal restart write phase executes, the outer meta-component SHALL perform a deep copy of each currently-enabled state (import, export, internal) into the in-memory checkpoint storage, independently allocating memory for the copied fields.

Which of the import, export, and internal states are copied SHALL be governed by the same checkpoint-control flags that govern existing netCDF checkpoint writes.

#### Scenario: Enabled states are deep-copied
- **WHEN** an in-memory checkpoint write executes and a state's checkpoint control flag is enabled
- **THEN** that state's fields SHALL be copied into independently allocated memory in the in-memory checkpoint storage, and subsequent modification of the live state's field data SHALL NOT alter the stored copy

#### Scenario: Disabled states are not copied
- **WHEN** an in-memory checkpoint write executes and a state's checkpoint control flag is disabled
- **THEN** that state SHALL NOT be copied into the in-memory checkpoint storage

### Requirement: In-memory checkpoint read performs data-only copy

When the internal restart read phase executes, the outer meta-component SHALL copy field data values from the stored in-memory checkpoint back into the corresponding live import, export, and internal states, without reallocating or replacing the live fields.

Which of the import, export, and internal states are restored SHALL be governed by the same restart-control flags that govern existing netCDF restart reads.

#### Scenario: Enabled states receive restored data
- **WHEN** an in-memory checkpoint read executes and a state's restart control flag is enabled and a stored snapshot exists for that state
- **THEN** the data values of that state's live fields SHALL be overwritten with the values from the stored snapshot, while the live fields themselves remain the same field objects

#### Scenario: Disabled states are left unmodified
- **WHEN** an in-memory checkpoint read executes and a state's restart control flag is disabled
- **THEN** that state's live field data SHALL NOT be modified by the read

#### Scenario: Read with no prior write
- **WHEN** an in-memory checkpoint read executes for a state that has no corresponding stored snapshot
- **THEN** the procedure SHALL fail with an error rather than silently leaving the live state unmodified

### Requirement: Existing netCDF restart behavior remains compatible

Existing netCDF restart reads and writes SHALL continue using existing phases, checkpoint controls, file naming, and restart handler operations without behavior changes.

#### Scenario: Existing restart read
- **WHEN** component runs existing restart read phase with netCDF restart controls enabled
- **THEN** current netCDF restart read behavior SHALL execute as before

#### Scenario: Existing restart write
- **WHEN** component runs existing restart write phase with netCDF checkpoint controls enabled
- **THEN** current netCDF restart write behavior SHALL execute as before
