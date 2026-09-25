## MODIFIED Requirements

### Requirement: Restart dispatch observes current phase

Outer meta-component restart procedures SHALL retrieve current phase from their ESMF grid component before selecting restart behavior.

#### Scenario: Read dispatch receives internal phase
- **WHEN** outer meta-component read restart procedure runs under internal restart read phase
- **THEN** procedure SHALL select the explicit internal branch and perform an in-memory checkpoint read as described in the in-memory checkpoint read requirement

#### Scenario: Write dispatch receives internal phase
- **WHEN** outer meta-component write restart procedure runs under internal restart write phase
- **THEN** procedure SHALL select the explicit internal branch and perform an in-memory checkpoint write as described in the in-memory checkpoint write requirement

## ADDED Requirements

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
