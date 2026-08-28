## Purpose

Provide explicit phase-aware hooks for future in-memory checkpoint operations without changing established netCDF restart processing.

## ADDED Requirements

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

MAPL generic grid components SHALL attach existing restart read and write entry points to the corresponding internal restart phases.

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
- **THEN** procedure SHALL select explicit internal branch and perform no internal checkpoint operation in this proposal

#### Scenario: Write dispatch receives internal phase
- **WHEN** outer meta-component write restart procedure runs under internal restart write phase
- **THEN** procedure SHALL select explicit internal branch and perform no internal checkpoint operation in this proposal

### Requirement: Existing netCDF restart behavior remains compatible

Existing netCDF restart reads and writes SHALL continue using existing phases, checkpoint controls, file naming, and restart handler operations without behavior changes.

#### Scenario: Existing restart read
- **WHEN** component runs existing restart read phase with netCDF restart controls enabled
- **THEN** current netCDF restart read behavior SHALL execute as before

#### Scenario: Existing restart write
- **WHEN** component runs existing restart write phase with netCDF checkpoint controls enabled
- **THEN** current netCDF restart write behavior SHALL execute as before
