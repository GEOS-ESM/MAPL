## Purpose

Lets an ExtData file collection declare a coordinate-comparison
tolerance for its files, so that ExtData can request geom reuse for
files whose grids are numerically noisy but semantically identical,
without any tolerance-specific logic existing outside ExtData.

## ADDED Requirements

### Requirement: Per-collection coordinate tolerance configuration, defaulting to a nonzero value
An ExtData file collection configuration SHALL support an optional
`coordinate_tolerance` field, interpreted as a dimensionless fraction of
a grid's own coordinate spacing (DX) rather than an absolute coordinate
difference (see the `geom/latlon-coordinate-tolerance` capability).
When the field is absent, ExtData SHALL use a nonzero default tolerance
rather than treating the collection as if no tolerance applied, so that
collections which do not set this field retain the historically
tolerant (MAPL2) grid-reuse behavior existing users depend on. A
collection MAY set `coordinate_tolerance` explicitly to `0` to opt into
strict/exact coordinate comparison for its files.

#### Scenario: Collection declares an explicit coordinate tolerance
- **WHEN** a file collection's configuration includes a
  `coordinate_tolerance` value
- **THEN** ExtData accepts and retains that exact value for use with
  files belonging to that collection

#### Scenario: Collection omits coordinate tolerance
- **WHEN** a file collection's configuration does not include
  `coordinate_tolerance`
- **THEN** ExtData retains a nonzero default tolerance for files
  belonging to that collection

#### Scenario: Collection explicitly opts into strict comparison
- **WHEN** a file collection's configuration includes
  `coordinate_tolerance` set to `0`
- **THEN** ExtData retains a tolerance of `0` for files belonging to
  that collection, disabling tolerant comparison for it

### Requirement: ExtData stamps its effective tolerance onto file metadata
For every file, whether its collection's `coordinate_tolerance` is
explicit or defaulted, ExtData SHALL set that effective value as a
`coordinate_tolerance` attribute on the `FileMetadata` object for that
file, using the metadata's existing generic attribute-setting
mechanism, before that metadata is used to look up or construct a geom.
Per the directional comparison semantics of
`geom/latlon-coordinate-tolerance`, this attribute only affects lookups
where this file's own grid is the new candidate being matched against
an already-registered grid; it has no effect on how other,
already-registered grids are matched against.

#### Scenario: Attribute stamped before geom lookup
- **WHEN** ExtData loads the metadata for a file belonging to any
  collection
- **THEN** the `coordinate_tolerance` attribute (explicit or defaulted)
  is present on that file's metadata object at the time ExtData
  requests a geom for it
</content>
