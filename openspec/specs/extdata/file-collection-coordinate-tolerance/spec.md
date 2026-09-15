# file-collection-coordinate-tolerance Specification

## Purpose

Lets an ExtData file collection declare a coordinate-comparison
tolerance for its files, so that ExtData can request geom reuse for
files whose grids are numerically noisy but semantically identical,
without any tolerance-specific logic existing outside ExtData.

## Requirements

### Requirement: Optional per-collection coordinate tolerance configuration
An ExtData file collection configuration SHALL support an optional
`coordinate_tolerance` field. When absent, the collection's behavior is
unchanged from before this capability existed.

#### Scenario: Collection declares a coordinate tolerance
- **WHEN** a file collection's configuration includes a
  `coordinate_tolerance` value
- **THEN** ExtData accepts and retains that value for use with files
  belonging to that collection

#### Scenario: Collection omits coordinate tolerance
- **WHEN** a file collection's configuration does not include
  `coordinate_tolerance`
- **THEN** ExtData does not apply any coordinate tolerance for files in
  that collection

### Requirement: ExtData stamps configured tolerance onto file metadata
When a file collection has a configured `coordinate_tolerance`, ExtData
SHALL set that value as a `coordinate_tolerance` attribute on the
`FileMetadata` object for each file in that collection, using the
metadata's existing generic attribute-setting mechanism, before that
metadata is used to look up or construct a geom.

#### Scenario: Attribute stamped before geom lookup
- **WHEN** ExtData loads the metadata for a file belonging to a
  collection with a configured `coordinate_tolerance`
- **THEN** the `coordinate_tolerance` attribute is present on that
  file's metadata object at the time ExtData requests a geom for it

#### Scenario: No attribute stamped when tolerance not configured
- **WHEN** ExtData loads the metadata for a file belonging to a
  collection with no configured `coordinate_tolerance`
- **THEN** ExtData does not add a `coordinate_tolerance` attribute to
  that file's metadata object
</content>
