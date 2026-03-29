# Field Dictionary in MAPL3

## Overview

MAPL3 integrates a **field dictionary** that provides canonical metadata for
GEOS fields: long names, units, physical dimension, and conservation flag.
When a component declares a field with a `standard_name`, MAPL3 automatically
fills in metadata from the dictionary — reducing duplication across component
specs and improving consistency across GEOS.

The dictionary is based on CF Conventions standard names where possible, and
tracks a **verification status** for each entry (`unverified`, `verified`, or
`cf_compliant`) to support a gradual migration toward full CF compliance.

---

## Quick Start

### Minimal component spec with dictionary lookup

```yaml
# MyComponent.yaml
mapl:
  states:
    export:
      air_temperature:
        standard_name: air_temperature
        dims: [lon, lat, lev]
        # long_name and units are filled automatically from the dictionary
```

When `air_temperature` is found in the dictionary, MAPL3 uses:
- `long_name: Air Temperature` (from the dictionary)
- `canonical_units: K` (from the dictionary)

These values become the defaults. Any keys you supply explicitly **override**
the dictionary values.

### Override individual fields

```yaml
mapl:
  states:
    export:
      surface_temp_degF:
        standard_name: air_temperature
        units: degF          # override the canonical units
        # long_name still comes from the dictionary
```

### Fully explicit spec (dictionary is not consulted)

```yaml
mapl:
  states:
    export:
      pressure:
        standard_name: air_pressure
        units: hPa
        long_name: Atmospheric Pressure
```

If `units` and `long_name` are both present in the component spec, the
dictionary is consulted only for the `conserved` flag (which drives the default
regrid method).

---

## How Dictionary Lookup Works

For each field entry in a component YAML file, MAPL3:

1. Checks the `itemType` — **service fields are exempt** (see below).
2. Looks up `standard_name` in the dictionary.
3. Uses dictionary values as **defaults** for `long_name` and `units`.
4. Applies any explicit overrides from the component YAML.
5. Uses the dictionary `conserved` flag to select the default regrid method
   (`CONSERVE` for conserved quantities, `BILINEAR` otherwise).
6. Emits a warning (PERMISSIVE mode) or error (STRICT mode) if the field is
   not found.

### Exempt item types

The following item types are always exempt from dictionary validation; no
warning is emitted even if the `standard_name` is absent from the dictionary:

| Item type | Exempt? |
|-----------|---------|
| `FIELD` | No — dictionary required |
| `VECTOR` | No — dictionary required |
| `BRACKET` | No — dictionary required |
| `VECTORBRACKET` | No — dictionary required |
| `SERVICE` | **Yes** |
| `SERVICE_PROVIDER` | **Yes** |
| `SERVICE_SUBSCRIBER` | **Yes** |
| `FIELDBUNDLE` | **Yes** (may be enforced in future) |
| `STATE` | **Yes** (may be enforced in future) |
| `WILDCARD` | **Yes** |
| `EXPRESSION` | **Yes** |

---

## Configuring the Dictionary

### Default behavior

By default, MAPL3 looks for `field_dictionary.yaml` in the current working
directory. Experiment setup is expected to place (or symlink) the dictionary
there before execution:

```bash
# In your experiment run directory:
ln -s /path/to/install/etc/geos_field_dictionary.yaml field_dictionary.yaml
```

The installed dictionary (`geos_field_dictionary.yaml`) is placed in the `etc/`
subdirectory of the MAPL install prefix during `cmake --install`.

If `field_dictionary.yaml` does not exist in the run directory, MAPL3 silently
skips dictionary lookups in PERMISSIVE mode.

### Explicit configuration in cap.yaml

```yaml
mapl:
  field_dictionary:
    path: /path/to/my_field_dictionary.yaml   # explicit path
    validation_mode: permissive               # permissive (default) | strict
```

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `path` | string | `field_dictionary.yaml` | Path to the YAML dictionary file |
| `validation_mode` | string | `permissive` | `permissive` = warnings only; `strict` = errors |

### Validation modes

| Mode | Missing dictionary file | Unknown standard_name |
|------|------------------------|----------------------|
| `permissive` | Silently skipped | Warning logged |
| `strict` | Fatal error | Fatal error |

Use `strict` mode once all your component specs have been validated to enforce
compliance going forward.

---

## Dictionary File Format

The dictionary is a YAML file in the GEOS_FieldDictionary v0.2.0 schema:

```yaml
field_dictionary:
  version: 0.2.0
  metadata:
    institution: NASA GMAO
    description: GEOS field dictionary

  entries:

    # CF-compliant entry
    air_temperature:
      long_name: Air Temperature
      canonical_units: K
      verification_status: cf_compliant
      conserved: false
      physical_dimension: temperature
      aliases: [T, TEMP]
      components: [MOIST, PHYSICS]

    # Conserved quantity — drives conservative regrid method
    mass_fraction_of_cloud_liquid_water_in_air:
      long_name: Mass Fraction of Cloud Liquid Water in Air
      canonical_units: kg kg-1
      verification_status: cf_compliant
      conserved: true
      physical_dimension: mass_fraction
      aliases: [QL]
      components: [MOIST]

    # Unverified entry (migrated from legacy schema)
    surface_upward_sensible_heat_flux:
      long_name: Surface Upward Sensible Heat Flux
      canonical_units: W m-2
      verification_status: unverified
      conserved: false
      physical_dimension: energy_flux
      aliases: [HFLUX]
      components: [SURFACE, TURBULENCE]
```

### Schema fields

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `long_name` | string | Yes | Human-readable description (used in NetCDF output) |
| `canonical_units` | string | Yes | Authoritative units (UDUNITS-2 compatible) |
| `verification_status` | enum | Yes | `unverified` / `verified` / `cf_compliant` |
| `conserved` | boolean | Yes | `true` → default regrid method is CONSERVE |
| `physical_dimension` | string | No | Category from controlled vocabulary |
| `aliases` | list | No | Short names used in code (documentation only) |
| `provenance.verified_by` | string | No | Name of scientist who verified the entry |
| `components` | list | No | GEOS components that use this field |

### Physical dimension vocabulary

```
temperature, pressure, density, velocity, acceleration
mass, mass_flux, mass_fraction, mole_fraction
energy, energy_flux, power
length, area, volume
concentration, mixing_ratio
frequency, angular_velocity
dimensionless, other
```

---

## Verification Status

Every dictionary entry carries a `verification_status` that tracks how
thoroughly it has been reviewed:

| Status | Meaning |
|--------|---------|
| `unverified` | Entry exists but has not been scientifically reviewed |
| `verified` | A domain scientist has reviewed units and long_name |
| `cf_compliant` | Entry is consistent with CF Conventions standard names |

The current production dictionary (`geos_field_dictionary.yaml`) contains:
- ~1,670 total entries
- 43 entries marked `cf_compliant` (consistent with CF standard names)
- The remainder marked `unverified` (migrated from the legacy schema)

MAPL3 does not enforce verification status at runtime in PERMISSIVE mode, but
logs a debug-level message for unverified fields. Future STRICT mode
enhancements may warn on unverified entries.

---

## Conserved Flag and Regrid Methods

The `conserved` flag in a dictionary entry drives the **default** horizontal
regrid method for that field:

| `conserved` | Default regrid method |
|-------------|----------------------|
| `false` | `BILINEAR` |
| `true` | `CONSERVE` |

This default can be overridden in the component YAML spec:

```yaml
mapl:
  states:
    export:
      mass_fraction_of_cloud_liquid_water_in_air:
        standard_name: mass_fraction_of_cloud_liquid_water_in_air
        regrid_method: bilinear    # override the dictionary default
```

Fields where `conserved: true` are typically:
- Mass mixing ratios (`kg kg-1`)
- Mass fractions
- Mole fractions
- Mass fluxes

---

## Providing a Custom Dictionary

You can use a custom dictionary instead of (or in addition to) the production
`geos_field_dictionary.yaml`. This is useful for:

- Testing with fictitious field names
- Adding new fields before they are merged into the production dictionary
- Overriding specific entries for a particular experiment

```yaml
# cap.yaml
mapl:
  field_dictionary:
    path: my_experiment_dictionary.yaml
    validation_mode: permissive
```

A custom dictionary must use the same YAML schema as the production dictionary.
See `generic3g/tests/field_dictionary_test.yaml` for an annotated example
covering all schema features.

---

## Experiment Setup

For a standard GEOS experiment:

1. **Install MAPL**: `cmake --install` places `geos_field_dictionary.yaml` in
   `$INSTALL_PREFIX/etc/`.

2. **Link the dictionary into your run directory:**
   ```bash
   ln -s $INSTALL_PREFIX/etc/geos_field_dictionary.yaml \
         $RUNDIR/field_dictionary.yaml
   ```

3. **Run**: MAPL3 automatically finds `field_dictionary.yaml` in the working
   directory.

If the symlink is absent, MAPL3 runs without the dictionary (PERMISSIVE mode).
In STRICT mode, the absence of the dictionary file is a fatal error.

---

## Troubleshooting

### Warning: "standard_name not found in field dictionary"

The field's `standard_name` does not appear in the loaded dictionary. Options:

1. **Check the standard name**: Ensure the spelling matches the dictionary
   entry exactly (case-sensitive, underscores).
2. **Add the entry**: If this is a new GEOS-specific field, add it to
   `GEOS_FieldDictionary/geos_field_dictionary.yaml` and open a PR to
   `GEOS-ESM/GEOS_FieldDictionary`.
3. **Use permissive mode**: If you intentionally want to skip validation,
   set `validation_mode: permissive` in `cap.yaml`.

### Error: "FieldDictionary file not found" (STRICT mode)

The file specified by `path` (or the default `field_dictionary.yaml`) does not
exist in the run directory. Either:

- Symlink `geos_field_dictionary.yaml` as described in Experiment Setup above.
- Switch to `validation_mode: permissive` to make the missing file a non-fatal
  condition.

### Dictionary not loaded (no warnings, no lookups)

- Verify `field_dictionary.yaml` (or the configured path) exists in the run
  directory.
- Check `cap.yaml` for a `field_dictionary.path` key; if absent, the default
  path `field_dictionary.yaml` is used.
- Enable debug logging to see dictionary load messages.

---

## See Also

- Production dictionary: `GEOS_FieldDictionary/geos_field_dictionary.yaml`
- Test dictionary: `generic3g/tests/field_dictionary_test.yaml`
- Schema RFC: `GEOS_FieldDictionary/docs/schema-rfc-v0.2.0.md`
- Developer reference: `docs/mapl3/field_dictionary_developer.md`
- GEOS_FieldDictionary repository: https://github.com/GEOS-ESM/GEOS_FieldDictionary
- CF Conventions: https://cfconventions.org/standard-names.html
- MAPL issues: [#4438](https://github.com/GEOS-ESM/MAPL/issues/4438) (epic),
  [#4440](https://github.com/GEOS-ESM/MAPL/issues/4440) (core infrastructure)

---

*Last updated: March 2026*
*MAPL Version: 3.0*
