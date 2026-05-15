# Field Dictionary in MAPL3

## Overview

MAPL3 integrates a **field dictionary** that provides canonical metadata for
GEOS fields: long names, units, physical dimension, and conservation flag.
The dictionary is based on CF Conventions standard names where possible, and
tracks a **verification status** for each entry (`unverified`, `verified`, or
`cf_compliant`) to support a gradual migration toward full CF compliance.

Dictionary lookup is **opt-in per variable**: a field only queries the dictionary
when it is explicitly declared with `use_field_dictionary=.true.` in its
`make_VariableSpec` call (or, when ACG support is available, via a
`use_field_dictionary: true` column in the RC file — see
[GH #4594](https://github.com/GEOS-ESM/MAPL/issues/4594)).

This design means ad-hoc diagnostic fields and History gridcomp mirror variables
are never burdened with requiring a dictionary entry.

---

## Quick Start

### Opt in to dictionary lookup for a variable

In Fortran component code:

```fortran
var_spec = make_VariableSpec( &
     state_intent=ESMF_STATEINTENT_EXPORT, &
     short_name='T', &
     standard_name='air_temperature', &
     use_field_dictionary=.true., &   ! <-- opt in
     rc=status)
```

When `use_field_dictionary=.true.` and `air_temperature` is found in the
dictionary, `make_VariableSpec` sets:
- `long_name = 'Air Temperature'` (from dictionary, if not already supplied)
- `units = 'K'` (from dictionary, if not already supplied)

Caller-supplied values always win:

```fortran
! Caller-specified units take priority; long_name still filled from dict
var_spec = make_VariableSpec( &
     state_intent=ESMF_STATEINTENT_EXPORT, &
     short_name='T', &
     standard_name='air_temperature', &
     units='degC', &
     use_field_dictionary=.true., &
     rc=status)
```

### Without the flag — dictionary is not consulted

```fortran
! Default behaviour: no FD lookup, no warning
var_spec = make_VariableSpec( &
     state_intent=ESMF_STATEINTENT_EXPORT, &
     short_name='diag_field', &
     units='1', &
     rc=status)
```

No `standard_name` and no `use_field_dictionary` — units and long_name are
exactly what the caller provides (or unallocated if omitted).

---

## How Dictionary Lookup Works

When `use_field_dictionary=.true.` is passed to `make_VariableSpec`:

1. **Key selection**:
   - If `standard_name` is present and is not a compound vector name
     (i.e. does not contain `(`): use `standard_name` as the lookup key.
   - Otherwise: use `short_name` as an alias lookup key.
2. **Lookup**: search the singleton dictionary for the key.
3. **Apply defaults**: if found, use dictionary values as defaults for
   `long_name` and `units` (caller-supplied values always take priority).
4. **Warn on miss**: if not found, a warning is logged — this is never a
   fatal error in the current implementation.

### Compound vector names

Names of the form `(eastward_wind,northward_wind)` are compound encodings for
two-component vectors. They are never valid dictionary keys and are silently
skipped even when `use_field_dictionary=.true.`.

### Exempt item types

The following item types are always exempt from dictionary validation; no
warning is emitted even if the `standard_name` is absent from the dictionary:

| Item type | Exempt? |
|-----------|---------|
| `FIELD` | No — opt in with `use_field_dictionary=.true.` |
| `VECTOR` | No — opt in with `use_field_dictionary=.true.` |
| `BRACKET` | No — opt in with `use_field_dictionary=.true.` |
| `VECTORBRACKET` | No — opt in with `use_field_dictionary=.true.` |
| `SERVICE` | **Yes** — always exempt |
| `SERVICE_PROVIDER` | **Yes** — always exempt |
| `SERVICE_SUBSCRIBER` | **Yes** — always exempt |
| `FIELDBUNDLE` | **Yes** (may be enforced in future) |
| `STATE` | **Yes** (may be enforced in future) |
| `WILDCARD` | **Yes** — always exempt |
| `EXPRESSION` | **Yes** — always exempt |

---

## Configuring the Dictionary

### Default behavior

By default, MAPL3 looks for `geos_field_dictionary.yaml` in the current working
directory. The installed dictionary is placed in the `etc/` subdirectory of the
MAPL install prefix during `cmake --install`. Experiment setup is expected to
place (or symlink) it into the run directory before execution:

```bash
# In your experiment run directory:
ln -s /path/to/install/etc/geos_field_dictionary.yaml geos_field_dictionary.yaml
```

If `geos_field_dictionary.yaml` does not exist in the run directory, MAPL3 logs
a warning and skips dictionary lookups.

### Explicit configuration in cap.yaml

```yaml
mapl:
  field_dictionary:
    path: /path/to/my_field_dictionary.yaml   # explicit path
    validation_mode: permissive               # permissive (default) | strict
```

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `path` | string | `geos_field_dictionary.yaml` | Path to the YAML dictionary file |
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
         $RUNDIR/geos_field_dictionary.yaml
   ```

3. **Run**: MAPL3 automatically finds `geos_field_dictionary.yaml` in the working
   directory.

If the symlink is absent, MAPL3 logs a warning and runs without the dictionary.
In STRICT mode (explicit `path:` key), the absence of the dictionary file is a
fatal error.

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

### Error: "Field dictionary not found at configured path" (explicit path)

The file specified by the `path` key in `cap.yaml` does not exist. Either:

- Fix the path, or symlink the file as described in Experiment Setup above.
- Remove the `path:` key entirely to fall back to the default
  `geos_field_dictionary.yaml` (missing default = warning, not error).

### Dictionary not loaded (no warnings, no lookups)

- Verify `geos_field_dictionary.yaml` (or the configured `path`) exists in the
  run directory.
- Check `cap.yaml` for a `mapl: field_dictionary:` key; if absent, the default
  path `geos_field_dictionary.yaml` in CWD is used.
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
