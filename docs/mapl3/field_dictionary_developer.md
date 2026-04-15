# Field Dictionary — Developer Reference

## Overview

This document describes the Fortran types and modules that implement the MAPL3
field dictionary subsystem, and explains how dictionary lookup is integrated
into `make_VariableSpec`.  For user-facing configuration, see
[docs/user_guide/docs/field_dictionary.md](../user_guide/docs/field_dictionary.md).

---

## Module Inventory

| Module | File | Purpose |
|--------|------|---------|
| `mapl3g_VerificationStatus` | `generic3g/VerificationStatus.F90` | Typed enum: `unverified` / `verified` / `cf_compliant` |
| `mapl3g_ValidationMode` | `generic3g/ValidationMode.F90` | Typed enum: `permissive` / `strict` |
| `mapl3g_FieldDictionaryItem` | `generic3g/FieldDictionaryItem.F90` | Single dictionary entry with getters |
| `mapl3g_FieldDictionary` | `generic3g/FieldDictionary.F90` | Dictionary container, YAML parser |
| `mapl3g_FieldDictionaryConfig` | `generic3g/FieldDictionaryConfig.F90` | Runtime config, exemption predicate |

---

## `mapl3g_VerificationStatus`

A typed enum following the MAPL pattern (see `esmf_utils/VectorBasisKind.F90`
for the reference pattern).

### Public API

```fortran
use mapl3g_VerificationStatus

! Parameter instances
type(VerificationStatus) :: s

s = VERIFICATION_STATUS_UNVERIFIED   ! id = 0
s = VERIFICATION_STATUS_VERIFIED     ! id = 1
s = VERIFICATION_STATUS_CF_COMPLIANT ! id = 2

! Constructor from string (case-insensitive)
s = VerificationStatus('cf_compliant')

! Comparison operators
if (s == VERIFICATION_STATUS_CF_COMPLIANT) ...
if (s /= VERIFICATION_STATUS_UNVERIFIED)  ...

! Conversion to string
print *, s%to_string()   ! 'unverified', 'verified', or 'cf_compliant'
```

### Notes

- Unknown strings default to `VERIFICATION_STATUS_UNVERIFIED` (no error).
- Both lowercase and uppercase strings are accepted by the constructor.

---

## `mapl3g_ValidationMode`

A typed enum with two values.

### Public API

```fortran
use mapl3g_ValidationMode

type(ValidationMode) :: m

m = VALIDATION_MODE_PERMISSIVE   ! id = 0 (default)
m = VALIDATION_MODE_STRICT       ! id = 1

! Constructor from string
m = ValidationMode('strict')

! Comparison
if (m == VALIDATION_MODE_STRICT) ...
```

---

## `mapl3g_FieldDictionaryItem`

Holds all metadata for a single dictionary entry.

### Types

```fortran
type :: CF_Provenance
   character(:), allocatable :: verified_by
end type

type :: FieldDictionaryItem
   ! (private members — use getters)
contains
   procedure :: get_long_name         ! character(:), allocatable
   procedure :: get_units             ! character(:), allocatable
   procedure :: get_physical_dimension ! character(:), allocatable
   procedure :: get_aliases           ! type(StringVector)
   procedure :: get_regrid_method     ! type(ESMF_RegridMethod_Flag)
   procedure :: get_verification_status ! type(VerificationStatus)
   procedure :: get_provenance        ! type(CF_Provenance)
   procedure :: is_conserved          ! logical
end type
```

### Constructors

```fortran
! Minimal — long_name and canonical_units only
item = FieldDictionaryItem(long_name, canonical_units)

! With one alias
item = FieldDictionaryItem(long_name, canonical_units, alias)

! With multiple aliases
item = FieldDictionaryItem(long_name, canonical_units, [alias1, alias2])

! Full — all optional fields available
item = FieldDictionaryItem( &
     long_name          = 'Air Temperature',   &
     canonical_units    = 'K',                 &
     aliases            = aliases_vector,       &   ! type(StringVector)
     physical_dimension = 'temperature',        &   ! optional
     conserved          = .false.,              &   ! optional, default .false.
     verification_status = VERIFICATION_STATUS_CF_COMPLIANT, &  ! optional
     provenance         = prov)                     ! optional, type(CF_Provenance)
```

### Regrid method derivation

The `regrid_method` member is set automatically in the constructor:

| `conserved` | `get_regrid_method()` |
|-------------|----------------------|
| `.false.` | `ESMF_REGRIDMETHOD_BILINEAR` |
| `.true.` | `ESMF_REGRIDMETHOD_CONSERVE` |

---

## `mapl3g_FieldDictionary`

Container for `FieldDictionaryItem` entries, keyed by CF standard name.  Also
maintains an alias map for reverse lookup by short name.

### Public API

```fortran
use mapl3g_FieldDictionary

type(FieldDictionary) :: dict

! Constructors
dict = FieldDictionary(filename='path/to/dict.yaml', rc=status)
dict = FieldDictionary(stream='yaml_string', rc=status)
dict = FieldDictionary()   ! empty dictionary

! Predicates
logical :: found
found = dict%has_item('air_temperature')     ! lookup by standard name
found = dict%has_item('T')                   ! lookup by alias

! Item access (returns pointer; check has_item first!)
type(FieldDictionaryItem), pointer :: item_ptr
item_ptr => dict%get_item('air_temperature', rc=status)

! Convenience accessors (by standard name or alias)
character(:), allocatable :: u, ln, sn
type(ESMF_RegridMethod_Flag) :: rm

u  = dict%get_units('air_temperature', rc=status)
ln = dict%get_long_name('air_temperature', rc=status)
sn = dict%get_standard_name('T', rc=status)   ! alias → standard name
rm = dict%get_regrid_method('air_temperature', rc=status)

! Size
integer :: n
n = dict%size()
```

### YAML parser behaviour

- The top-level YAML node is expected to be a mapping where each key is a CF
  standard name.
- For each entry, `long_name` and `canonical_units` are required; all other
  fields are optional with the defaults shown in the user guide.
- The parser also supports the `field_dictionary.entries` wrapper (v0.2.0
  schema with metadata header).  Entries without the wrapper (flat mapping)
  are also accepted for backward compatibility.
- Duplicate standard names are a fatal parse error.
- Ambiguous aliases (same alias pointing to two different standard names) are
  stored as `__ambiguous__`; a lookup by that alias fails with a clear error.

### Alias lookup rules

```fortran
! Aliases are registered automatically for each entry's 'aliases' list.
! get_standard_name(alias) returns the standard name, or fails if ambiguous.
! has_item(alias) returns .true. if the alias maps unambiguously to one entry.
! get_item(standard_name) requires the canonical key — aliases are not accepted.
```

---

## `mapl3g_FieldDictionaryConfig`

Holds the runtime configuration for the dictionary subsystem, parsed from the
`mapl/field_dictionary` section of `cap.yaml`.

### Public API

```fortran
use mapl3g_FieldDictionaryConfig

type(FieldDictionaryConfig) :: cfg

! Default constructor (path='field_dictionary.yaml', permissive mode)
cfg = FieldDictionaryConfig()

! Constructor from ESMF_HConfig node
type(ESMF_HConfig) :: node
cfg = FieldDictionaryConfig(node, rc=status)

! Accessors
character(:), allocatable :: path
path = cfg%get_dictionary_path()         ! 'field_dictionary.yaml' by default

type(ValidationMode) :: mode
mode = cfg%get_validation_mode()         ! VALIDATION_MODE_PERMISSIVE by default

! Predicates
logical :: has_path, exempt
has_path = cfg%has_dictionary_path()     ! .true. if path is non-empty

type(ESMF_StateItem_Flag) :: item_type
exempt = cfg%is_exempt(item_type)        ! .true. for SERVICE, STATE, etc.
```

### `is_exempt` logic

Returns `.true.` for item types that skip dictionary validation:

```fortran
is_exempt = any(item_type == [ &
     MAPL_STATEITEM_SERVICE,            &
     MAPL_STATEITEM_SERVICE_PROVIDER,   &
     MAPL_STATEITEM_SERVICE_SUBSCRIBER, &
     MAPL_STATEITEM_FIELDBUNDLE,        &
     MAPL_STATEITEM_STATE,              &
     MAPL_STATEITEM_WILDCARD,           &
     MAPL_STATEITEM_EXPRESSION          &
     ])
```

---

## Dictionary Initialization — `MaplFramework`

The dictionary singleton is loaded once, unconditionally, during
`MaplFramework%initialize` (called by `MAPL_Initialize` which is invoked at
program startup, and also by `MAPL_pFUnit_Initialize` for every pFUnit test
suite).

The relevant code lives in `mapl3g/MaplFramework.F90` in the
`initialize_field_dictionary` subroutine:

```
1. Read the optional 'field_dictionary' key from the mapl: section of cap.yaml
   (ESMF_HConfig).
   - If present, use that string as the path.
   - If absent, use the default path 'geos_field_dictionary.yaml' (CWD).

2. inquire(file=path, exist=file_exists)   ! avoid ESMF throw on missing file

3. if file_exists:
       call load_field_dictionary(path, rc)   ! populates the singleton
   else if path was explicitly configured:
       _ASSERT(.false., ...)                  ! fatal — explicit path must exist
   else (default path missing):
       lgr%warning(...)                       ! warn and continue without dict
```

The `inquire()` check is essential because `FieldDictionary(filename=...)` calls
`ESMF_HConfigCreateFromFile`, which calls `MAPL_Verify` internally.  If the
file is missing, `MAPL_Verify` would throw an exception into pFUnit's machinery
before the caller could check the status, causing every test suite to fail.

---

## Integration in `make_VariableSpec`

The per-field dictionary lookup is performed in
`generic3g/specs/VariableSpec.F90` inside the `make_VariableSpec` function.
Lookup is **opt-in**: it is only triggered when `use_field_dictionary=.true.`
is passed to `make_VariableSpec`.  The flag defaults to `.false.`, so existing
callers are entirely unaffected.

### `VariableSpec` member

```fortran
type VariableSpec
   ...
   logical :: use_field_dictionary = .false.
   ...
end type
```

### `make_VariableSpec` signature (relevant fragment)

```fortran
function make_VariableSpec( &
     state_intent, short_name, unusable, &
     ...
     use_field_dictionary, &   ! optional logical, default .false.
     rc) result(var_spec)
```

### Lookup logic

```fortran
if (var_spec%use_field_dictionary) then
   fd => get_field_dictionary()
   block
      character(:), allocatable :: lookup_key
      logical :: by_alias

      by_alias = .false.
      if (present(standard_name) .and. index(standard_name, '(') == 0) then
         lookup_key = standard_name        ! standard_name is the FD key
      else
         lookup_key = short_name           ! fall back to short_name as alias
         by_alias = .true.
      end if

      if (fd%has_item(lookup_key)) then
         dict_item = fd%get_item(lookup_key, rc)
         if (.not. present(units))     var_spec%units     = dict_item%get_units()
         if (.not. present(long_name)) var_spec%long_name = dict_item%get_long_name()
      else
         lgr%warning(...)   ! warn — never a fatal error
      end if
   end block
end if
```

Key points:

- **Opt-in**: the block is entirely skipped when `use_field_dictionary` is
  absent or `.false.` — no FD pointer is obtained, no singleton is queried.
- **Key priority**: `standard_name` (if present and not a compound vector
  name) → `short_name` as alias.
- **Compound names skipped**: any `standard_name` containing `(` is a
  two-component vector encoding and is never a valid FD key.
- **Warn-only on miss**: a missing key logs a warning but never fails.
- **Caller values win**: `units` and `long_name` already supplied by the
  caller are never overwritten by the dictionary.
- **Singleton access**: `get_field_dictionary()` always returns a pointer to
  the module-level singleton `the_field_dictionary`.  If the dictionary was
  never loaded, the singleton is an empty `FieldDictionary` and `has_item`
  always returns `.false.`.

### Regrid method

The `conserved` → regrid method mapping is resolved separately in
`get_regrid_param` → `get_regrid_method_from_field_dict_`.  These functions
now accept a `use_field_dictionary` logical argument; `get_regrid_param`
skips the FD lookup unless `use_field_dictionary=.true.` is passed:

```fortran
function get_regrid_param(requested_param, standard_name, use_field_dictionary) &
     result(regrid_param)
   ...
   use_fd = .false.
   if (present(use_field_dictionary)) use_fd = use_field_dictionary
   if (.not. use_fd) return     ! skip FD lookup entirely

   regrid_method = get_regrid_method_from_field_dict_(standard_name, rc=status)
   if (status==ESMF_SUCCESS) then
      regrid_param = EsmfRegridderParam(regridmethod=regrid_method)
   end if
end function
```

---

## Adding New Fields to the Dictionary

1. Edit `GEOS_FieldDictionary/geos_field_dictionary.yaml`.
2. Add an entry following the v0.2.0 schema (see user guide for schema details).
3. Set `verification_status: unverified` initially; mark `verified` after
   domain-scientist review; mark `cf_compliant` only for fields with exact
   CF standard names.
4. Run `python GEOS_FieldDictionary/utils/validate_schema.py
   GEOS_FieldDictionary/geos_field_dictionary.yaml` to validate.
5. Open a PR to `GEOS-ESM/GEOS_FieldDictionary`.

---

## Writing Tests with the Dictionary

Unit tests should use the test dictionary
`generic3g/tests/field_dictionary_test.yaml`, which contains fictitious
standard names to avoid coupling tests to the production dictionary:

```fortran
! In a pFUnit test:
use mapl3g_FieldDictionary
use mapl3g_VariableSpec

type(VariableSpec) :: vs
integer :: status

call load_field_dictionary('field_dictionary_test.yaml', rc=status)
@assertEqual(0, status)

! Must pass use_field_dictionary=.true. to trigger FD lookup
vs = make_VariableSpec(ESMF_STATEINTENT_EXPORT, short_name='T', &
     standard_name='test_temperature', &
     use_field_dictionary=.true., rc=status)
@assertEqual(0, status)
@assertEqual('K', vs%units)

! Without the flag — dictionary is not consulted
vs = make_VariableSpec(ESMF_STATEINTENT_EXPORT, short_name='T', &
     standard_name='test_temperature', rc=status)
@assertEqual(0, status)
@assertFalse(allocated(vs%units))   ! units not filled from dict
```

See `generic3g/tests/Test_FieldDictionary.pf` and
`generic3g/tests/Test_FieldDictIntegration.pf` for full examples.

**Important**: do **not** call `load_field_dictionary` in a `@before`
subroutine in test modules that do not actually use `use_field_dictionary=.true.`.
The FD singleton is not needed for tests that never opt in, and loading it
unnecessarily couples those tests to the test YAML file.

---

## Design Notes

### Typed enum pattern

`VerificationStatus` and `ValidationMode` follow the pattern established by
`esmf_utils/VectorBasisKind.F90`: a private `integer :: id` member, `parameter`
instances, `operator(==)` / `operator(/=)` overloads, and a string constructor
for YAML parsing.  This avoids magic integers scattered throughout the code
while remaining comparable with `==`.

### `implicit none(type, external)` and NAG

Under NAG's strict `implicit none(type, external)`, a local variable cannot
share a name with a derived type in scope.  When adding new local variables to
any module in this subsystem, ensure their names do not collide with the type
names (`FieldDictionary`, `FieldDictionaryItem`, `VerificationStatus`, etc.).

### gFTL `StringStringMap` insert/erase

`gFTL StringStringMap::insert` does **not** overwrite an existing key.  To
replace a value: call `map%erase(key)` first, then `map%insert(key, value)`.

### `map%at()` on missing key (NAG)

Under NAG, `map%at(key)` on a missing key returns a disassociated pointer and
does not raise an error at the Fortran level — it causes a runtime crash later.
Always call `map%count(key) > 0` (or `dict%has_item(key)`) before `%at()`.

---

## Related Files

```
generic3g/VerificationStatus.F90
generic3g/ValidationMode.F90
generic3g/FieldDictionaryItem.F90
generic3g/FieldDictionaryItemMap.F90
generic3g/FieldDictionary.F90
generic3g/FieldDictionaryConfig.F90
generic3g/ComponentSpecParser/parse_var_specs.F90
generic3g/specs/VariableSpec.F90
generic3g/tests/field_dictionary_test.yaml
generic3g/tests/Test_FieldDictionary.pf
generic3g/tests/Test_FieldDictIntegration.pf
GEOS_FieldDictionary/geos_field_dictionary.yaml
GEOS_FieldDictionary/utils/validate_schema.py
GEOS_FieldDictionary/docs/schema-rfc-v0.2.0.md
```

---

*Last updated: March 2026*
*MAPL Version: 3.0*
