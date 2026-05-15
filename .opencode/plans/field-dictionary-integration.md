# Field Dictionary Integration into MAPL3

**Date:** 2026-02-22  
**Author:** MAPL Team  
**Status:** Planning  
**Related Issues:** 
- MAPL Epic: [#4438](https://github.com/GEOS-ESM/MAPL/issues/4438)
- MAPL Phases: [#4439](https://github.com/GEOS-ESM/MAPL/issues/4439), [#4440](https://github.com/GEOS-ESM/MAPL/issues/4440), [#4441](https://github.com/GEOS-ESM/MAPL/issues/4441), [#4442](https://github.com/GEOS-ESM/MAPL/issues/4442), [#4443](https://github.com/GEOS-ESM/MAPL/issues/4443), [#4444](https://github.com/GEOS-ESM/MAPL/issues/4444)
- GEOS_FieldDictionary: [#1](https://github.com/GEOS-ESM/GEOS_FieldDictionary/issues/1), [#2](https://github.com/GEOS-ESM/GEOS_FieldDictionary/issues/2), [#3](https://github.com/GEOS-ESM/GEOS_FieldDictionary/issues/3)

---

## Executive Summary

Integrate a comprehensive Field Dictionary into MAPL3 that uses CF-compliant standard names to provide canonical metadata (units, long_name, physical dimensions, etc.) for fields. This reduces duplication in component specs, ensures consistency across GEOS, and enables gradual improvement toward CF compliance.

**Key Benefits:**
- Users specify only `standard_name`, get canonical metadata automatically
- Track verification status and provenance for each field
- Support for ~1,500 existing GEOS fields
- Backward compatible with existing component specs
- Configurable validation (permissive → strict migration path)

---

## Background

ESMF/NUOPC has a field dictionary concept using "standard name" as a key. MAPL currently has a minimal placeholder implementation. We need to extend this to:

1. Support GEOS-specific fields (not just CF-compliant)
2. Track verification status and provenance
3. Handle vector fields (u,v components)
4. Support mixing ratios and regridding metadata
5. Allow user overrides while providing sensible defaults
6. Exempt exotic types (ServiceService) from validation

**Current State:**
- Existing `FieldDictionary` and `FieldDictionaryItem` types in `generic3g/`
- GEOS_FieldDictionary repository with ~1,500 entries (basic schema)
- Test scenarios with fictitious field names
- Commented-out NUOPC integration hooks

---

## Design Decisions

### 1. **Verification Status**
Three-level enum: `unverified` → `verified` → `cf_compliant`

### 2. **Provenance Tracking**
Nested structure with `verified_by` (scientist name), extensible for future metadata (date, notes)

### 3. **Vector Fields**
Individual component lookup (eastward_wind, northward_wind stored separately)

### 4. **Mixing Ratios**
Follow CF standard_name conventions (encoded in name: `mass_fraction_of_X_in_Y`)

### 5. **Test Strategy**
Separate test dictionary (`field_dictionary_test.yaml`) for existing test scenarios

### 6. **Validation Modes**
Context-aware with runtime configuration:
- **PERMISSIVE:** Warnings only (default for gradual adoption)
- **STRICT:** Errors for missing dictionary entries

### 7. **Dictionary Location**
- Separate git repository: `GEOS-ESM/GEOS_FieldDictionary`
- Installed to `${CMAKE_INSTALL_PREFIX}/etc/MAPL/`
- Configured in cap.yaml, defaults to CWD

### 8. **Physical Dimensions**
High-level categories (temperature, pressure, velocity, mass_fraction, etc.)

### 9. **Exempt Field Types**
Based on `itemType` in VariableSpec:
- **Require dictionary:** Field, Vector, Bracket, VectorBracket
- **Exempt:** Service, ServiceProvider, ServiceSubscriber, FieldBundle, State

### 10. **Fortran Enum Pattern**
Follow MAPL pattern (see `VectorBasisKind.F90`): typed enum with operator overloading

---

## Extended Schema Design

### YAML Schema (User-Facing)

```yaml
field_dictionary:
  version: 0.2.0
  metadata:
    last_modified: 2026-02-22T10:00:00Z
    institution: NASA GMAO
    contact: mapl-support@example.com
    description: GEOS field dictionary
  
  entries:
    air_temperature:
      long_name: Air Temperature
      canonical_units: K
      physical_dimension: temperature
      conserved: false                    # Drives default regrid method
      verification_status: cf_compliant   # unverified | verified | cf_compliant
      provenance:
        verified_by: Jane Doe             # Extensible structure
      aliases: [T, TEMP]                  # Human reference only
      
      # Legacy fields (preserve from existing dict)
      components: [MOIST, PHYSICS]        # Which GEOS components use this
      incomplete: false                   # Metadata completeness flag
```

### Migration from Current Schema

**Current (1,500 existing entries):**
```yaml
field_name:
  components: [list]
  incomplete: true
  short_names: [aliases]
  units: str
```

**Migration Strategy:**
- Rename `short_names` → `aliases`
- Rename `units` → `canonical_units`
- Add `long_name` (auto-generate from standard_name if missing)
- Add `verification_status: unverified` (default)
- Add `conserved: false` (default, override for known conserved quantities)
- Add `physical_dimension` (infer from units)
- Keep `components` and `incomplete` for backward compatibility

---

## Configuration in cap.yaml

```yaml
#####################################
# Global services
esmf:
  logKindFlag: ESMF_LOGKIND_MULTI_ON_ERROR

mapl:
  model_petcount: 1
  pflogger: pflogger.yaml
  
  # Field dictionary configuration
  field_dictionary:
    path: field_dictionary.yaml        # Relative to CWD (default)
    validation_mode: permissive        # permissive | strict
    warn_on_unverified: true          # Warn when using unverified entries
    warn_on_non_cf: false             # Warn when using non-CF entries
  
  servers:
    # ... existing
```

---

## Component YAML Behavior

```yaml
# Example 1: Dictionary lookup with override
mapl:
  states:
    export:
      surface_temp:
        standard_name: air_temperature
        # units, long_name auto-populated from dictionary
        units: degF                     # User can override!
        
# Example 2: Full explicit specification (backward compatible)
      pressure:
        standard_name: air_pressure
        units: hPa
        long_name: Atmospheric Pressure
        
# Example 3: Service (exempt from validation)
      ServiceService_Advection:
        standard_name: advection_service  # No dictionary entry needed
```

**Validation Logic:**
1. Check `itemType` → skip if exempt (Service, etc.)
2. Look up `standard_name` in dictionary
3. Use dictionary values as **defaults**
4. **Apply YAML overrides** (user can override anything)
5. Emit warnings/errors based on validation_mode

---

## Fortran Implementation

### VerificationStatus Type (Following MAPL Pattern)

```fortran
module mapl3g_VerificationStatus
   implicit none(type, external)
   private
   
   public :: VerificationStatus
   public :: operator(==), operator(/=)
   public :: VERIFICATION_STATUS_UNVERIFIED
   public :: VERIFICATION_STATUS_VERIFIED
   public :: VERIFICATION_STATUS_CF_COMPLIANT
   
   type :: VerificationStatus
      private
      integer :: id = -1
   contains
      procedure :: to_string
   end type VerificationStatus
   
   type(VerificationStatus), parameter :: &
      VERIFICATION_STATUS_UNVERIFIED = VerificationStatus(0), &
      VERIFICATION_STATUS_VERIFIED = VerificationStatus(1), &
      VERIFICATION_STATUS_CF_COMPLIANT = VerificationStatus(2)
   
   interface VerificationStatus
      procedure new_from_string
   end interface
   
   ! ... operator overloading, to_string, etc.
end module
```

### ValidationMode Type

```fortran
module mapl3g_ValidationMode
   type :: ValidationMode
      integer :: id
   end type
   
   type(ValidationMode), parameter :: &
      VALIDATION_MODE_PERMISSIVE = ValidationMode(0), &
      VALIDATION_MODE_STRICT = ValidationMode(1)
end module
```

### Provenance Type

```fortran
type :: Provenance
   character(:), allocatable :: verified_by
   ! Future extensions: verification_date, notes, etc.
end type
```

### FieldDictionaryItem Type

```fortran
type :: FieldDictionaryItem
   private
   character(:), allocatable :: long_name
   character(:), allocatable :: canonical_units
   character(:), allocatable :: physical_dimension
   logical :: conserved
   type(VerificationStatus) :: verification_status
   type(Provenance) :: provenance
   type(StringVector) :: aliases  ! Human reference only
contains
   procedure :: get_long_name
   procedure :: get_units
   procedure :: get_physical_dimension
   procedure :: get_verification_status
   procedure :: get_provenance
   procedure :: is_conserved
   procedure :: get_default_regrid_method  ! Uses conserved flag
   procedure :: get_aliases
end type
```

### FieldDictionaryConfig Type

```fortran
type :: FieldDictionaryConfig
   character(:), allocatable :: dictionary_path
   type(ValidationMode) :: validation_mode
   logical :: warn_on_unverified
   logical :: warn_on_non_cf
contains
   procedure :: is_exempt  ! Check itemType
end type
```

---

## Implementation Phases

### **Phase 0: Schema Refinement & Team Review** (1 week)

**Objective:** Get team consensus on extended schema

**Tasks:**
1. Create Schema RFC document
2. Present options for team review:
   - Field naming: `units` vs `canonical_units`, `short_names` vs `aliases`
   - Keep `incomplete` flag or replace with `verification_status`?
   - Physical dimension vocabulary
   - Migration strategy for 1,500 entries
3. Hold design review meeting
4. Iterate based on feedback
5. Document final decisions

**Deliverables:**
- [ ] Schema RFC document
- [ ] Team consensus on final schema
- [ ] Migration strategy documented
- [ ] Design decisions recorded

**Gate:** No implementation until schema approved

---

### **Phase 1: Core Infrastructure** (2 weeks)

**Objective:** Extend Fortran types and YAML parser

**MAPL Repository Tasks:**

1. **Create new modules:**
   - `mapl3g_VerificationStatus.F90` (typed enum following MAPL pattern)
   - `mapl3g_ValidationMode.F90` (typed enum)
   - `mapl3g_PhysicalDimension.F90` (optional: recommended vocabulary)

2. **Extend `FieldDictionaryItem.F90`:**
   - Add `Provenance` type
   - Add fields: `physical_dimension`, `conserved`, `verification_status`, `provenance`
   - Add getters for all new fields
   - Implement `get_default_regrid_method()` using `conserved` flag

3. **Update `FieldDictionary.F90`:**
   - Update YAML parser for new schema
   - All new fields optional with defaults
   - Handle both old and new schema formats
   - Remove `alias_map` (aliases are documentation only)

4. **Create `FieldDictionaryConfig.F90`:**
   - Singleton pattern for global dictionary
   - Parse `mapl/field_dictionary/*` from cap.yaml
   - Defaults: `path=field_dictionary.yaml`, `validation_mode=PERMISSIVE`
   - Implement `is_exempt(itemType)` checking

5. **Create test dictionary:**
   - `generic3g/tests/field_dictionary_test.yaml`
   - ~20 fictitious test fields
   - Mix of verification statuses and conserved flags

6. **Update test scenarios:**
   - Modify cap.yaml files to reference test dictionary

7. **Extend `Test_FieldDictionary.pf`:**
   - Test all new fields
   - Test backward compatibility
   - Test itemType exemption

**GEOS_FieldDictionary Repository Tasks:**

1. Update CHANGELOG.md (document v0.2.0 schema)
2. Update README.md (schema documentation)
3. Create `utils/validate_schema.py` (CI validation)

**Deliverables:**
- [ ] New Fortran modules with typed enums
- [ ] Extended FieldDictionaryItem
- [ ] Updated FieldDictionary parser
- [ ] FieldDictionaryConfig with cap.yaml parsing
- [ ] Test dictionary
- [ ] >90% test coverage
- [ ] GEOS_FieldDictionary schema docs

**Files Modified:**
- `generic3g/FieldDictionary.F90`
- `generic3g/FieldDictionaryItem.F90`
- `generic3g/tests/Test_FieldDictionary.pf`
- `generic3g/tests/field_dictionary_test.yaml` (new)

**Files Created:**
- `generic3g/VerificationStatus.F90`
- `generic3g/ValidationMode.F90`
- `generic3g/FieldDictionaryConfig.F90`

---

### **Phase 2: Integration & Validation** (2 weeks)

**Objective:** Integrate dictionary with VariableSpec and implement validation

**MAPL Repository Tasks:**

1. **Modify `VariableSpec.F90`:**
   - Initialize global dictionary during MAPL init
   - When processing component YAML:
     - Check `itemType` → skip if exempt
     - Look up `standard_name` in dictionary
     - Use dictionary as **defaults**
     - **Apply YAML overrides**
   - Use `conserved` flag for default regrid method

2. **Implement validation logic:**
   - Missing dictionary entry → warning (PERMISSIVE) or error (STRICT)
   - Using unverified entry → warning if configured
   - Using non-CF entry → warning if configured

3. **Update `get_regrid_method_from_field_dict_()`:**
   - Use global dictionary singleton
   - Check `conserved` flag

4. **Integration tests:**
   - Dictionary lookup with overrides
   - Exempt itemTypes
   - Validation modes
   - Conserved flag → regrid method
   - Warning emissions

5. **Documentation:**
   - User guide for dictionary in component specs
   - Override mechanisms
   - Validation messages

**Deliverables:**
- [ ] VariableSpec integrated with dictionary
- [ ] Validation working correctly
- [ ] Integration tests passing
- [ ] User documentation

**Files Modified:**
- `generic3g/specs/VariableSpec.F90`

---

### **Phase 3: Build & Installation** (1 week)

**Objective:** Integrate GEOS_FieldDictionary repo and install dictionary

**MAPL Repository Tasks:**

1. **Add to `components.yaml`:**
   ```yaml
   GEOS_FieldDictionary:
     local: ./GEOS_FieldDictionary
     remote: ../GEOS_FieldDictionary.git
     develop: main
   ```

2. **Update `CMakeLists.txt`:**
   - Install `geos_field_dictionary.yaml` to `${CMAKE_INSTALL_PREFIX}/etc/MAPL/`
   - Handle renaming if needed

3. **Update build docs:**
   - Mepo workflow
   - Dictionary location
   - Customization

4. **Test installation:**
   - Verify dictionary accessible
   - Test with GEOS config

**Deliverables:**
- [ ] components.yaml updated
- [ ] CMake installs dictionary
- [ ] Build documentation updated
- [ ] Installation tested

**Files Modified:**
- `components.yaml`
- `CMakeLists.txt` (or subdirectory CMakeLists)
- Build documentation

---

### **Phase 4: Production Dictionary Migration** (2-3 weeks)

**Objective:** Migrate 1,500 existing entries to new schema

**GEOS_FieldDictionary Repository Tasks:**

1. **Create migration script:**
   - `utils/migrate_to_v0.2.0.py`
   - Convert all entries to new schema
   - Preserve existing data
   - Add defaults for new fields

2. **Automated field classification:**
   - Identify conserved quantities (mixing ratios, mass fractions)
   - Set `conserved: true` for these
   - Pattern matching on standard names and units

3. **Map CF-compliant fields:**
   - Use existing `cf_compliant_geos_long_names.txt` (43 fields)
   - Set `verification_status: cf_compliant`
   - Add provenance

4. **Infer physical dimensions:**
   - Pattern matching on units
   - Manual review of ambiguous cases
   - Use recommended vocabulary

5. **Add long_name:**
   - Auto-generate from standard_name where missing
   - Human-readable formatting

6. **Create verification workflow:**
   - GitHub issue template
   - Process documentation in CONTRIBUTING.md
   - GitHub project board for tracking

7. **Initial verification sprint:**
   - Identify top 50-100 most-used fields
   - Engage domain scientists
   - Target verified set

8. **Update CHANGELOG:**
   - Document v1.0.0 release
   - Migration notes

**Deliverables:**
- [ ] All 1,500 entries migrated
- [ ] Conserved flags set
- [ ] CF-compliant entries marked
- [ ] Physical dimensions assigned
- [ ] Long names added
- [ ] Verification workflow documented
- [ ] Initial verified entries
- [ ] CHANGELOG updated for v1.0.0

**Files Modified:**
- `geos_field_dictionary.yaml`
- `CHANGELOG.md`
- `CONTRIBUTING.md`
- `README.md`

**Files Created:**
- `utils/migrate_to_v0.2.0.py`
- `.github/ISSUE_TEMPLATE/field-verification.md`

---

### **Phase 5: ACG Enhancement** (1 week)

**Objective:** Enhance ACG to use dictionary

**MAPL Repository Tasks:**

1. **Modify `MAPL_GridCompSpecs_ACGv3.py`:**
   - Optional: load dictionary at runtime
   - When only `standard_name` in YAML:
     - Add comments showing canonical metadata
     - Show aliases for reference
   - Warn if standard_name not in dictionary

2. **Create example specs:**
   - Before/after with simplified YAML
   - Override examples
   - Vector field handling

3. **Update ACG docs:**
   - Best practices
   - Examples
   - Troubleshooting

**Deliverables:**
- [ ] ACG uses dictionary
- [ ] Example specs
- [ ] Documentation

**Files Modified:**
- `Apps/MAPL_GridCompSpecs_ACGv3.py`
- ACG documentation

---

### **Phase 6: Documentation & Training** (1 week)

**Objective:** Comprehensive documentation and training materials

**Tasks:**

1. **User Guide:**
   - Using dictionary in components
   - Override mechanisms
   - Verification status
   - Contributing verified entries

2. **Developer Reference:**
   - Schema specification
   - Adding fields
   - Validation modes
   - Physical dimension vocabulary

3. **Examples:**
   - Common scenarios
   - Edge cases
   - Troubleshooting

4. **Training Materials:**
   - Tutorial for component developers
   - Tutorial for domain scientists
   - FAQ

**Deliverables:**
- [ ] Complete user documentation
- [ ] Developer reference
- [ ] Example gallery
- [ ] Training materials
- [ ] FAQ

---

## Success Metrics

- ✅ Users specify only `standard_name`, get canonical metadata
- ✅ Users can override any metadata when needed
- ✅ Dictionary tracks verification status and provenance
- ✅ ~1,500 GEOS fields in dictionary
- ✅ Conserved flag drives default regrid method
- ✅ Vector fields work via component lookup
- ✅ Test scenarios run with test dictionary
- ✅ Validation modes enable gradual compliance
- ✅ CF-compliant entries (43+) clearly marked
- ✅ ServiceService and exotic types exempt
- ✅ Physical dimensions available for regridding

---

## Risks & Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| Breaking existing configs | High | Backward compatibility testing, gradual rollout |
| Scientists resist verification | Medium | Start permissive, demonstrate value |
| Dictionary becomes stale | Medium | Automated discovery, CI checks |
| Mepo integration issues | Low | Test early, coordinate with team |
| Performance overhead | Low | Singleton pattern, cache lookups |
| Schema debate delays work | Medium | Phase 0 dedicated to consensus |

---

## Open Questions (To Be Resolved in Phase 0)

1. Schema naming: `units` vs `canonical_units`, `short_names` vs `aliases`?
2. Keep `incomplete` flag or replace with `verification_status`?
3. Keep `components` field (useful for provenance)?
4. Physical dimension vocabulary: controlled vs free-form?
5. Default regrid method: per-field or category-based?

---

## References

- CF Conventions: https://cfconventions.org
- NUOPC Field Dictionary: https://earthsystemmodeling.org
- GEOS_FieldDictionary: https://github.com/GEOS-ESM/GEOS_FieldDictionary
- MAPL VectorBasisKind pattern: `./esmf_utils/VectorBasisKind.F90`
- Existing test dictionary: `./generic3g/tests/scenarios/FieldDictionary.yml`

---

## Appendix: Physical Dimension Vocabulary

**Recommended categories (extensible):**
- `temperature`, `pressure`, `density`
- `velocity`, `acceleration`
- `mass`, `mass_flux`, `mass_fraction`, `mole_fraction`
- `energy`, `energy_flux`, `power`
- `length`, `area`, `volume`
- `concentration`, `mixing_ratio`
- `frequency`, `angular_velocity`
- `dimensionless`, `other`

---

## Appendix: ItemType Validation Matrix

| ItemType | Dictionary Required | Notes |
|----------|---------------------|-------|
| FIELD | ✅ Yes | Standard fields |
| VECTOR | ✅ Yes | Components looked up individually |
| BRACKET | ✅ Yes | Bracket notation fields |
| VECTORBRACKET | ✅ Yes | Vector bracket fields |
| SERVICE | ❌ No | ServiceService, etc. |
| SERVICE_PROVIDER | ❌ No | Service infrastructure |
| SERVICE_SUBSCRIBER | ❌ No | Service infrastructure |
| FIELDBUNDLE | ⚠️ Future | May enforce later |
| STATE | ⚠️ Future | May enforce later |
| WILDCARD | ❌ No | Special pattern matching |
| EXPRESSION | ⚠️ TBD | Computed fields |

---

**End of Plan**
