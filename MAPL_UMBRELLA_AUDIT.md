# MAPL Umbrella Module Export Audit

**Issue**: #4999 (Part of #4975, #4969)  
**Date**: 2026-05-26

## Summary

The MAPL umbrella module (`mapl/MAPL.F90`) currently uses default public visibility and imports many sub-modules. This analysis identifies all unprefixed entities that are exported through the umbrella, violating MAPL3 naming conventions.

## Unprefixed Entities Currently Exported

### 1. mapl_String_mod (utils/String.F90)
- `String` (type) - **USED** (409 occurrences in ported repos)

### 2. mapl_StringUtilities_mod (utils/StringUtilities.F90)
- `split` - **USED** (26 occurrences)
- `lowercase` - **USED** (32 occurrences)
- `uppercase` - **USED** (66 occurrences)
- `to_lower` - NOT USED
- `to_upper` - NOT USED
- `capitalize` - NOT USED
- `is_alpha` - NOT USED
- `is_alpha_only` - NOT USED
- `is_numeric` - NOT USED
- `is_alphanumeric` - NOT USED
- `to_string` - NOT USED
- `to_character_array` - NOT USED
- `is_digit` - NOT USED
- `get_ascii_interval` - NOT USED
- `is_alphanum_character` - NOT USED
- `is_lower_character` - NOT USED
- `is_upper_character` - NOT USED

### 3. mapl_FileSystemUtilities_mod (utils/FileSystemUtilities.F90)
- `get_file_extension` - NOT USED
- `get_file_basename` - NOT USED

### 4. mapl_DSO_Utilities_mod (utils/DSO_Utilities.F90)
- `is_valid_dso_name` - NOT USED
- `is_valid_dso_extension` - NOT USED
- `is_supported_dso_name` - NOT USED
- `is_supported_dso_extension` - NOT USED
- `adjust_dso_name` - NOT USED
- `SYSTEM_DSO_EXTENSION` - NOT USED

### 5. mapl_DirPath_mod (utils/MAPL_DirPath.F90)
- `DirPath` (type) - NOT USED
- `dirpaths` (module variable) - NOT USED

### 6. mapl_os_mod (utils/OS.F90)
✅ All entities already have `mapl_` prefix (mapl_ChangeDirectory, etc.)

## Recommended Actions

### Option A: Use `only:` Clauses (Recommended)
Modify MAPL.F90 to use explicit `only:` clauses with renaming:
```fortran
use mapl_String_mod, only: mapl_String => String
use mapl_StringUtilities_mod, only: mapl_split => split, &
                                     mapl_lowercase => lowercase, &
                                     mapl_uppercase => uppercase
! Don't export unused utilities
```

**Pros**: 
- Explicit control over exports
- Clear documentation of public API
- Can add prefixes via renaming
- Removes unused entities from public API

**Cons**:
- More verbose
- Requires updating MAPL.F90 for each change

### Option B: Add Renaming in Source Modules
Keep default public in MAPL.F90, but modify source modules to export with prefixes:
```fortran
module mapl_String_mod
   public :: mapl_String
   type :: mapl_String
      ! ...
   end type mapl_String
end module
```

**Pros**:
- Source of truth in module itself
- MAPL.F90 stays simple

**Cons**:
- Requires changing many source files
- May break internal MAPL code using unprefixed names

### Option C: Make Unused Entities Private
For unused entities, make them private in source modules. For used entities, use Option A or B.

**Pros**:
- Reduces public API surface
- Best practice

**Cons**:
- Requires changing source modules
- Need to ensure nothing internal breaks

## Usage Summary (Ported Repos Only)

**USED entities** (must handle carefully):
- `String` (409)
- `split` (26)
- `lowercase` (32)
- `uppercase` (66)

**UNUSED entities** (candidates for removal from public API):
- All other StringUtilities functions
- All FileSystemUtilities functions
- All DSO_Utilities functions
- DirPath type and dirpaths variable

## Recommendation

**Phase 1 (This PR)**:
1. Add `only:` clauses to MAPL.F90 for the 6 modules above
2. For USED entities: add renaming to include `mapl_` prefix
3. For UNUSED entities: don't export them at all

**Phase 2 (Future)**:
1. Update source modules to make unused entities private
2. Consider refactoring source modules to use prefixed names internally

## Breaking Changes

This will be a **BREAKING API CHANGE** for client code using:
- `String` → must use `mapl_String`
- `split` → must use `mapl_split`
- `lowercase` → must use `mapl_lowercase`
- `uppercase` → must use `mapl_uppercase`

All other removed exports are unused in ported repos, so should have minimal impact.
