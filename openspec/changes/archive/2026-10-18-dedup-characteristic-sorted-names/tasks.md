## 1. Extract shared helper

- [x] 1.1 Decide exact home for the shared helper: a new
      `superstructure/generic/graph/StringSortUtilities.F90` (or an
      existing general-utility module under
      `superstructure/generic/graph/` if one is found at implementation
      time to already fit) — verify no existing module already provides
      an equivalent sorted-`StringVector`-to-`character` helper before
      creating a new file.
- [x] 1.2 Add `function sorted_names(names) result(sorted)` to that
      module, as a verbatim copy of the existing logic in
      `AttributesCharacteristic.F90`/`VerticalCoordinateCharacteristic.F90`
      (signature: `type(StringVector), intent(in) :: names` ->
      `character(:), allocatable :: sorted(:)`), `public`.
- [x] 1.3 If a new file was created in 1.1, register it in the owning
      `superstructure/generic/graph/CMakeLists.txt` (same pattern as the
      other flat-level files in that directory).

## 2. Update call sites

- [x] 2.1 In `AttributesCharacteristic.F90`: add a `use` of the new
      shared module's `sorted_names`, delete the file's own private
      `sorted_names` function, and confirm `attributes_get_signature`
      still compiles unchanged.
- [x] 2.2 In `VerticalCoordinateCharacteristic.F90`: add the same `use`,
      delete the file's own private `sorted_names` function, and confirm
      both call sites (signature construction and the overlap-list
      construction) still compile unchanged.

## 3. Verify

- [x] 3.1 Build (NAG or gfortran per the active toolchain) and confirm no
      new warnings/errors from either changed file or the new module.
- [x] 3.2 Run
      `Test_StateItemCharacteristicSubclasses.pf`/`Test_StateItemCharacteristic.pf`
      (the coverage exercising
      `AttributesCharacteristic%get_signature`/
      `VerticalCoordinateCharacteristic%get_signature`) and confirm 0 new
      failures, with identical signature-string output to before the
      change.
- [x] 3.3 Run `ctest -L ESSENTIAL` for the full-suite regression check
      and confirm 0 new failures.
