! Internal umbrella for the MAPL utils layer.
! Aggregates all leaf modules compiled into MAPL.utils for use by
! other MAPL subdirectories. Constants/, udunits2f/, and regex/ are
! separate CMake targets and are NOT included here.
!
! Sibling consumers should: use mapl_utils_internal
! (migration tracked in GitHub issue #5005)
module mapl_utils_internal

   ! Error handling
   use mapl_ErrorHandling_mod
   use mapl_ErrorHandling_mod
   use mapl_Throw_mod

   ! Keyword enforcer (abstract sentinel type)
   use mapl_KeywordEnforcer_mod

   ! String types and utilities
   use mapl_String_mod
   use mapl_StringUtilities_mod
   use mapl_StringDictionary_mod

   ! OS / filesystem
   use mapl_os_mod
   use mapl_DirPath_mod
   use mapl_FileSystemUtilities_mod
   use mapl_DSO_Utilities_mod

   ! Numeric utilities
   use mapl_Hash_mod
   use mapl_MinMax_mod
   use mapl_Range_mod
   use mapl_Sort_mod
   use mapl_Interp_mod

   ! Time utilities
   use mapl_TimeUtilities_mod
   ! Use only the intended public API from ISO8601 to avoid conflicts
   ! with is_valid_date/is_valid_time already exported by mapl_TimeUtils_mod
   use mapl_ISO8601_DateTime_mod, only: convert_ISO8601_to_integer_time, &
        convert_ISO8601_to_integer_date, &
        ISO8601Date, ISO8601Time, ISO8601DateTime, ISO8601Duration, ISO8601Interval
   ! mapl_DateTime_Parsing_mod excluded: its is_valid_date/is_valid_time/is_digit
   ! conflict with mapl_TimeUtils_mod. Consumers needing it should use it directly.
   use mapl_DateTime_Parsing_mod
   use mapl_Sleep_mod
   use mapl_CF_Time_mod

   ! ESMF info keys — KEY_UNITS and KEY_TYPEKIND excluded: their values
   ! differ from the same-named constants in mapl_HistoryConstants_mod,
   ! causing conflicts for files that use both. Use mapl_esmf_info_keys_mod
   ! directly when the /units and /typekind ESMF-path variants are needed.
   use mapl_esmf_info_keys_mod, only: &
        INFO_SHARED_NAMESPACE, INFO_PRIVATE_NAMESPACE, INFO_INTERNAL_NAMESPACE, &
        KEY_UNGRIDDED_DIMS, KEY_VERT_DIM, KEY_VERT_GRID, &
        KEY_INTERPOLATION_WEIGHTS, KEY_FIELD_PROTOTYPE, &
        KEY_FIELDBUNDLETYPE, KEY_LONG_NAME, &
        KEY_STANDARD_NAME, KEY_NUM_LEVELS, &
        KEY_VLOC, KEY_NUM_UNGRIDDED_DIMS, KEYSTUB_DIM, &
        KEY_UNGRIDDED_NAME, KEY_UNGRIDDED_UNITS, KEY_UNGRIDDED_COORD, &
        KEY_DIM_STRINGS, KEY_VERT_STAGGERLOC, &
        KEY_BRACKET_UPDATED, KEY_VECTOR_BASIS_KIND, &
        make_dim_key


   ! Validation
   use mapl_Validation_mod

   implicit none

end module mapl_utils_internal
