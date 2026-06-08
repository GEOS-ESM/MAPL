! Export umbrella for the MAPL utils layer.
! Defines the public API of utils/ for external consumers.
! Directly imports from leaf modules and re-exports only the intended public symbols.
module mapl_utils_api

   ! Error handling
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

   ! Memory info
   use mapl_MemInfo_mod, only: mapl_MemInfo => MemInfo
   use mapl_MemInfo_mod, only: mapl_MemInfoWrite => MemInfoWrite

   implicit none
   private

   ! Error handling
   public :: MAPL_Assert
   public :: MAPL_Verify
   public :: MAPL_Return
   public :: MAPL_Deprecated
   public :: MAPL_SetFailOnDeprecated
   public :: MAPL_abort
   public :: MAPL_set_abort_handler
   public :: MAPL_SUCCESS
   public :: MAPL_UNKNOWN_ERROR
   public :: MAPL_NO_SUCH_PROPERTY
   public :: MAPL_NO_SUCH_VARIABLE
   public :: MAPL_TYPE_MISMATCH
   public :: MAPL_UNSUPPORTED_TYPE
   public :: MAPL_VALUE_NOT_SUPPORTED
   public :: MAPL_NO_DEFAULT_VALUE
   public :: MAPL_DUPLICATE_KEY
   public :: MAPL_STRING_TOO_SHORT

   ! Keyword enforcer
   public :: KeywordEnforcer

   ! OS / filesystem
   public :: mapl_GetCurrentWorkingDirectory
   public :: mapl_ChangeDirectory
   public :: mapl_MakeDirectory
   public :: mapl_DirectoryExists
   public :: mapl_RemoveDirectory
   public :: mapl_RemoveFile
   public :: mapl_PushDirectory
   public :: mapl_PopDirectory
   public :: mapl_ClearDirectoryStack
   public :: mapl_PathJoin
   public :: mapl_MakeSymbolicLink
   public :: get_file_extension
   public :: get_file_basename

   ! Memory info
   public :: mapl_MemInfo
   public :: mapl_MemInfoWrite

   ! Time utilities
   public :: PackDate
   public :: PackDateTime
   public :: UnpackDate

   ! ISO8601 date/time conversion
   public :: convert_ISO8601_to_integer_time
   public :: convert_ISO8601_to_integer_date
   public :: ISO8601Date
   public :: ISO8601Time
   public :: ISO8601DateTime
   public :: ISO8601Duration
   public :: ISO8601Interval

   ! ESMF info keys
   ! KEY_UNITS and KEY_TYPEKIND excluded: values differ from mapl_HistoryConstants_mod
   ! homonyms; consumers needing them should use mapl_esmf_info_keys_mod directly.

   ! Validation
   public :: is_valid_name

end module mapl_utils_api
