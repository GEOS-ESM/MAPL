! Export umbrella for the MAPL utils layer.
! Defines the public API of utils/ for external consumers.
! Uses mapl_utils_internal and re-exports only the intended public symbols.
module mapl_utils_export

   use mapl_utils_internal

   implicit none
   private

   ! Error handling
   public :: MAPL_Assert, MAPL_Verify, MAPL_Return, MAPL_Deprecated
   public :: MAPL_SetFailOnDeprecated, MAPL_abort, MAPL_set_abort_handler
   public :: MAPL_RTRN, MAPL_Vrfy, MAPL_ASRT
   public :: MAPL_SUCCESS, MAPL_UNKNOWN_ERROR
   public :: MAPL_NO_SUCH_PROPERTY, MAPL_NO_SUCH_VARIABLE
   public :: MAPL_TYPE_MISMATCH, MAPL_UNSUPPORTED_TYPE
   public :: MAPL_VALUE_NOT_SUPPORTED, MAPL_NO_DEFAULT_VALUE
   public :: MAPL_DUPLICATE_KEY, MAPL_STRING_TOO_SHORT
   public :: MAPL_throw_exception, MAPL_set_throw_method

   ! Keyword enforcer
   public :: KeywordEnforcer

   ! String types and utilities
   public :: String
   public :: split, to_lower, to_upper, capitalize
   public :: lowercase, uppercase
   public :: is_alpha, is_alpha_only, is_numeric, is_alphanumeric
   public :: to_string, to_character_array
   public :: StringDictionary

   ! OS / filesystem
   public :: mapl_GetCurrentWorkingDirectory, mapl_ChangeDirectory
   public :: mapl_MakeDirectory, mapl_DirectoryExists
   public :: mapl_RemoveDirectory, mapl_RemoveFile
   public :: mapl_PushDirectory, mapl_PopDirectory
   public :: mapl_ClearDirectoryStack, mapl_PathJoin
   public :: mapl_MakeSymbolicLink
   public :: get_file_extension, get_file_basename
   public :: get_checkpoint_subdir

   ! Numeric utilities
   public :: MAPL_HashCreate, MAPL_HashIncrement, MAPL_HashDestroy
   public :: MAPL_HashSize, MAPL_HashDump
   public :: IntegerMinMax, RealMinMax, Real64MinMax
   public :: MAPL_Range
   public :: MAPL_Sort
   public :: MAPL_Interp

   ! Time utilities
   ! public :: PackDate, PackDateTime, UnpackDate
   ! UnpackDateTime removed - conflicts with mp_utils version, see issue #5011
   ! public :: UnpackDateTime
   public :: ISO8601Date, ISO8601Time, ISO8601DateTime, ISO8601Duration, ISO8601Interval
   public :: convert_ISO8601_to_integer_time, convert_ISO8601_to_integer_date
   public :: MAPL_Sleep
   public :: CF_Time, CF_Time_Integer, CF_Time_Real
   public :: extract_ISO8601_from_CF_Time, extract_CF_Time_duration
   public :: extract_CF_Time_unit, convert_CF_Time_to_datetime_duration

   ! ESMF info keys
   ! KEY_UNITS and KEY_TYPEKIND excluded: values differ from mapl_HistoryConstants_mod
   ! homonyms; consumers needing them should use mapl_esmf_info_keys_mod directly.
   public :: INFO_SHARED_NAMESPACE, INFO_PRIVATE_NAMESPACE, INFO_INTERNAL_NAMESPACE
   public :: KEY_UNGRIDDED_DIMS, KEY_VERT_DIM, KEY_VERT_GRID
   public :: KEY_INTERPOLATION_WEIGHTS, KEY_FIELD_PROTOTYPE
   public :: KEY_FIELDBUNDLETYPE, KEY_LONG_NAME
   public :: KEY_STANDARD_NAME, KEY_NUM_LEVELS
   public :: KEY_VLOC, KEY_NUM_UNGRIDDED_DIMS, KEYSTUB_DIM
   public :: KEY_UNGRIDDED_NAME, KEY_UNGRIDDED_UNITS, KEY_UNGRIDDED_COORD
   public :: KEY_DIM_STRINGS, KEY_VERT_STAGGERLOC
   public :: KEY_BRACKET_UPDATED, KEY_VECTOR_BASIS_KIND
   public :: make_dim_key

   ! Memory info
   public :: MemInfo, MemInfoWrite

   ! Validation
   public :: is_valid_name

end module mapl_utils_export
