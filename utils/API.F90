! Export umbrella for the MAPL utils layer.
! Defines the public API of utils/ for external consumers.
! Directly imports from leaf modules and re-exports only the intended public symbols.
module mapl_utils_api

   ! Error handling
   use mapl_ErrorHandling_mod

   ! Keyword enforcer (abstract sentinel type)
   use mapl_KeywordEnforcer_mod, only: mapl_KeywordEnforcer => KeywordEnforcer

   ! OS / filesystem
   use mapl_os_mod, only: &
      & mapl_GetCurrentWorkingDirectory => GetCurrentWorkingDirectory, &
      & mapl_ChangeDirectory => ChangeDirectory, &
      & mapl_MakeDirectory => MakeDirectory, &
      & mapl_DirectoryExists => DirectoryExists, &
      & mapl_RemoveDirectory => RemoveDirectory, &
      & mapl_RemoveFile => RemoveFile, &
      & mapl_PushDirectory => PushDirectory, &
      & mapl_PopDirectory => PopDirectory, &
      & mapl_ClearDirectoryStack => ClearDirectoryStack, &
      & mapl_PathJoin => PathJoin, &
      & mapl_MakeSymbolicLink => MakeSymbolicLink
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
   use mapl_TimeUtilities_mod, only: mapl_PackDate => PackDate
   use mapl_TimeUtilities_mod, only: mapl_PackDateTime => PackDateTime
   use mapl_TimeUtilities_mod, only: mapl_UnpackDate => UnpackDate

   ! Use only the intended public API from ISO8601 to avoid conflicts
   ! with is_valid_date/is_valid_time already exported by mapl_TimeUtils_mod
   use mapl_ISO8601_DateTime_mod, only: mapl_convert_ISO8601_to_integer_time => convert_ISO8601_to_integer_time
   use mapl_ISO8601_DateTime_mod, only: mapl_convert_ISO8601_to_integer_date => convert_ISO8601_to_integer_date
   use mapl_ISO8601_DateTime_mod, only: mapl_ISO8601Date => ISO8601Date
   use mapl_ISO8601_DateTime_mod, only: mapl_ISO8601Time => ISO8601Time
   use mapl_ISO8601_DateTime_mod, only: mapl_ISO8601DateTime => ISO8601DateTime
   use mapl_ISO8601_DateTime_mod, only: mapl_ISO8601Duration => ISO8601Duration
   use mapl_ISO8601_DateTime_mod, only: mapl_ISO8601Interval => ISO8601Interval
   use mapl_DateTime_Parsing_mod
   use mapl_Sleep_mod
   use mapl_CF_Time_mod

   ! Keys for ESMF_Info objects used in MAPL. See module for keys.
   use mapl_esmf_info_keys_mod

   ! Validation
   use mapl_Validation_mod

   ! Memory info
   use mapl_MemInfo_mod, only: mapl_MemInfo => MemInfo
   use mapl_MemInfo_mod, only: mapl_MemInfoWrite => MemInfoWrite
   use mapl_TimeUtilities_mod

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
   public :: mapl_KeywordEnforcer

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
   public :: mapl_PackDate
   public :: mapl_PackDateTime
   public :: mapl_UnpackDate

   ! ISO8601 date/time conversion
   public :: mapl_convert_ISO8601_to_integer_time
   public :: mapl_convert_ISO8601_to_integer_date
   ! ISO8601 date/time data types
   public :: mapl_ISO8601Date
   public :: mapl_ISO8601Time
   public :: mapl_ISO8601DateTime
   public :: mapl_ISO8601Duration
   public :: mapl_ISO8601Interval

end module mapl_utils_api
