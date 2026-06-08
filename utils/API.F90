! Export umbrella for the MAPL utils layer.
! Defines the public API of utils/ for external consumers.
! Uses mapl_utils_internal and re-exports only the intended public symbols.
module mapl_utils_api

   use mapl_utils_internal
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
