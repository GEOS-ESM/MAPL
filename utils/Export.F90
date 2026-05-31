! Export umbrella for the MAPL utils layer.
! Defines the public API of utils/ for external consumers.
! Uses mapl_utils_internal and re-exports only the intended public symbols.
module mapl_utils_export

   use mapl_utils_internal

   implicit none
   private

   ! Error handling

   ! Keyword enforcer

   ! String types and utilities

   ! OS / filesystem

   ! Numeric utilities

   ! Time utilities
   ! public :: PackDate, PackDateTime, UnpackDate
   ! UnpackDateTime removed - conflicts with mp_utils version, see issue #5011
   ! public :: UnpackDateTime

   ! ESMF info keys
   ! KEY_UNITS and KEY_TYPEKIND excluded: values differ from mapl_HistoryConstants_mod
   ! homonyms; consumers needing them should use mapl_esmf_info_keys_mod directly.

   ! Memory info

   ! Validation

end module mapl_utils_export
