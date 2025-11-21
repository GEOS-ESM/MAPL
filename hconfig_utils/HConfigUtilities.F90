#include "MAPL.h"
module mapl3g_HConfigUtilities
   use esmf, only: ESMF_HConfig, ESMF_HConfigIter, ESMF_HConfigIterBegin
   use esmf, only: ESMF_HConfigIterEnd, ESMF_HConfigIterLoop
   use esmf, only: ESMF_HConfigCreate, ESMF_HConfigIsMap, ESMF_HConfigAsStringMapKey
   use esmf, only: ESMF_HConfigIsDefined, ESMF_HConfigCreateAtMapVal, ESMF_HConfigSet
   use mapl_ErrorHandling
   implicit none(type,external)
   private

   public :: merge_hconfig

   character(*), parameter :: MAPL_SECTION = 'mapl'

contains

   ! Merge two hconfigs
   ! 1) Do not include parent `mapl` section
   ! 2) Duplicate keys defer to those of the child
   function merge_hconfig(parent_hconfig, child_hconfig, rc) result(total_hconfig)
      type(ESMF_HConfig) :: total_hconfig
      type(ESMF_HConfig), intent(in) :: parent_hconfig
#if defined(ESMF_HCONFIGSET_HAS_INTENT_INOUT)
      type(ESMF_HConfig), intent(inout) :: child_hconfig
#else
      type(ESMF_HConfig), intent(in) :: child_hconfig
#endif
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_HConfigIter) :: iter_begin, iter_end, iter
      character(:), allocatable :: key
      type(ESMF_HConfig) :: val
      logical :: duplicate_key

      _ASSERT(ESMF_HConfigIsMap(parent_hconfig), 'parent hconfig must be a mapping.')
      _ASSERT(ESMF_HConfigIsMap(child_hconfig), 'child hconfig must be a mapping.')
      total_hconfig = ESMF_HConfigCreate(child_hconfig, _RC)

      iter_begin = ESMF_HConfigIterBegin(parent_hconfig, rc=rc)
      iter_end = ESMF_HConfigIterEnd(parent_hconfig, rc=rc)
      iter = iter_begin
      do while (ESMF_HConfigIterLoop(iter, iter_begin, iter_end, rc=status))
         _VERIFY(status)

         ! ignore mapl section
         key = ESMF_HConfigAsStringMapKey(iter, rc=rc)
         if (key == MAPL_SECTION) cycle

         ! ignore duplicate key
         duplicate_key = ESMF_HConfigIsDefined(child_hconfig, keystring=key, _RC)
         if (duplicate_key) cycle

         val = ESMF_HConfigCreateAtMapVal(iter, _RC)
         call ESMF_HConfigSet(child_hconfig, keystring=key, content=val, _RC)
      end do

      _RETURN(_SUCCESS)
   end function merge_hconfig

end module mapl3g_HConfigUtilities

