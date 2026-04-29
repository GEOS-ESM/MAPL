#include "MAPL.h"

submodule (mapl3g_XYGeomFactory) add_mask_smod
   use mapl_ErrorHandlingMod
   use mapl_InternalConstants, only: MAPL_UNDEFINED_REAL64, MAPL_MASK_IN, MAPL_MASK_OUT, &
        MAPL_DESTINATIONMASK
   use esmf
   implicit none

contains

   ! Add an ESMF item mask wherever coordinates equal MAPL_UNDEF.
   ! Only needed when the coordinate arrays contain missing values
   ! (e.g. scan lines that don't intersect the Earth disk for ABI).
   module subroutine add_mask(spec, grid, rc)
      type(XYGeomSpec), intent(in) :: spec
      type(ESMF_Grid),  intent(inout) :: grid
      integer, optional, intent(out) :: rc

      integer :: status
      integer(ESMF_KIND_I4), pointer :: mask(:,:)
      real(ESMF_KIND_R8),    pointer :: fptr_lon(:,:), fptr_lat(:,:)
      type(ESMF_VM) :: vm
      integer :: has_undef, local_has_undef
      type(ESMF_Info) :: infoh

      call ESMF_GridGetCoord(grid, coordDim=1, localDE=0, &
           staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=fptr_lon, _RC)
      call ESMF_GridGetCoord(grid, coordDim=2, localDE=0, &
           staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=fptr_lat, _RC)

       if (any(fptr_lon == MAPL_UNDEFINED_REAL64) .or. any(fptr_lat == MAPL_UNDEFINED_REAL64)) then
         local_has_undef = 1
      else
         local_has_undef = 0
      end if

      call ESMF_VMGetCurrent(vm, _RC)
      call ESMF_VMAllFullReduce(vm, [local_has_undef], has_undef, 1, ESMF_REDUCE_MAX, _RC)
      _RETURN_IF(has_undef == 0)

      call ESMF_GridAddItem(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
           itemflag=ESMF_GRIDITEM_MASK, _RC)
      call ESMF_GridGetItem(grid, localDE=0, staggerLoc=ESMF_STAGGERLOC_CENTER, &
           itemflag=ESMF_GRIDITEM_MASK, farrayPtr=mask, _RC)

      mask = MAPL_MASK_IN
      where (fptr_lon == MAPL_UNDEFINED_REAL64 .or. fptr_lat == MAPL_UNDEFINED_REAL64) &
           mask = MAPL_MASK_OUT

      call ESMF_InfoGetFromHost(grid, infoh, _RC)
      call ESMF_InfoSet(infoh, key=MAPL_DESTINATIONMASK, values=[MAPL_MASK_OUT], _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(spec)
   end subroutine add_mask

end submodule add_mask_smod
