#include "MAPL_ErrLog.h"
submodule (mapl3g_LatLonGeomFactory) get_ranks_smod
   use mapl3g_GeomSpec
   use mapl3g_LonAxis
   use mapl3g_LatAxis
   use mapl3g_LatLonDecomposition
   use mapl3g_LatLonGeomSpec
   use mapl_MinMaxMod
   use mapl_ErrorHandlingMod
   use mapl_Constants
   use pFIO
   use gFTL2_StringVector
   use esmf
   use mapl_KeywordEnforcer, only: KE => KeywordEnforcer
   implicit none


contains


   module subroutine get_ranks(nx, ny, ix, iy, rc)
      integer, intent(in) :: nx, ny
      integer, intent(out) :: ix, iy
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: petCount, localPet
      type(ESMF_VM) :: vm

      call ESMF_VMGetCurrent(vm, _RC)
      call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, _RC)

      ix = mod(localPet, nx)
      iy = localPet / nx

      _RETURN(_SUCCESS)
   end subroutine get_ranks

end submodule get_ranks_smod
