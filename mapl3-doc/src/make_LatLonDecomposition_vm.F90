#include "MAPL_ErrLog.h"

submodule (mapl3g_LatLonDecomposition) make_LatLonDecomposition_vm_smod
   use mapl_ErrorHandlingMod
   use MAPL_Base
   implicit none (type, external)

contains

   module function make_LatLonDecomposition_vm(dims, vm, rc) result(decomp)
      type(LatLonDecomposition) :: decomp
      integer, intent(in) :: dims(2)
      type(ESMF_VM), intent(in) :: vm
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: petCount

      call ESMF_VMGet(vm, petCount=petCount, _RC)
      decomp = LatLonDecomposition(dims, petCount=petCount)

      _RETURN(_SUCCESS)
   end function make_LatLonDecomposition_vm

end submodule make_LatLonDecomposition_vm_smod

