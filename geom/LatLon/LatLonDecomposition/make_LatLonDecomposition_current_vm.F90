#include "MAPL_ErrLog.h"

submodule (mapl3g_LatLonDecomposition) make_LatLonDecomposition_current_vm_smod
   use mapl_ErrorHandlingMod
   implicit none (type, external)

contains

   ! Static factory methods
   module function make_LatLonDecomposition_current_vm(dims, rc) result(decomp)
      type(LatLonDecomposition) :: decomp
      integer, intent(in) :: dims(2)
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_VM) :: vm

      call ESMF_VMGetCurrent(vm, _RC)
      decomp = make_LatLonDecomposition(dims, vm, _RC)

      _RETURN(_SUCCESS)
   end function make_LatLonDecomposition_current_vm

end submodule make_LatLonDecomposition_current_vm_smod

