#include "MAPL_ErrLog.h"

submodule (mapl3g_MeshDecomposition) make_MeshDecomposition_current_vm_smod
   use mapl_ErrorHandlingMod
   implicit none (type, external)

contains

   module procedure make_MeshDecomposition_current_vm
      type(ESMF_VM) :: vm
      integer :: status

      call ESMF_VMGetCurrent(vm, _RC)
      decomp = make_MeshDecomposition(npoints, vm, _RC)

      _RETURN(_SUCCESS)
   end procedure make_MeshDecomposition_current_vm

end submodule make_MeshDecomposition_current_vm_smod
