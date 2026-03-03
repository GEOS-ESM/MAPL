#include "MAPL_ErrLog.h"

submodule (mapl3g_MeshDecomposition) make_MeshDecomposition_vm_smod
   use mapl_ErrorHandlingMod
   implicit none (type, external)

contains

   module procedure make_MeshDecomposition_vm
      integer :: status
      integer :: petCount

      call ESMF_VMGet(vm, petCount=petCount, _RC)
      decomp = MeshDecomposition(npoints, petCount=petCount)

      _RETURN(_SUCCESS)
   end procedure make_MeshDecomposition_vm

end submodule make_MeshDecomposition_vm_smod
