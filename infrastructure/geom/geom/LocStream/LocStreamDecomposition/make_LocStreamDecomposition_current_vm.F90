#include "MAPL_ErrLog.h"

submodule (mapl3g_LocStreamDecomposition) make_LocStreamDecomposition_current_vm_smod
   use mapl_ErrorHandlingMod
   implicit none (type, external)

contains

   module procedure make_LocStreamDecomposition_current_vm
      type(ESMF_VM) :: vm
      integer :: status

      call ESMF_VMGetCurrent(vm, _RC)
      decomp = make_LocStreamDecomposition(npoints, vm, _RC)

      _RETURN(_SUCCESS)
   end procedure make_LocStreamDecomposition_current_vm

end submodule make_LocStreamDecomposition_current_vm_smod
