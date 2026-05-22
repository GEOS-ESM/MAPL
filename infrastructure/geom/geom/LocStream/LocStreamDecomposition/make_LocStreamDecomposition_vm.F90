#include "MAPL_ErrLog.h"

submodule (mapl_LocStreamDecomposition_mod) make_LocStreamDecomposition_vm_smod
   use mapl_ErrorHandling_mod
   implicit none (type, external)

contains

   module procedure make_LocStreamDecomposition_vm
      integer :: status
      integer :: petCount

      call ESMF_VMGet(vm, petCount=petCount, _RC)
      decomp = LocStreamDecomposition(npoints, petCount=petCount)

      _RETURN(_SUCCESS)
   end procedure make_LocStreamDecomposition_vm

end submodule make_LocStreamDecomposition_vm_smod
