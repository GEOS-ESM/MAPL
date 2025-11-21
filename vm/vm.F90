#include "MAPL.h"
module mapl3g_vm
   use mapl_ErrorHandling
   use esmf, only: esmf_VM
   use esmf, only: esmf_VMGetCurrent
   use esmf, only: esmf_VMGet
   use esmf, only: esmf_VMBarrier
   implicit none(type,external)
   private

   public :: mapl_AmIRoot
   public :: mapl_AmIPet
   public :: mapl_Barrier

   interface mapl_AmIRoot
      procedure :: am_I_root
   end interface mapl_AmIRoot

   interface mapl_AmIPet
      procedure :: am_I_root
   end interface mapl_AmIPet

   interface mapl_Barrier
      procedure :: barrier
   end interface mapl_Barrier

contains

   logical function am_I_root(vm)
      type (esmf_VM), optional :: vm

      am_i_root = am_i_pet(vm, pet=0)

   end function am_I_root

   logical function am_i_pet(vm, pet, rc)
      type(esmf_VM), optional :: vm
      integer, optional :: pet
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: pet_
      integer :: localpet
      type(esmf_VM) :: vm_

      pet_ = 0
      if (present(pet)) pet_ = pet

      vm_ = current_vm(_RC)
      call esmf_VMGet(vm_, localPet=localPet, _RC)

      am_i_pet = (localPet == pet_)
      _RETURN(_SUCCESS)
   end function am_i_pet

   subroutine barrier(vm, rc)
      type(esmf_VM), optional, intent(in) :: vm
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_VM) :: vm_

      vm_ = current_vm(_RC)
      call esmf_VMBarrier(vm_, _RC)

      _RETURN(_SUCCESS)
   end subroutine barrier

   function current_vm(vm, rc)
      type(esmf_VM) :: current_vm
      type(esmf_VM), optional, intent(in) :: vm
      integer, optional, intent(out) :: rc

      integer :: status

      if (present(vm)) then
         current_vm = vm
         _RETURN(_SUCCESS)
      end if

      call esmf_VMGetCurrent(current_vm, _RC)

      _RETURN(_SUCCESS)
   end function current_vm

end module mapl3g_vm
