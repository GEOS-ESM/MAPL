#include "MAPL.h"

! Test-only component for the callback_freeze_captest Cap case: its
! SetServices registers a callback interface, which must fail because the
! Cap freezes the callback registry before any SetServices runs.
module mapl_CallbackFreezeGridComp_mod
   use mapl_CallbackInterfaceRegistry_mod, only: register_callback_interface
   use mapl_CallbackInterface_mod, only: CallbackInterface, make_CallbackInterface
   use mapl_CallbackInterfaceId_mod, only: CallbackInterfaceId
   use mapl_ErrorHandling_mod
   use esmf, only: ESMF_GridComp
   implicit none(type, external)
   private

   public :: setServices

contains

   subroutine setServices(gridcomp, rc)
      type(ESMF_GridComp) :: gridcomp
      integer, intent(out) :: rc

      type(CallbackInterface) :: iface
      type(CallbackInterfaceId) :: id
      integer :: status

      iface = make_CallbackInterface(1, 'registered from SetServices', _RC)
      call register_callback_interface('late_interface', iface, id, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(gridcomp)
   end subroutine setServices

end module mapl_CallbackFreezeGridComp_mod

subroutine setServices(gridcomp, rc)
   use esmf, only: ESMF_GridComp
   use mapl_ErrorHandling_mod
   use mapl_CallbackFreezeGridComp_mod, only: CallbackFreeze_setServices => setServices
   type(ESMF_GridComp) :: gridcomp
   integer, intent(out) :: rc

   integer :: status

   call CallbackFreeze_setServices(gridcomp, _RC)

   _RETURN(_SUCCESS)
end subroutine setServices
