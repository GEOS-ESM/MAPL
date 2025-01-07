#include "MAPL_Generic.h"

module mapl3g_FakeDynGridComp

   use mapl_ErrorHandling
   use generic3g
   use esmf
   use mapl, only: MAPL_GetPointer

   implicit none
   private

   public :: SetServices

contains

   subroutine SetServices(gridcomp, rc)
      type(ESMF_GridComp) :: gridcomp
      integer, intent(out) :: rc

      integer :: status

      ! Set entry points
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, init, phase_name="GENERIC::INIT_USER", _RC)
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, run, phase_name="run", _RC)

      _RETURN(_SUCCESS)
   end subroutine setServices

   subroutine init(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gridcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out)  :: rc

      integer :: status
      real(kind=ESMF_KIND_R4), pointer :: pl(:, :, :), t_dyn(:, :, :)

      call MAPL_GetPointer(exportState, pl,  "PL", _RC)
      call set_pressure_(pl)

      call MAPL_GetPointer(exportState, t_dyn, "T_DYN", _RC)
      call set_temperature_(t_dyn)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(gridcomp)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(clock)
   end subroutine init

   subroutine run(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gridcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

      integer :: status

      call MAPL_RunChildren(gridcomp, phase_name="run", _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
   end subroutine run

   subroutine set_pressure_(pressure)
      real(kind=ESMF_KIND_R4), pointer, intent(inout) :: pressure(:, :, :)

      integer :: shape_(3), i, j, k, num_levels

      shape_ = shape(pressure)
      num_levels = shape_(3)
      do concurrent(i = 1:shape_(1), j = 1:shape_(2))
         do k = 1, num_levels
            pressure(i, j, k) = real((num_levels - k + 1) * 10)
         end do
      end do
   end subroutine set_pressure_

   subroutine set_temperature_(temperature)
      real(kind=ESMF_KIND_R4), pointer, intent(inout) :: temperature(:, :, :)

      integer :: shape_(3), i, j, k, num_levels

      shape_ = shape(temperature)
      num_levels = shape_(3)
      do concurrent(i = 1:shape_(1), j=1:shape_(2))
         do k = 1, num_levels
            temperature(i, j, k) = real(5 * (2 ** (num_levels - k)))
         end do
      end do
   end subroutine set_temperature_

end module mapl3g_FakeDynGridComp

subroutine SetServices(gridcomp, rc)
   use MAPL_ErrorHandlingMod
   use mapl3g_FakeDynGridComp, only: FakeDyn_SetServices => SetServices
   use esmf

   type(ESMF_GridComp), intent(inout)  :: gridcomp
   integer, intent(out) :: rc

   integer :: status

   call FakeDyn_SetServices(gridcomp, _RC)

   _RETURN(_SUCCESS)
end subroutine SetServices
