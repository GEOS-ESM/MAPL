#include "MAPL.h"

module mapl3g_ComponentDriver
   use mapl3g_MultiState
   use mapl_ErrorHandlingMod
   use :: MaplShared, only: KeywordEnforcer
   use mapl3g_MultiState
   use :: esmf
   implicit none
   private

   public :: ComponentDriver
   public :: ComponentDriverPtr
   public :: mapl_DriverInitializePhases

   type, abstract :: ComponentDriver
      private
   contains
      procedure(I_run), deferred :: run
      procedure(I_run), deferred :: initialize
      procedure(I_run), deferred :: finalize
      procedure(I_run), deferred :: write_restart

      procedure(I_get_states), deferred :: get_states
   end type ComponentDriver

   type :: ComponentDriverPtr
      class(ComponentDriver), pointer :: ptr
   end type ComponentDriverPtr

   abstract interface

      recursive subroutine I_run(this, unusable, phase_idx, rc)
         use :: MaplShared, only: KeywordEnforcer
         import ComponentDriver
         class(ComponentDriver), intent(inout) :: this
         class(KeywordEnforcer), optional, intent(in) :: unusable
         integer, optional, intent(in) :: phase_idx
         integer, optional, intent(out) :: rc
      end subroutine I_run

      function I_get_states(this) result(states)
         import ComponentDriver
         import multistate
         type(MultiState) :: states
         class(ComponentDriver), intent(in) :: this
      end function I_get_states

   end interface

   interface mapl_DriverInitializePhases
      procedure :: initialize_phases
   end interface mapl_DriverInitializePhases

contains

      recursive subroutine initialize_phases(this, unusable, phases, rc)
         class(ComponentDriver), intent(inout) :: this
         class(KeywordEnforcer), optional, intent(in) :: unusable
         integer, optional, intent(in) :: phases(:)
         integer, optional, intent(out) :: rc

         integer :: status
         integer :: i

         do i = 1, size(phases)
            call this % initialize(phase_idx=phases(i), _RC)
         end do
         
         _RETURN(_SUCCESS)
         _UNUSED_DUMMY(unusable)
      end subroutine initialize_phases

end module mapl3g_ComponentDriver
