#include "MAPL_Generic.h"

module mapl3g_ComponentDriver
   use mapl3g_MultiState
   use mapl_ErrorHandlingMod
   use :: MaplShared, only: KeywordEnforcer
   use :: esmf
   implicit none
   private

   public :: ComponentDriver
   public :: initialize_phases

   type, abstract :: ComponentDriver
      private
   contains
      procedure(I_run), deferred :: run
      procedure(I_run), deferred :: initialize
      procedure(I_run), deferred :: finalize
      procedure(I_run), deferred :: read_restart
      procedure(I_run), deferred :: write_restart
   end type ComponentDriver

   abstract interface

      recursive subroutine I_run(this, unusable, phase_idx, rc)
         use :: MaplShared, only: KeywordEnforcer
         import ComponentDriver
         class(ComponentDriver), intent(inout) :: this
         class(KeywordEnforcer), optional, intent(in) :: unusable
         integer, optional, intent(in) :: phase_idx
         integer, optional, intent(out) :: rc
      end subroutine I_run

   end interface

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
