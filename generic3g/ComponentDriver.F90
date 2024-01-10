#include "MAPL_Generic.h"

module mapl3g_ComponentDriver
   use mapl3g_MultiState
   use mapl_ErrorHandlingMod
   use :: esmf
   implicit none
   private

   public :: ComponentDriver

   type, abstract :: ComponentDriver
      private
   contains
      procedure(I_run), deferred :: run
      procedure(I_run), deferred:: initialize
      procedure(I_run), deferred :: finalize
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

end module mapl3g_ComponentDriver
