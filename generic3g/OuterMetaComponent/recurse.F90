#include "MAPL.h"

submodule (mapl3g_OuterMetaComponent) recurse_smod
   use mapl3g_GriddedComponentDriverMap
   use mapl_ErrorHandling
   implicit none

contains

   ! This procedure is used to recursively invoke a given ESMF phase down
   ! the hierarchy.
   module recursive subroutine recurse_(this, phase_idx, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer :: phase_idx
      integer, optional, intent(out) :: rc

      integer :: status
      type(GriddedComponentDriverMapIterator) :: iter
      type(GriddedComponentDriver), pointer :: child

      associate(e => this%children%ftn_end())
        iter = this%children%ftn_begin()
        do while (iter /= e)
           call iter%next()
           child => iter%second()
           call child%initialize(phase_idx=phase_idx, _RC)
        end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine recurse_

   ! This procedure is used to recursively invoke write_restart
   module recursive subroutine recurse_write_restart_(this, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(GriddedComponentDriverMapIterator) :: iter
      type(GriddedComponentDriver), pointer :: child

      associate(e => this%children%ftn_end())
        iter = this%children%ftn_begin()
        do while (iter /= e)
           call iter%next()
           child => iter%second()
           call child%write_restart(_RC)
        end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine recurse_write_restart_

end submodule recurse_smod
