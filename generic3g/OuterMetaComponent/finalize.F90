#include "MAPL.h"

submodule (mapl3g_OuterMetaComponent) finalize_smod
   use mapl3g_GriddedComponentDriverMap
   use mapl3g_GenericPhases
   use mapl_ErrorHandling
   implicit none (type, external)

contains

   module recursive subroutine finalize(this, importState, exportState, clock, unusable, rc)
      class(OuterMetaComponent), intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      ! optional arguments
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(GriddedComponentDriver), pointer :: child
      type(GriddedComponentDriverMapIterator) :: iter
      integer :: status
      character(*), parameter :: PHASE_NAME = 'GENERIC::FINALIZE_USER'
      type(StringVector), pointer :: finalize_phases
      logical :: found

      finalize_phases => this%user_phases_map%at(ESMF_METHOD_FINALIZE, _RC)
      ! User gridcomp may not have any given phase; not an error condition if not found.
      associate (phase => get_phase_index(finalize_phases, phase_name=phase_name, found=found))
        _RETURN_UNLESS(found)

        ! TODO:  Should user finalize be after children finalize?

        ! TODO:  Should there be a phase option here?  Probably not
        ! right as is when things get more complicated.

        call this%run_custom(ESMF_METHOD_FINALIZE, PHASE_NAME, _RC)

        associate(b => this%children%begin(), e => this%children%end())
          iter = b
          do while (iter /= e)
             child => iter%second()
             call child%finalize(phase_idx=GENERIC_FINALIZE_USER, _RC)
             call iter%next()
          end do
        end associate
      end associate

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
      _UNUSED_DUMMY(unusable)
   end subroutine finalize

end submodule finalize_smod
