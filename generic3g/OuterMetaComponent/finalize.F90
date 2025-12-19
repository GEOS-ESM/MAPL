#include "MAPL.h"

submodule (mapl3g_OuterMetaComponent) finalize_smod
   use mapl3g_GriddedComponentDriverMap
   use mapl3g_GenericPhases
   use mapl_ErrorHandling
   use mapl3g_Generic
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
      integer :: phase_idx, status
      character(*), parameter :: PHASE_NAME = 'GENERIC::FINALIZE_USER'
      type(StringVector), pointer :: finalize_phases
      logical :: found
  
      call recurse_finalize_(this, phase_idx=GENERIC_FINALIZE_USER, _RC)

      ! User gridcomp may not have any given phase; not an error condition if not found.
      finalize_phases => this%user_phases_map%at(ESMF_METHOD_FINALIZE, _RC)
      phase_idx = get_phase_index(finalize_phases, phase_name=phase_name, found=found)
      _RETURN_UNLESS(found)

      call this%run_custom(ESMF_METHOD_FINALIZE, PHASE_NAME, _RC)

      ! TODO - component profile
      ! TODO - release resources

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine finalize

   recursive subroutine recurse_finalize_(this, phase_idx, rc)
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
           call child%finalize(phase_idx=phase_idx, _RC)
        end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine recurse_finalize_

end submodule finalize_smod
