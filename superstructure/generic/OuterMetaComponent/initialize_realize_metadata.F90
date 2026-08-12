#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) initialize_realize_metadata_smod
   use mapl_enums_api, only: MAPL_GENERIC_INIT_REALIZE_METADATA
   use mapl_MultiState_mod
   use mapl_Connection_mod
   use mapl_ConnectionPt_mod
   use mapl_ConnectionVector_mod, only: ConnectionVectorIterator
   use mapl_ConnectionVector_mod, only: operator(/=)
   use mapl_ErrorHandling_mod
   implicit none(type,external)

contains

   module recursive subroutine initialize_realize_metadata(this, importState, exportState, clock, unusable, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      character(*), parameter :: PHASE_NAME = 'GENERIC::INIT_REALIZE_METADATA'
      type(MultiState) :: user_states

      call process_connections(this, _RC)
      call this%registry%propagate_exports(_RC)
      user_states = this%user_gc_driver%get_states()
      call this%registry%add_to_states(user_states, mode='user', _RC)
      call recurse(this, phase_idx=MAPL_GENERIC_INIT_REALIZE_METADATA, _RC)
      call this%run_custom(ESMF_METHOD_INITIALIZE, PHASE_NAME, _RC)
      call this%registry%propagate_exports(_RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
      _UNUSED_DUMMY(unusable)
   end subroutine initialize_realize_metadata

   subroutine process_connections(this, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(ConnectionVectorIterator) :: iter
      class(Connection), pointer :: c

      associate (e => this%component_spec%connections%end())
        iter = this%component_spec%connections%begin()
        do while (iter /= e)
           c => iter%of()
           call c%connect(this%registry, _RC)
           call iter%next()
        end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine process_connections

end submodule initialize_realize_metadata_smod
