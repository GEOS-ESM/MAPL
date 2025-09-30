#include "MAPL.h"

submodule (mapl3g_OuterMetaComponent) initialize_modify_advertised_smod
   use mapl3g_GenericPhases
   use mapl3g_MultiState
   use mapl3g_Connection
   use mapl3g_ConnectionVector, only: ConnectionVectorIterator
   use mapl3g_ConnectionVector, only: operator(/=)
   use mapl_ErrorHandling
   implicit none

contains

   module recursive subroutine initialize_modify_advertised(this, importState, exportState, clock, unusable, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      ! optional arguments
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      character(*), parameter :: PHASE_NAME = 'GENERIC::INIT_MODIFY_ADVERTISED'
      type(MultiState) :: user_states

      user_states = this%user_gc_driver%get_states()
      call this%registry%add_to_states(user_states, mode='user', _RC)

      call this%run_custom(ESMF_METHOD_INITIALIZE, PHASE_NAME, _RC)
      call recurse(this, phase_idx=GENERIC_INIT_MODIFY_ADVERTISED, _RC)

      call process_connections(this, _RC)
      call this%registry%propagate_exports(_RC)

      _RETURN(_SUCCESS)

      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
   end subroutine initialize_modify_advertised
   
   subroutine process_connections(this, rc)
      class(OuterMetaComponent), intent(inout) :: this
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

end submodule initialize_modify_advertised_smod
