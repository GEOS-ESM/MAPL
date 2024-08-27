#include "MAPL_Generic.h"

submodule (mapl3g_OuterMetaComponent) initialize_modify_advertise_smod
   implicit none

contains

   module recursive subroutine initialize_modify_advertise(this, importState, exportState, clock, unusable, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      ! optional arguments
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      character(*), parameter :: PHASE_NAME = 'GENERIC::INIT_MODIFY_ADVERTISE'
      type(MultiState) :: outer_states, user_states

      if (this%subphase == 0) then
         call self_advertise(this, _RC)
         call recurse(this, phase_idx=GENERIC_INIT_MODIFY_ADVERTISE, _RC)
         this%subphase = 1 - this%subphase
         _RETURN(_SUCCESS)
      end if

      call this%run_custom(ESMF_METHOD_INITIALIZE, PHASE_NAME, _RC)
      call process_connections(this, _RC)
      call this%registry%propagate_exports(_RC)

      user_states = this%user_gc_driver%get_states()
      call this%registry%add_to_states(user_states, mode='user', _RC)
      outer_states = MultiState(importState=importState, exportState=exportState)
      call this%registry%add_to_states(outer_states, mode='outer', _RC)
      call recurse(this, phase_idx=GENERIC_INIT_MODIFY_ADVERTISE, _RC)
      this%subphase = 1 - this%subphase

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine initialize_modify_advertise
   
   subroutine self_advertise(this, unusable, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      
      integer :: status

      call this%registry%initialize_specs(this%geom, this%vertical_grid, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine self_advertise

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

end submodule initialize_modify_advertise_smod
