#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) initialize_realize_provided_smod
   use mapl_enums_api, only: MAPL_GENERIC_INIT_REALIZE_PROVIDED
   use mapl_MultiState_mod
   use mapl_Connection_mod
   use mapl_ConnectionVector_mod, only: ConnectionVectorIterator
   use mapl_ConnectionVector_mod, only: operator(/=)
   use mapl_GriddedComponentDriverMap_mod
   use mapl_ErrorHandling_mod
   implicit none(type,external)

contains

   module recursive subroutine initialize_realize_provided(this, importState, exportState, clock, unusable, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status

      call this%publish_provided_metadata(_RC)
      call this%resolve_metadata(_RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
      _UNUSED_DUMMY(unusable)
   end subroutine initialize_realize_provided

   module recursive subroutine publish_provided_metadata(this, unusable, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      character(*), parameter :: PHASE_NAME = 'GENERIC::INIT_REALIZE_PROVIDED'
      type(MultiState) :: user_states
      type(GriddedComponentDriverMapIterator) :: iter
      type(GriddedComponentDriver), pointer :: child
      type(OuterMetaComponent), pointer :: child_meta
      type(ESMF_GridComp) :: child_gc

      user_states = this%user_gc_driver%get_states()
      call this%registry%add_to_states(user_states, mode='user', _RC)
      call publish_children(this, _RC)
      call this%run_custom(ESMF_METHOD_INITIALIZE, PHASE_NAME, _RC)
      call this%registry%propagate_exports(_RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   contains
      recursive subroutine publish_children(parent, rc)
         class(OuterMetaComponent), target, intent(inout) :: parent
         integer, optional, intent(out) :: rc

         associate (e => parent%children%end())
            iter = parent%children%begin()
            do while (iter /= e)
               child => iter%second()
               child_gc = child%get_gridcomp()
               child_meta => get_outer_meta(child_gc, _RC)
               call child_meta%publish_provided_metadata(_RC)
               call iter%next()
            end do
         end associate
         _RETURN(_SUCCESS)
      end subroutine publish_children
   end subroutine publish_provided_metadata

   module recursive subroutine resolve_metadata(this, unusable, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      character(*), parameter :: PHASE_NAME = 'GENERIC::INIT_REALIZE_METADATA'
      type(MultiState) :: user_states
      type(GriddedComponentDriverMapIterator) :: iter
      type(GriddedComponentDriver), pointer :: child
      type(OuterMetaComponent), pointer :: child_meta
      type(ESMF_GridComp) :: child_gc

      call process_connections(this, _RC)
      call this%registry%propagate_exports(_RC)
      user_states = this%user_gc_driver%get_states()
      call this%registry%add_to_states(user_states, mode='user', _RC)
      call resolve_children(this, _RC)
      call this%run_custom(ESMF_METHOD_INITIALIZE, PHASE_NAME, _RC)
      call this%registry%propagate_exports(_RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   contains
      recursive subroutine resolve_children(parent, rc)
         class(OuterMetaComponent), target, intent(inout) :: parent
         integer, optional, intent(out) :: rc

         associate (e => parent%children%end())
            iter = parent%children%begin()
            do while (iter /= e)
               child => iter%second()
               child_gc = child%get_gridcomp()
               child_meta => get_outer_meta(child_gc, _RC)
               call child_meta%resolve_metadata(_RC)
               call iter%next()
            end do
         end associate
         _RETURN(_SUCCESS)
      end subroutine resolve_children

      subroutine process_connections(component, rc)
         class(OuterMetaComponent), target, intent(inout) :: component
         integer, optional, intent(out) :: rc

         integer :: local_status
         type(ConnectionVectorIterator) :: connection_iter
         class(Connection), pointer :: connection

         associate (e => component%component_spec%connections%end())
            connection_iter = component%component_spec%connections%begin()
            do while (connection_iter /= e)
               connection => connection_iter%of()
               call connection%connect(component%registry, _RC)
               call connection_iter%next()
            end do
         end associate
         _RETURN(_SUCCESS)
      end subroutine process_connections
   end subroutine resolve_metadata

end submodule initialize_realize_provided_smod
