#include "MAPL_Generic.h"

module mapl3g_ComponentSpec
   use mapl3g_AbstractStateItemSpec
   use mapl3g_RelativeConnectionPoint
   use mapl3g_ConnectionSpecVector
   use mapl3g_ConnectionSpec
   use mapl3g_RelConnPtStateItemSpecMap
   use mapl3g_HierarchicalRegistry
   use mapl_ErrorHandling
   use ESMF
   implicit none
   private

   public :: ComponentSpec

   type :: ComponentSpec
!!$      private
      type(RelConnPtStateItemSpecMap) :: state_item_specs
      type(ConnectionSpecVector) :: connections
   contains
      procedure :: add_state_item_spec
      procedure :: add_connection

      procedure :: make_primary_states
      procedure :: process_connections
      procedure :: process_connection
   end type ComponentSpec

   interface ComponentSpec
      module procedure new_ComponentSpec
   end interface ComponentSpec

contains

   function new_ComponentSpec(state_item_specs, connections) result(spec)
      type(ComponentSpec) :: spec
      type(RelConnPtStateItemSpecMap), optional, intent(in) :: state_item_specs
      type(ConnectionSpecVector), optional, intent(in) :: connections

      if (present(state_item_specs)) spec%state_item_specs = state_item_specs
      if (present(connections)) spec%connections = connections
   end function new_ComponentSpec


   subroutine add_state_item_spec(this, conn_pt, spec)
      class(ComponentSpec), intent(inout) :: this
      type(RelativeConnectionPoint), intent(in) :: conn_pt
      class(AbstractStateItemSpec), intent(in) :: spec
      call this%state_item_specs%insert(conn_pt, spec)
   end subroutine add_state_item_spec


   subroutine add_connection(this, connection)
      class(ComponentSpec), intent(inout) :: this
      type(ConnectionSpec), intent(in) :: connection
      call this%connections%push_back(connection)
   end subroutine add_connection


   subroutine make_primary_states(this, registry, comp_states, rc)
      class(ComponentSpec), intent(in) :: this
      type(HierarchicalRegistry), intent(in) :: registry
      type(ESMF_State), intent(in) :: comp_states
      integer, optional, intent(out) :: rc

      integer :: status
      type(RelConnPtStateItemSpecMapIterator) :: iter

      associate (e => this%state_item_specs%end())
        iter = this%state_item_specs%begin()
        do while (iter /= e)
           call add_item_to_state(iter, registry, comp_states, _RC)
           call iter%next()
        end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine make_primary_states

   subroutine add_item_to_state(iter, registry, comp_states, rc)
      type(RelConnPtStateItemSpecMapIterator), intent(in) :: iter
      type(HierarchicalRegistry), intent(in) :: registry
      type(ESMF_State), intent(in) :: comp_states
      integer, optional, intent(out) :: rc

      class(AbstractStateItemSpec), pointer :: spec
      integer :: status
      type(ESMF_State) :: primary_state
      type(RelativeConnectionPoint), pointer :: conn_pt
      
      conn_pt => iter%first()
      spec => registry%get_item_spec(conn_pt)
      _ASSERT(associated(spec), 'invalid connection point')

      call ESMF_StateGet(comp_states, itemName=conn_pt%state_intent(), nestedState=primary_state, _RC)
      call add_to_state(primary_state, conn_pt, spec, _RC)
      
      _RETURN(_SUCCESS)
   end subroutine add_item_to_state


   subroutine add_to_state(state, relative_pt, spec, rc)
      type(ESMF_State), intent(inout) :: state
      type(RelativeConnectionPoint), intent(in) :: relative_pt
      class(AbstractStateItemSpec), intent(in) :: spec
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_State) :: innermost_state

!!$      innermost_state = create_substates(state, relative_pt%substates, _RC)
!!$      call spec%add_to_state(innermost_state, short_name, _RC)
!!$
!!$      _RETURN(_SUCCESS)
   end subroutine add_to_state


   function create_substates(state, substates, rc) result(innermost_state)
      use gftl2_StringVector
      type(ESMF_State) :: innermost_state
      type(ESMF_State), intent(inout) :: state
      type(StringVector), intent(in) :: substates
      integer, optional, intent(out) :: rc


!!$      type(StringVectorIterator) :: iter
!!$      character(:), pointer :: substate_name
!!$      integer :: itemcount
!!$      integer :: status

!!$      innermost_state = state
!!$      associate (e => substates%end())
!!$        iter = substates%begin()
!!$        do while (iter /= e)
!!$           substate_name => iter%of()
!!$           call ESMF_StateGet(innermost_state, itemSearch=substate_name, itemCount=itemcount, _RC)
!!$
!!$           select case (itemcount)
!!$           case (0)
!!$              call ESMF_StateCreate(substate, name=substate_name, _RC)
!!$              call ESMF_StateAdd(innermost_state, substate, _RC)
!!$           case (1)
!!$              call ESMF_StateGet(innermost_state, itemName=substate_name, substate, _RC)
!!$           case default
!!$              _FAIL('Duplicate substate name found in create_substates()')
!!$           end select
!!$
!!$           innermost_state = substate
!!$           call iter%next()
!!$        end do
!!$      end associate
!!$
!!$      _RESULT(_SUCCESS)
   end function create_substates

   subroutine process_connections(this, rc)
      class(ComponentSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(ConnectionSpecVectorIterator) :: iter
      type(ConnectionSpec), pointer :: conn

      associate (e => this%connections%end())
        iter = this%connections%begin()
        do while (iter /= e)
           conn => iter%of()
!!$           call this%validate_user_connection(conn, _RC)
           call this%process_connection(conn, _RC)
           call iter%next()
        end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine process_connections


   subroutine process_connection(this, conn, rc)
      class(ComponentSpec), intent(inout) :: this
      type(ConnectionSpec) :: conn
      integer, optional, intent(out) :: rc

!!$      integer :: status

!!$      src_comp => this%get_source_comp(connection)
!!$      dst_comp => this%get_dest_comp(connection)
!!$      if (.not. src_comp%can_connect(dst_comp, connection)) then
!!$         _FAIL(...)
!!$      end if
!!$      
!!$      call src_comp%do_connect(dst_comp, connection)

      _RETURN(_SUCCESS)
   end subroutine process_connection


end module mapl3g_ComponentSpec

