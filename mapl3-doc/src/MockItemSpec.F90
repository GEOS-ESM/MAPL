#include "MAPL_Generic.h"

module MockItemSpecMod
   use mapl3g_StateItemSpec
   use mapl3g_AbstractActionSpec
   use mapl3g_VariableSpec
   use mapl3g_MultiState
   use mapl3g_ActualConnectionPt
   use mapl3g_ActualPtVector
   use mapl3g_ExtensionAction
   use mapl3g_NullAction
   use mapl3g_VerticalGrid
   use mapl_ErrorHandling
   use mapl_KeywordEnforcer
   use esmf
   implicit none
   private

   public :: MockItemSpec
   public :: MockAction

   ! Note - this leaks memory
   type, extends(StateItemSpec) :: MockItemSpec
      character(len=:), allocatable :: name
      character(len=:), allocatable :: subtype
      character(len=:), allocatable :: adapter_type
   contains
      procedure :: create
      procedure :: destroy
      procedure :: allocate
      procedure :: set_geometry

      procedure :: connect_to
      procedure :: can_connect_to
      procedure :: make_extension
      procedure :: extension_cost
      procedure :: make_adapters
      procedure :: add_to_state
      procedure :: add_to_bundle
   end type MockItemSpec

   type, extends(ExtensionAction) :: MockAction
      character(:), allocatable :: details
   contains
      procedure :: initialize
      procedure :: run
   end type MockAction

   interface MockItemSpec
      module procedure new_MockItemSpec
   end interface MockItemSpec

   interface MockAction
      module procedure new_MockAction
   end interface MockAction

   type, extends(StateItemAdapter) :: SubtypeAdapter
      character(:), allocatable :: subtype
   contains
      procedure :: adapt_one => adapt_subtype
      procedure :: match_one => match_subtype
   end type SubtypeAdapter

   interface SubtypeAdapter
      procedure :: new_SubtypeAdapter
   end interface SubtypeAdapter
      

   type, extends(StateItemAdapter) :: NameAdapter
      character(:), allocatable :: name
   contains
      procedure :: adapt_one => adapt_name
      procedure :: match_one => match_name
   end type NameAdapter

   interface NameAdapter
      procedure :: new_NameAdapter
   end interface NameAdapter
      
contains

   function new_MockItemSpec(name, subtype, adapter_type) result(spec)
      type(MockItemSpec) :: spec
      character(*), intent(in) :: name
      character(*), optional, intent(in) :: subtype
      character(*), optional, intent(in) :: adapter_type

      spec%name = name
      if (present(subtype)) spec%subtype = subtype
      if (present(adapter_type)) spec%adapter_type = adapter_type

   end function new_MockItemSpec

   subroutine set_geometry(this, geom, vertical_grid, rc)
      class(MockItemSpec), intent(inout) :: this
      type(ESMF_Geom), optional, intent(in) :: geom
      class(VerticalGrid), optional, intent(in) :: vertical_grid
      integer, optional, intent(out) :: rc

      _RETURN(_SUCCESS)
   end subroutine set_geometry

   subroutine create(this, rc)
      class(MockItemSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc


      _RETURN(ESMF_SUCCESS)
   end subroutine create


   subroutine destroy(this, rc)
      class(MockItemSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      _RETURN(ESMF_SUCCESS)
   end subroutine destroy


   ! Tile / Grid   X  or X, Y
   subroutine allocate(this, rc)
      class(MockItemSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc
      
      _RETURN(ESMF_SUCCESS)
   end subroutine allocate

   subroutine connect_to(this, src_spec, actual_pt, rc)
      class(MockItemSpec), intent(inout) :: this
      class(StateItemSpec), intent(inout) :: src_spec
      type(ActualConnectionPt), intent(in) :: actual_pt ! unused
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: can_connect

      can_connect = this%can_connect_to(src_spec, _RC)
      _ASSERT(can_connect, 'illegal connection')

      select type (src_spec)
      class is (MockItemSpec)
         ! ok
         this%name = src_spec%name
         if (allocated(src_spec%subtype)) then
            this%subtype = src_spec%subtype
         end if
      class default
         _FAIL('Cannot connect field spec to non field spec.')
      end select

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(actual_pt)
   end subroutine connect_to


   logical function can_connect_to(this, src_spec, rc)
      class(MockItemSpec), intent(in) :: this
      class(StateItemSpec), intent(in) :: src_spec
      integer, optional, intent(out) :: rc

      select type(src_spec)
      class is (MockItemSpec)
         can_connect_to = .true.
      class default
         can_connect_to = .false.
      end select

      _RETURN(_SUCCESS)
   end function can_connect_to


   subroutine add_to_state(this, multi_state, actual_pt, rc)
      class(MockItemSpec), intent(in) :: this
      type(MultiState), intent(inout) :: multi_state
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      type(ESMF_State) :: state
      type(ESMF_Info) :: info
      integer :: status

      call multi_state%get_state(state, actual_pt%get_state_intent(), _RC)
      call ESMF_InfoGetFromHost(state, info, _RC)
      call ESMF_InfoSet(info, key=actual_pt%get_full_name(), value=.true., _RC)

      _RETURN(_SUCCESS)
   end subroutine add_to_state

   subroutine add_to_bundle(this, bundle, rc)
      class(MockItemSpec), intent(in) :: this
      type(ESMF_FieldBundle), intent(inout) :: bundle
      integer, optional, intent(out) :: rc

      _FAIL('unimplemented')

   end subroutine add_to_bundle

   function new_MockAction(src_subtype, dst_subtype) result(action)
      type(MockAction) :: action
      character(*), optional, intent(in) :: src_subtype
      character(*), optional, intent(in) :: dst_subtype

      if (present(src_subtype) .and. present(dst_subtype)) then
         action%details = src_subtype // ' ==> ' // dst_subtype
      else
         action%details = 'no subtype'
      end if
   end function new_MockAction


   recursive subroutine make_extension(this, dst_spec, new_spec, action, rc)
      class(MockItemSpec), intent(in) :: this
      class(StateItemSpec), intent(in) :: dst_spec
      class(StateItemSpec), allocatable, intent(out) :: new_spec
      class(ExtensionAction), allocatable, intent(out) :: action
      integer, optional, intent(out) :: rc

      integer :: status
      type(MockItemSpec) :: tmp_spec

      action = NullAction()
      new_spec = this
      select type(dst_spec)
      type is (MockItemSpec)
         call make_extension_typesafe(this, dst_spec, tmp_spec, action, _RC)
         deallocate(new_spec)
         allocate(new_spec, source=tmp_spec)
         new_spec = tmp_spec
      class default
         _FAIL('incompatible spec')
      end select
      
      _RETURN(_SUCCESS)
   end subroutine make_extension

   subroutine make_extension_typesafe(this, dst_spec, new_spec, action, rc)
      class(MockItemSpec), intent(in) :: this
      type(MockItemSpec), intent(in) :: dst_spec
      class(MockItemSpec), intent(out) :: new_spec
      class(ExtensionAction), allocatable, intent(out) :: action
      integer, optional, intent(out) :: rc

      integer :: status

      action = NullAction()

      if (this%name /= dst_spec%name) then
         new_spec%name = dst_spec%name
         action = MockAction(this%subtype, new_spec%subtype)
         _RETURN(_SUCCESS)
      end if
      
      if (allocated(dst_spec%subtype) .and. allocated(this%subtype)) then
         if (this%subtype /= dst_spec%subtype) then
            new_spec%subtype = dst_spec%subtype
            action = MockAction(this%subtype, new_spec%subtype)
            _RETURN(_SUCCESS)
         end if
      end if

      _RETURN(_SUCCESS)

   end subroutine make_extension_typesafe
 
  integer function extension_cost(this, src_spec, rc) result(cost)
      class(MockItemSpec), intent(in) :: this
      class(StateItemSpec), intent(in) :: src_spec
      integer, optional, intent(out) :: rc

      integer :: status

      cost = 0
      select type(src_spec)
      type is (MockItemSpec)
         if (this%name /= src_spec%name) cost = cost + 1
         if (allocated(src_spec%subtype) .and. allocated(this%subtype)) then
            if (this%subtype /= src_spec%subtype) cost = cost + 1
         end if
      class default
         _FAIL('incompatible spec')
      end select

      _RETURN(_SUCCESS)
   end function extension_cost

   subroutine initialize(this, importState, exportState, clock, rc)
      use esmf
      class(MockAction), intent(inout) :: this
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock      
      integer, optional, intent(out) :: rc
      _FAIL('This procedure should not be called.')
   end subroutine initialize

   subroutine run(this, importState, exportState, clock, rc)
      use esmf
      class(MockAction), intent(inout) :: this
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock      
      integer, optional, intent(out) :: rc
      _FAIL('This procedure should not be called.')
   end subroutine run
   
   function make_adapters(this, goal_spec, rc) result(adapters)
      type(StateItemAdapterWrapper), allocatable :: adapters(:)
      class(MockItemSpec), intent(in) :: this
      class(StateItemSpec), intent(in) :: goal_spec
      integer, optional, intent(out) :: rc

      type(SubtypeAdapter) :: subtype_adapter
      type(NameAdapter) :: name_adapter
      allocate(adapters(0)) ! just in case

      select type (goal_spec)
      type is (MockItemSpec)

         
         if (allocated(this%adapter_type)) then
            select case (this%adapter_type)
            case ('subtype')
               deallocate(adapters)
               allocate(adapters(1))
               subtype_adapter = SubtypeAdapter(goal_spec%subtype)
               allocate(adapters(1)%adapter, source=subtype_adapter)
            case ('name')
               deallocate(adapters)
               allocate(adapters(1))
               name_adapter = NameAdapter(goal_spec%name)
               allocate(adapters(1)%adapter, source=name_adapter)
            case default
               _FAIL('unsupported adapter type')
            end select
         else
            deallocate(adapters)
            allocate(adapters(2))
            subtype_adapter = SubtypeAdapter(goal_spec%subtype)
            name_adapter = NameAdapter(goal_spec%name)
            allocate(adapters(1)%adapter, source=name_adapter)
            allocate(adapters(2)%adapter, source=subtype_adapter)
         end if
      end select

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(goal_spec)
   end function make_adapters

   subroutine adapt_subtype(this, spec, action)
      class(SubtypeAdapter), intent(in) :: this
      class(StateItemSpec), intent(inout) :: spec
      class(ExtensionAction), allocatable, intent(out) :: action

      select type (spec)
      type is (MockItemSpec)
         spec%subtype = this%subtype
         action = MockAction(spec%subtype, this%subtype)
      end select
   end subroutine adapt_subtype

   logical function match_subtype(this, spec) result(match)
      class(SubtypeAdapter), intent(in) :: this
      class(StateItemSpec), intent(in) :: spec

      match = .false.
      select type (spec)
      type is (MockItemSpec)
         if (allocated(this%subtype)) then
            if (allocated(spec%subtype)) then
               match = this%subtype == spec%subtype
            else
               match = .true.
            end if
         else
            match = .true.
         end if
      end select
      
   end function match_subtype

   subroutine adapt_name(this, spec, action)
      class(NameAdapter), intent(in) :: this
      class(StateItemSpec), intent(inout) :: spec
      class(ExtensionAction), allocatable, intent(out) :: action
      select type (spec)
      type is (MockItemSpec)
         spec%name = this%name
         action = MockAction()
      end select
   end subroutine adapt_name

   logical function match_name(this, spec) result(match)
      class(NameAdapter), intent(in) :: this
      class(StateItemSpec), intent(in) :: spec


      match = .false.
      select type (spec)
      type is (MockItemSpec)
         if (allocated(this%name)) then
            if (allocated(spec%name)) then
               match = this%name == spec%name
            else
               match = .true.
            end if
         else
            match = .true.
         end if
      end select
      
   end function match_name

   function new_SubtypeAdapter(subtype) result(adapter)
     type(SubtypeAdapter) :: adapter
     character(*), optional, intent(in) :: subtype
     if (present(subtype)) then
        adapter%subtype=subtype
     end if
   end function new_SubtypeAdapter
     
   function new_NameAdapter(name) result(adapter)
     type(NameAdapter) :: adapter
     character(*), optional, intent(in) :: name
     if (present(name)) then
        adapter%name=name
     end if
   end function new_NameAdapter
     
end module MockItemSpecMod
