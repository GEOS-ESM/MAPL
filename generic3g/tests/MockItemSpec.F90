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
      character(len=:), allocatable :: filter_type
   contains
      procedure :: create
      procedure :: destroy
      procedure :: allocate
      procedure :: set_geometry

      procedure :: connect_to
      procedure :: can_connect_to
      procedure :: make_extension
      procedure :: extension_cost
      procedure :: make_filters
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

   type, extends(StateItemFilter) :: SubtypeFilter
      character(:), allocatable :: subtype
   contains
      procedure :: apply_one => match_subtype
   end type SubtypeFilter

   interface SubtypeFilter
      procedure :: new_SubtypeFilter
   end interface SubtypeFilter
      

   type, extends(StateItemFilter) :: NameFilter
      character(:), allocatable :: name
   contains
      procedure :: apply_one => match_name
   end type NameFilter

   interface NameFilter
      procedure :: new_NameFilter
   end interface NameFilter
      
contains

   function new_MockItemSpec(name, subtype, filter_type) result(spec)
      type(MockItemSpec) :: spec
      character(*), intent(in) :: name
      character(*), optional, intent(in) :: subtype
      character(*), optional, intent(in) :: filter_type

      spec%name = name
      if (present(subtype)) spec%subtype = subtype
      if (present(filter_type)) spec%filter_type = filter_type

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

   function new_MockAction(src_spec, dst_spec) result(action)
      type(MockAction) :: action
      type(MockItemSpec), intent(in) :: src_spec
      type(MockItemSpec), intent(in) :: dst_spec

      if (allocated(src_spec%subtype) .and. allocated(dst_spec%subtype)) then
         action%details = src_spec%subtype // ' ==> ' // dst_spec%subtype
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
         action = MockAction(this, new_spec)
         _RETURN(_SUCCESS)
      end if
      
      if (allocated(dst_spec%subtype) .and. allocated(this%subtype)) then
         if (this%subtype /= dst_spec%subtype) then
            new_spec%subtype = dst_spec%subtype
            action = MockAction(this, new_spec)
            action = MockAction()
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
   
   function make_filters(this, goal_spec, rc) result(filters)
      type(StateItemFilterWrapper), allocatable :: filters(:)
      class(MockItemSpec), intent(in) :: this
      class(StateItemSpec), intent(in) :: goal_spec
      integer, optional, intent(out) :: rc

      type(SubtypeFilter) :: subtype_filter
      type(NameFilter) :: name_filter
      allocate(filters(0)) ! just in case

      select type (goal_spec)
      type is (MockItemSpec)

         
         if (allocated(this%filter_type)) then
            select case (this%filter_type)
            case ('subtype')
               deallocate(filters)
               allocate(filters(1))
               subtype_filter = SubtypeFilter(goal_spec%subtype)
               allocate(filters(1)%filter, source=subtype_filter)
            case ('name')
               deallocate(filters)
               allocate(filters(1))
               name_filter = NameFilter(goal_spec%name)
               allocate(filters(1)%filter, source=name_filter)
            case default
               _FAIL('unsupported filter type')
            end select
         else
            deallocate(filters)
            allocate(filters(2))
            subtype_filter = SubtypeFilter(goal_spec%subtype)
            name_filter = NameFilter(goal_spec%name)
            allocate(filters(1)%filter, source=name_filter)
            allocate(filters(2)%filter, source=subtype_filter)
         end if
      end select

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(goal_spec)
   end function make_filters

   logical function match_subtype(this, spec) result(match)
      class(SubtypeFilter), intent(in) :: this
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

   logical function match_name(this, spec) result(match)
      class(NameFilter), intent(in) :: this
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

   function new_SubtypeFilter(subtype) result(filter)
     type(SubtypeFilter) :: filter
     character(*), optional, intent(in) :: subtype
     if (present(subtype)) then
        filter%subtype=subtype
     end if
   end function new_SubtypeFilter
     
   function new_NameFilter(name) result(filter)
     type(NameFilter) :: filter
     character(*), optional, intent(in) :: name
     if (present(name)) then
        filter%name=name
     end if
   end function new_NameFilter
     
end module MockItemSpecMod
