#include "MAPL.h"

module MockItemSpecMod

   use mapl_ErrorHandling
   use mapl_KeywordEnforcer
   use mapl3g_StateItemSpec
   use mapl3g_VariableSpec
   use mapl3g_MultiState
   use mapl3g_ActualConnectionPt
   use mapl3g_ActualPtVector
   use mapl3g_ExtensionAction
   use mapl3g_NullAction
   use mapl3g_VerticalGrid
   use mapl3g_AspectCollection
   use mapl3g_StateItemAspect
   use mapl3g_UnitsAspect
   use mapl3g_TypekindAspect
   use esmf

   implicit none
   private

   public :: MockItemSpec
   public :: MockAction

   ! Note - this leaks memory
   type, extends(StateItemSpec) :: MockItemSpec
      character(len=:), allocatable :: name
   contains
      procedure :: create
      procedure :: destroy
      procedure :: allocate
      procedure :: set_geometry

      procedure :: connect_to
      procedure :: can_connect_to
      procedure :: add_to_state
      procedure :: add_to_bundle
      procedure :: write_formatted

      procedure :: get_aspect_priorities
   end type MockItemSpec

   type, extends(ExtensionAction) :: MockAction
      character(:), allocatable :: details
   contains
      procedure :: initialize
      procedure :: update
   end type MockAction

   interface MockItemSpec
      module procedure new_MockItemSpec
   end interface MockItemSpec

   interface MockAction
      module procedure new_MockAction
   end interface MockAction

contains

   function new_MockItemSpec(name, typekind, units) result(spec)
      type(MockItemSpec), target :: spec
      character(*), intent(in) :: name
      type(ESMF_Typekind_Flag), optional, intent(in) :: typekind
      character(*), optional, intent(in) :: units

      type(AspectCollection), pointer :: aspects

      spec%name = name

      aspects => spec%get_aspects()
      call aspects%set_aspect(TypekindAspect(typekind))
      call aspects%set_aspect(UnitsAspect(units))

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
      class(StateItemAspect), pointer :: aspect

      can_connect = this%can_connect_to(src_spec, _RC)
      _ASSERT(can_connect, 'illegal connection')

      select type (src_spec)
      class is (MockItemSpec)
         ! ok
         this%name = src_spec%name
         aspect => src_spec%get_aspect('UNITS', _RC)
         call this%set_aspect(aspect, _RC)
         aspect => src_spec%get_aspect('TYPEKIND', _RC)
         call this%set_aspect(aspect, _RC)
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

   subroutine write_formatted(this, unit, iotype, v_list, iostat, iomsg)
      class(MockItemSpec), intent(in) :: this
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      write(unit, "(a)", iostat=iostat, iomsg=iomsg) "MockItemSpec(write not implemented yet)"
   end subroutine write_formatted

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

   subroutine initialize(this, importState, exportState, clock, rc)
      use esmf
      class(MockAction), intent(inout) :: this
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock      
      integer, optional, intent(out) :: rc
      _FAIL('This procedure should not be called.')
   end subroutine initialize

   subroutine update(this, importState, exportState, clock, rc)
      use esmf
      class(MockAction), intent(inout) :: this
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock      
      integer, optional, intent(out) :: rc
      _FAIL('This procedure should not be called.')
   end subroutine update
   
   function get_aspect_priorities(src_spec, dst_spec) result(order)
      character(:), allocatable :: order
      class(MockItemSpec), intent(in) :: src_spec
      class(StateItemSpec), intent(in) :: dst_spec

      select case (src_spec%name)
      case ('0')
         order = ''
      case ('1')
         order = 'TYPEKIND'
      case ('3')
         order = 'TYPEKIND::UNITS'
      case default
         order = 'TYPEKIND::UNITS'
      end select
   end function get_aspect_priorities

end module MockItemSpecMod
