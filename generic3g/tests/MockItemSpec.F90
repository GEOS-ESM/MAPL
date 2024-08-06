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
   contains
      procedure :: create
      procedure :: destroy
      procedure :: allocate

      procedure :: connect_to
      procedure :: can_connect_to
      procedure :: make_extension
      procedure :: new_make_extension
      procedure :: make_extension_typesafe
      procedure :: extension_cost
      procedure :: add_to_state
      procedure :: add_to_bundle
      procedure :: make_action
   end type MockItemSpec

   type, extends(ExtensionAction) :: MockAction
      character(:), allocatable :: details
   contains
      procedure :: run => mock_run
   end type MockAction

   interface MockItemSpec
      module procedure new_MockItemSpec
   end interface MockItemSpec

   interface MockAction
      module procedure new_MockAction
   end interface MockAction

contains

   function new_MockItemSpec(name, subtype) result(spec)
      type(MockItemSpec) :: spec
      character(*), intent(in) :: name
      character(*), optional, intent(in) :: subtype

      spec%name = name
      if (present(subtype)) spec%subtype = subtype

   end function new_MockItemSpec

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

   function make_action(this, dst_spec, rc) result(action)
      use mapl3g_ExtensionAction
      class(ExtensionAction), allocatable :: action
      class(MockItemSpec), intent(in) :: this
      class(StateItemSpec), intent(in) :: dst_spec
      integer, optional, intent(out) :: rc

      select type (dst_spec)
      type is (Mockitemspec)
         action = MockAction(this, dst_spec)
      class default
         _FAIL('unsupported subclass')
      end select

      _RETURN(_SUCCESS)
   end function make_action

   subroutine mock_run(this, rc)
      class(MockAction), intent(inout) :: this
      integer, optional, intent(out) :: rc

      _RETURN(_SUCCESS)
   end subroutine mock_run

   function make_extension(this, dst_spec, rc) result(extension)
      class(StateItemSpec), allocatable :: extension
      class(MockItemSpec), intent(in) :: this
      class(StateItemSpec), intent(in) :: dst_spec
      integer, optional, intent(out) :: rc

      integer :: status
      type(MockItemSpec) :: tmp
      
      select type(dst_spec)
      type is (MockItemSpec)
         tmp = this%make_extension_typesafe(dst_spec, _RC)
         allocate(extension, source=tmp)
      class default
         _FAIL('incompatible spec')
      end select

      _RETURN(_SUCCESS)
   end function make_extension

   function make_extension_typesafe(this, src_spec, rc) result(extension)
      type(MockItemSpec) :: extension
      class(MockItemSpec), intent(in) :: this
      class(MockItemSpec), intent(in) :: src_spec
      integer, optional, intent(out) :: rc

      integer :: status

     if (this%name /= src_spec%name) then
         extension%name = src_spec%name
         _RETURN(_SUCCESS)
      end if

      if (allocated(src_spec%subtype) .and. allocated(this%subtype)) then
         if (this%subtype /= src_spec%subtype) then
            extension%subtype = src_spec%subtype
            _RETURN(_SUCCESS)
         end if
      end if

      _RETURN(_SUCCESS)
   end function make_extension_typesafe

    subroutine new_make_extension(this, dst_spec, new_spec, action, rc)
      class(MockItemSpec), intent(in) :: this
      class(StateItemSpec), intent(in) :: dst_spec
      class(StateItemSpec), allocatable, intent(out) :: new_spec
      class(ExtensionAction), allocatable, intent(out) :: action
      integer, optional, intent(out) :: rc

      integer :: status
      type(MockItemSpec) :: tmp_spec

      action = NullAction() ! default
      new_spec = this

       select type(dst_spec)
       type is (MockItemSpec)
          call new_make_extension_typesafe(this, dst_spec, tmp_spec, action, _RC)
          deallocate(new_spec)
          new_spec = tmp_spec
      class default
         _FAIL('incompatible spec')
      end select

      _RETURN(_SUCCESS)
   end subroutine new_make_extension

   subroutine new_make_extension_typesafe(this, dst_spec, new_spec, action, rc)
      class(MockItemSpec), intent(in) :: this
      type(MockItemSpec), intent(in) :: dst_spec
      class(MockItemSpec), intent(out) :: new_spec
      class(ExtensionAction), allocatable, intent(out) :: action
      integer, optional, intent(out) :: rc

      integer :: status

      if (this%name /= dst_spec%name) then
         new_spec%name = dst_spec%name
         _RETURN(_SUCCESS)
      end if
      
      if (allocated(dst_spec%subtype) .and. allocated(this%subtype)) then
         if (this%subtype /= dst_spec%subtype) then
            new_spec%subtype = dst_spec%subtype
            _RETURN(_SUCCESS)
         end if
      end if

      _RETURN(_SUCCESS)

   end subroutine new_make_extension_typesafe
 
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

end module MockItemSpecMod
