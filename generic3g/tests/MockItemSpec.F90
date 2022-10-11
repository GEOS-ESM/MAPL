#include "MAPL_Generic.h"

module MockItemSpecMod
   use mapl3g_AbstractStateItemSpec
   use mapl3g_AbstractActionSpec
   use mapl_ErrorHandling
   use esmf
   implicit none
   private

   public :: MockItemSpec
   public :: MockActionSpec

   ! Note - this leaks memory
   type, extends(AbstractStateItemSpec) :: MockItemSpec
      character(len=:), allocatable :: name
      character(len=:), allocatable :: subtype
   contains
      procedure :: create
      procedure :: destroy
      procedure :: allocate

      procedure :: connect_to
      procedure :: can_connect_to
      procedure :: requires_extension
      procedure :: make_extension
      procedure :: add_to_state
   end type MockItemSpec

   type, extends(AbstractActionSpec) :: MockActionSpec
      character(:), allocatable :: details
   end type MockActionSpec

   interface MockItemSpec
      module procedure new_MockItemSpec
   end interface MockItemSpec

   interface MockActionSpec
      module procedure new_MockActionSpec
   end interface MockActionSpec

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

      integer :: status
      
      call this%set_created()

      _RETURN(ESMF_SUCCESS)
   end subroutine create


   subroutine destroy(this, rc)
      class(MockItemSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      call this%set_created(.false.)

      _RETURN(ESMF_SUCCESS)
   end subroutine destroy


   ! Tile / Grid   X  or X, Y
   subroutine allocate(this, rc)
      class(MockItemSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      
      _RETURN(ESMF_SUCCESS)
   end subroutine allocate


   subroutine connect_to(this, src_spec, rc)
      class(MockItemSpec), intent(inout) :: this
      class(AbstractStateItemSpec), intent(in) :: src_spec
      integer, optional, intent(out) :: rc

      integer :: status

      _ASSERT(this%can_connect_to(src_spec), 'illegal connection')

      select type (src_spec)
      class is (MockItemSpec)
         ! ok
         this%name = src_spec%name
         call this%set_active(src_spec%is_active())
      class default
         _FAIL('Cannot connect field spec to non field spec.')
      end select

      _RETURN(ESMF_SUCCESS)

   end subroutine connect_to


   logical function can_connect_to(this, src_spec)
      class(MockItemSpec), intent(in) :: this
      class(AbstractStateItemSpec), intent(in) :: src_spec

      select type(src_spec)
      class is (MockItemSpec)
         can_connect_to = .true.
      class default
         can_connect_to = .false.
      end select

   end function can_connect_to


   logical function requires_extension(this, src_spec)
      class(MockItemSpec), intent(in) :: this
      class(AbstractStateItemSpec), intent(in) :: src_spec

      select type(src_spec)
      class is (MockItemSpec)
         if (allocated(this%subtype) .and. allocated(src_spec%subtype)) then
            requires_extension = (this%subtype /= src_spec%subtype)
         else
            requires_extension = (allocated(this%subtype) .eqv. allocated(src_spec%subtype))
         end if
      class default
         requires_extension = .false. ! should never get here
      end select

   end function requires_extension


   subroutine add_to_state(this, state, short_name, rc)
      class(MockItemSpec), intent(in) :: this
      type(ESMF_State), intent(inout) :: state
      character(*), intent(in) :: short_name
      integer, optional, intent(out) :: rc

      type(ESMF_Field) :: alias
      integer :: status

      _FAIL('unimplemented')

   end subroutine add_to_state

   function new_MockActionSpec(subtype_1, subtype_2) result(action_spec)
      type(MockActionSpec) :: action_spec
      character(*), intent(in) :: subtype_1, subtype_2

      action_spec%details = subtype_1 // ' ==> ' // subtype_2
   end function new_MockActionSpec

   function make_extension(this, src_spec, rc) result(action_spec)
      class(AbstractActionSpec), allocatable :: action_spec
      class(MockItemSpec), intent(in) :: this
      class(AbstractStateItemSpec), intent(in) :: src_spec
      integer, optional, intent(out) :: rc

      integer :: status

      select type(src_spec)
      type is (MockItemSpec)
         action_spec = MockActionSpec(this%subtype, src_spec%subtype)
      class default
         _FAIL('incompatible spec')
      end select

      _RETURN(_SUCCESS)
   end function make_extension
   
end module MockItemSpecMod
