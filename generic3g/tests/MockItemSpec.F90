#include "MAPL_Generic.h"

module MockItemSpecMod
   use mapl3g_AbstractStateItemSpec
   use mapl_ErrorHandling
   use esmf
   implicit none
   private

   public :: MockItemSpec

   type, extends(AbstractStateItemSpec) :: MockItemSpec
      character(len=:), allocatable :: name
   contains
      procedure :: create
      procedure :: destroy
      procedure :: allocate

      procedure :: connect_to
      procedure :: can_connect_to
      procedure :: requires_extension
      procedure :: add_to_state
   end type MockItemSpec

   interface MockItemSpec
      module procedure new_MockItemSpec
   end interface MockItemSpec

contains

   function new_MockItemSpec(name) result(spec)
      type(MockItemSpec) :: spec
      character(*), intent(in) :: name

      spec%name = name
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

      requires_extension = .false.

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
   
end module MockItemSpecMod
