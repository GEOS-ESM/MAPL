#include "MAPL.h"

! Client code would look something like:

! Call MAPL_AddService('x', ['T','U'])

! The intermediate layer should assemble an array of ConnectionPoint
! objects with component name and 'internal' for state intent:

! allocate(c_pts(n_fields))
! do i = 1, n_fields
!    c_pts(i) = ConnectionPoint(names(i), component_name=this%name, intent='internal')
! end do
! call this%add_import_spec(ServiceRequesterSpec(service_name, c_pts))
! deallocate(c_pts)


module mapl3g_ServiceRequesterSpec
   use mapl3g_StateItemSpec
   use gftl2_StringVector
   implicit none
   private

   public :: ServiceRequesterSpec

   type, extends(StateItemSpec) :: ServiceRequesterSpec
      character(:), allocatable :: service_name
      type(ConnectionPoint), allocatable :: items(:)
   contains
      procedure :: create => noop
      procedure :: destroy => noop
      procedure :: allocate => noop

      procedure :: connect_to
      procedure :: can_connect_to
      procedure :: requires_coupler
   end type ServiceRequesterSpec

   interface ServiceRequesterSpec
      module procedure new_ServiceRequesterSpec
   end interface ServiceRequesterSpec

contains

   pure function new_ServiceRequesterSpec(service_name, items) result(spec)
      type(ServiceRequesterSpec) :: spec
      character(*), intent(in) :: service_name
      type(ConnectionPoint), intent(in) :: items(:)

      spec%service_name = service_name
      spec%items = items
      
   end function new_ServiceRequesterSpec

   subroutine noop(this, rc)
      class(ServiceRequesterSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      _RETURN(_SUCCESS)
   end subroutine noop

   subroutine connect_to(this, other, rc)
      class(ServiceRequesterSpec), intent(inout) :: this
      class(StateItemSpec), intent(in) :: other
      integer, optional, intent(out) :: rc

      _ASSERT(this%can_connect_to(other), 'merge requested for incompatible spec')
      _ASSERT(.not. this%requires_coupler(other), 'connection must be to intermediate coupler')

      select type (other)
      type is (ServiceRequesterSpec)
         this%items = [this%items, other%items]
      class default
         _FAIL(...)
      end select
      
      _RETURN(_SUCCESS)
   end subroutine connect_to
   
   subroutine can_connect_to(this, dst_spec)
      class(ServiceRequesterSpec), intent(inout) :: this
      class(StateItemSpec), intent(in) :: other

      can_connect_to = .false. ! unless

      select type (dst_spec)
      type is (ServiceRequesterSpec)
         can_connect_to = .true.
      end select
      
      _RETURN(_SUCCESS)
   end subroutine connect_to

   subroutine requires_coupler(this, dst_spec)
      class(ServiceRequesterSpec), intent(inout) :: this
      class(StateItemSpec), intent(in) :: other

      requires_coupler = .false. ! unless

      _RETURN(_SUCCESS)
   end subroutine connect_to

end module mapl3g_ServiceRequesterSpec


