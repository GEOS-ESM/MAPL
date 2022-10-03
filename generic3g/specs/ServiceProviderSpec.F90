module mapl3g_ServiceProviderSpec
   use mapl3g_AbstractStateItemSpec
   implicit none
   private

   public :: ServiceProviderSpec

   ! A service provider specifies the name of a service and the Field
   ! characteristics that all subscribers must adhere to.  E.g., the
   ! service provider currently specifies a grid, extra dims, and a
   ! halo.  Subscribers can pass fields on different grids or halos,
   ! in which case an extension can be inserted.  Service should not care
   ! about units (needs to be thought about).  Extensions cannot handle
   ! differing extra dims.

   type, extends(AbstractStateItemSpec) :: ServiceProviderSpec
      character(:), allocatable :: service_name
      type(ESMF_Grid) :: grid
      type(ExtraDimsSpec) :: dims_spec
      integer :: halo_width
      
      type(ESMF_FieldBundle) :: payload
      type(ConnectionPoint), allocatable :: items(:)
   contains
      procedure :: create
      procedure :: destroy
      procedure :: allocate

      procedure :: connect_to
      procedure :: can_connect_to
      procedure :: requires_extension

   end type ServiceProviderSpec

   interface ServiceProviderSpec
      module procedure new_ServiceProviderSpec
   end interface ServiceProviderSpec

contains

   function new_ServiceProviderSpec(service_name, grid) result(spec)
      type(ServiceProviderSpec) :: spec
      character(*), intent(in) :: service_name
      type(ESMF_GridComp), intent(in) :: grid

      spec%service_name = service_name
      spec%grid = grid

   end function new_ServiceProviderSpec

   subroutine create(this, rc)
      class(ServiceProviderSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      this%payload = ESMF_FieldBundleCreate(_RC)
      call ESMF_FieldBundleSet(this%payload, this%grid, _RC)

      _RETURN(_SUCCESS)
   end subroutine create


   subroutine destroy(this, rc)
      class(ServiceProviderSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      call ESMF_FieldBundleDestroy(this%payload, _RC)

      _RETURN(_SUCCESS)
   end subroutine destroy

   subroutine allocate(this, rc)
      class(ServiceRequesterSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      _RETURN(_SUCCESS)
   end subroutine allocate



   subroutine connect_to(this, dst, rc)
      class(ServiceProviderSpec), intent(inout) :: this
      class(AbstractStateItemSpec), intent(in) :: dst
      integer, optional, intent(out) :: rc

      _ASSERT(this%can_connect_to(dst), 'merge requested for incompatible spec')
      _ASSERT(.not. this%requires_extension(dst), 'merge requires intermediate extension')

      select type (dst)
      type is (ServiceProviderSpec)
         ! This case should only arise in E2E context, and as such we
         ! expect the connection that trigers this merge() to happen
         ! immediately after the parent export is created.  As such,
         ! the parent will not have been populated by any E2I
         ! connections at this point.
         ! Other is "dst", this is "src".
         _ASSERT(size(other%items) == 0, 'Bad E2E connection for service provider.')
      type is (ServiceRequestorSpec)  ! E2I
         this%items = [this%items, other%items]
      class default
         _FAIL(...)
      end select
      
      _RETURN(_SUCCESS)
   end subroutine merge
   
end module mapl3g_ServiceProviderSpec
