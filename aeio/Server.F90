#include "MAPL_Generic.h"


module AEIO_Server
   use ESMF
   use CollectionMod
   use MAPL_ExceptionHandling
   use gFTL_StringVector
   use AEIO_RHConnector
   
   implicit none
   private

   public :: Server

   type Server
      type(collection) :: hist_collection
      integer, allocatable :: server_ranks(:)
      integer, allocatable :: writer_ranks(:)
      type(ESMF_FieldBundle) :: bundle
      type(RHConnector) :: client_connection
      type(RHConnector) :: writer_prototype_conn
   contains
      procedure initialize
      procedure set_grid
      procedure get_grid
      !procedure set_prototype_grid
      !procedure get_prototype_grid
      procedure get_bundle
      procedure set_client_server_connector
      procedure set_server_writer_prototype
      procedure get_server_writer_prototype
      procedure get_data_from_client
      procedure offload_data
      procedure get_connector
   end type

   interface Server
      module procedure new_Server
   end interface Server

contains

   function get_connector(this) result(rh)
      class(Server), intent(inout) :: this
      type(RHConnector) :: rh
      rh=this%client_connection
   end function get_connector

   subroutine initialize(this,state,rc)
      class(Server), intent(inout) :: this
      type(ESMF_State), intent(inout) :: state
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Grid) :: grid

      call ESMF_FieldBundleGet(this%bundle,grid=grid,_RC)
      call this%hist_collection%fill_bundle(state,this%bundle,grid=grid,_RC)

      _RETURN(_SUCCESS)
   end subroutine initialize

   function new_Server(hist_collection,server_ranks,writer_ranks,rc) result(c)
      type(Collection), intent(in) :: hist_collection 
      integer, intent(in)          :: server_ranks(:)
      integer, intent(in)          :: writer_ranks(:)
      integer, optional, intent(out) :: rc
      type(Server) :: c
      integer :: status

      c%hist_collection = hist_collection
      allocate(c%server_ranks,source=server_ranks,stat=status)
      _VERIFY(status)
      allocate(c%writer_ranks,source=writer_ranks,stat=status)
      _VERIFY(status)
      c%bundle = ESMF_FieldBundleCreate(_RC)
      _RETURN(_SUCCESS)

   end function new_Server

   function get_bundle(this) result(bundle)
      class(Server), intent(inout) :: this
      type(ESMF_FieldBundle) :: bundle
      bundle=this%bundle
   end function

   subroutine set_grid(this,grid,rc)
      class(server), intent(inout) :: this
      type(esmf_grid), intent(in) :: grid
      integer, optional, intent(out) :: rc

      integer :: status
      
      call ESMF_FieldBundleSet(this%bundle,grid=grid,_RC)
      _RETURN(_SUCCESS)

   end subroutine set_grid

   function get_grid(this,rc) result(grid)
      class(server), intent(inout) :: this
      integer, optional, intent(out) :: rc

      type(esmf_grid) :: grid
      integer :: status
      
      call ESMF_FieldBundleGet(this%bundle,grid=grid,_RC)
      _RETURN(_SUCCESS)

   end function get_grid

   !subroutine set_prototype_grid(this,grid,rc)
      !class(server), intent(inout) :: this
      !type(esmf_grid), intent(in) :: grid
      !integer, optional, intent(out) :: rc
!
      !integer :: status
      !call ESMF_FieldBundleSet(this%prototype_bundle,grid=grid,_RC)
      !_RETURN(_SUCCESS)
!
   !end subroutine set_prototype_grid

   !function get_prototype_grid(this,rc) result(grid)
      !class(server), intent(inout) :: this
      !integer, optional, intent(out) :: rc
!
      !type(esmf_grid) :: grid
      !integer :: status
     ! 
      !call ESMF_FieldBundleGet(this%prototype_bundle,grid=grid,_RC)
      !_RETURN(_SUCCESS)
!
   !end function get_prototype_grid

   subroutine set_client_server_connector(this,rh)
      class(Server), intent(inout) :: this
      type(RHConnector), intent(in) :: rh

      this%client_connection = rh
      call this%client_connection%set_sender(.false.)

   end subroutine set_client_server_connector

   subroutine set_server_writer_prototype(this,rh)
      class(Server), intent(inout) :: this
      type(RHConnector), intent(in) :: rh

      this%writer_prototype_conn = rh

   end subroutine set_server_writer_prototype

   subroutine get_server_writer_prototype(this,rh)
      class(Server), intent(inout) :: this
      type(RHConnector), intent(out) :: rh

      rh = this%writer_prototype_conn

   end subroutine get_server_writer_prototype

   subroutine get_data_from_client(this,rc)
      class(server), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Field) :: field !bmaa
      integer :: field_count,i !bmaa
      character(len=ESMF_MAXSTR), allocatable :: field_names(:) !bmaa

      call ESMF_FieldBundleGet(this%bundle,fieldCount=field_count,_RC) !bmaa
      allocate(field_names(field_count)) !bmaa
      call ESMF_FieldBundleGet(this%bundle,fieldNameList=field_names,_RC) !bmaa
      do i=1,field_count !bmaa
         call ESMF_FieldBundleGet(this%bundle,trim(field_names(i)),field=field,_RC) !bmaa
         call this%client_connection%redist_fields(dstField=field,_RC) !bmaa
      enddo !bmaa

      !call this%client_connection%redist_fieldBundles(dstFieldBundle=this%bundle,_RC)
      _RETURN(_SUCCESS)
   end subroutine get_data_from_client

   subroutine offload_data(this,rc)
      class(server), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      ! here is where we 
      ! get work we will talk to
      ! generate new rh from prototype
      ! send data
      ! how do we send the actual filename? what the heck the file should look like including time?
      type(ESMF_Field) :: field !bmaa
      integer :: field_count,i !bmaa
      character(len=ESMF_MAXSTR), allocatable :: field_names(:) !bmaa
      real, pointer :: ptr2d(:,:),ptr3d(:,:,:) !bmaa
      integer :: rank !bmaa

      call ESMF_FieldBundleGet(this%bundle,fieldCount=field_count,_RC) !bmaa
      allocate(field_names(field_count)) !bmaa
      call ESMF_FieldBundleGet(this%bundle,fieldNameList=field_names,_RC) !bmaa
      do i=1,field_count !bmaa
         call ESMF_FieldBundleGet(this%bundle,trim(field_names(i)),field=field,_RC) !bmaa
         call ESMF_FieldGet(field,rank=rank,_RC) !bmaa
         if (rank==2) then ! bmaa
            call ESMF_FieldGet(field,farrayptr=ptr2d,_RC) !bmaa
            write(*,*)"bmaa 2d: ",maxval(ptr2d) !bmaa
         else if (rank==3) then ! bmaa
            call ESMF_FieldGet(field,farrayptr=ptr3d,_RC) !bmaa
            write(*,*)"bmaa 3d: ",maxval(ptr3d) !bmaa
         end if !bmaa
      enddo !bmaa
      _RETURN(_SUCCESS)
   end subroutine offload_data

end module AEIO_Server
