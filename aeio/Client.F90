#include "MAPL_Generic.h"


module AEIO_Client
   use ESMF
   use CollectionMod
   use MAPL_ExceptionHandling
   use AEIO_RHConnector
   
   implicit none
   private

   public :: Client
   type Client
      type(collection) :: hist_collection
      integer, allocatable :: pet_list(:,:)
      type(ESMF_FieldBundle) :: bundle
      type(RHConnector) :: server_connection
   contains
      procedure initialize
      procedure get_grid
      procedure get_bundle
      procedure set_client_server_connector
      procedure transfer_data_to_server
      procedure get_connector !bmaa
   end type

   interface Client
      module procedure new_Client
   end interface Client

contains

   subroutine initialize(this,state,rc)
      class(Client), intent(inout) :: this
      type(ESMF_State), intent(inout) :: state
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=:), allocatable :: collection_name

      collection_name = this%hist_collection%get_name()
      this%bundle=ESMF_FieldBundleCreate(name=collection_name,_RC)
      call this%hist_collection%fill_bundle(state,this%bundle,_RC)

      _RETURN(_SUCCESS)
   end subroutine initialize

   function new_Client(hist_collection,pet_list,rc) result(c)
      type(Collection), intent(in) :: hist_collection 
      integer, intent(in)          :: pet_list(:,:)
      integer, optional, intent(out) :: rc
      type(Client) :: c
      integer :: status

      c%hist_collection = hist_collection
      allocate(c%pet_list,source=pet_list,stat=status)
      _VERIFY(status)
      _RETURN(_SUCCESS)

   end function new_Client

   function get_bundle(this) result(bundle)
      class(Client), intent(inout) :: this
      type(ESMF_FieldBundle) :: bundle
      bundle=this%bundle
   end function

   function get_grid(this,rc) result(grid)
      class(Client), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Grid) :: grid
      call ESMF_FieldBundleGet(this%bundle,grid=grid,_RC)
      _RETURN(_SUCCESS)
   end function get_grid

   subroutine set_client_server_connector(this,rh)
      class(Client), intent(inout) :: this
      type(RHConnector), intent(in) :: rh

      this%server_connection = rh
      call this%server_connection%set_sender(.true.)
   end subroutine set_client_server_connector

   function get_connector(this) result(rh)
      class(Client),intent(inout) :: this
      type(RHConnector) :: rh
      rh=this%server_connection
   end function

   subroutine transfer_data_to_server(this,rc)
      class(client), intent(inout) :: this
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
         call this%server_connection%redist_fields(srcField=field,_RC) !bmaa
      enddo !bmaa
      !call this%server_connection%redist_fieldBundles(srcFieldBundle=this%bundle,_RC)
      _RETURN(_SUCCESS)

   end subroutine transfer_data_to_server

end module AEIO_Client
