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
      integer, allocatable :: pet_list(:,:)
      type(ESMF_FieldBundle) :: bundle
      type(RHConnector) :: client_connection
   contains
      procedure initialize
      procedure set_grid
      procedure get_grid
      procedure get_bundle
      procedure set_client_server_connector
   end type

   interface Server
      module procedure new_Server
   end interface Server

contains

   subroutine initialize(this,state,rc)
      class(Server), intent(inout) :: this
      type(ESMF_State), intent(inout) :: state
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Grid) :: grid
      !type(ESMF_State) :: state1

      !state1 = ESMF_StateCreate(__RC__)
      call ESMF_FieldBundleGet(this%bundle,grid=grid,__RC__)
      call this%hist_collection%fill_bundle(state,this%bundle,grid=grid,__RC__)
      !call ESMF_StateAdd(state1,[this%bundle],__RC__)
      !call ESMF_StateReconcile(state1,__RC__)

      _RETURN(_SUCCESS)
   end subroutine initialize

   function new_Server(hist_collection,pet_list,rc) result(c)
      type(Collection), intent(in) :: hist_collection 
      integer, intent(in)          :: pet_list(:,:)
      integer, optional, intent(out) :: rc
      type(Server) :: c
      integer :: status

      c%hist_collection = hist_collection
      allocate(c%pet_list,source=pet_list,stat=status)
      _VERIFY(status)
      c%bundle=ESMF_FieldBundleCreate(__RC__)
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
      
      call ESMF_FieldBundleSet(this%bundle,grid=grid,__RC__)
      _RETURN(_SUCCESS)

   end subroutine set_grid

   function get_grid(this,rc) result(grid)
      class(server), intent(inout) :: this
      integer, optional, intent(out) :: rc

      type(esmf_grid) :: grid
      integer :: status
      
      call ESMF_FieldBundleGet(this%bundle,grid=grid,__RC__)
      _RETURN(_SUCCESS)

   end function get_grid

   subroutine set_client_server_connector(this,rh)
      class(Server), intent(inout) :: this
      type(RHConnector), intent(in) :: rh

      this%client_connection = rh
      call this%client_connection%set_sender(.false.)
   end subroutine set_client_server_connector

end module AEIO_Server
