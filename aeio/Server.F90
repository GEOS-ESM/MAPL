#include "MAPL_Generic.h"


module AEIO_Server
   use MPI
   use ESMF
   use CollectionMod
   use MAPL_ExceptionHandling
   use gFTL_StringVector
   use AEIO_RHConnector
   use AEIO_MpiConnection
   
   implicit none
   private

   public :: Server

   type Server
      type(collection) :: hist_collection
      integer :: collection_id
      type(MpiConnection) :: front_back_connection
      type(ESMF_FieldBundle) :: bundle
      type(RHConnector) :: client_connection
      type(RHConnector) :: writer_prototype_conn
      type(RHConnector) :: writer_conn
   contains
      procedure initialize
      procedure set_grid
      procedure get_grid
      procedure get_bundle
      procedure set_client_server_connector
      procedure set_server_writer_prototype
      procedure get_server_writer_prototype
      procedure get_data_from_client
      procedure get_writer
      procedure offload_data
      procedure get_connector
      procedure i_am_server_root
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

   function i_am_server_root(this) result(am_root)
      class(Server), intent(inout) :: this
      logical :: am_root

      am_root = this%front_back_connection%am_i_front_root()

   end function i_am_server_root

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

   function new_Server(hist_collection,collection_id,mpi_connection,rc) result(c)
      type(Collection), intent(in) :: hist_collection
      integer, intent(in)          :: collection_id 
      type(MpiConnection), intent(in) :: mpi_connection
      integer, optional, intent(out) :: rc
      type(Server) :: c
      integer :: status

      c%hist_collection = hist_collection
      c%collection_id = collection_id
      c%front_back_connection = mpi_connection
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
      class(server), intent(inout) :: this
      type(rhconnector), intent(out) :: rh

      rh = this%writer_prototype_conn

   end subroutine get_server_writer_prototype

   subroutine get_data_from_client(this,rc)
      class(server), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      call this%client_connection%redist_fieldBundles(dstFieldBundle=this%bundle,_RC)
      _RETURN(_SUCCESS)
   end subroutine get_data_from_client

   subroutine get_writer(this,rc)
      class(server), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      ! here is where we 
      ! get work we will talk to
      ! generate new rh from prototype
      ! send data
      ! how do we send the actual filename? what the heck the file should look like including time?
      integer, allocatable :: originPetList(:),targetPetList(:)
      integer :: server_size,worker_rank
      integer :: MPI_STAT(MPI_STATUS_SIZE)
      integer :: connector_comm,front_comm
      integer, allocatable :: front_ranks(:),back_ranks(:),front_pets(:),back_pets(:)

      connector_comm = this%front_back_connection%get_connection_comm()
      front_comm = this%front_back_connection%get_front_comm()
      front_pets = this%front_back_connection%get_front_pets()
      back_pets = this%front_back_connection%get_back_pets()
      front_ranks = this%front_back_connection%get_front_mpi_ranks()
      back_ranks = this%front_back_connection%get_back_mpi_ranks()

      if (this%front_back_connection%am_i_front_root()) then
         call MPI_Send(this%collection_id,1,MPI_INTEGER,back_ranks(1), &
             back_ranks(1),connector_comm,status)
         _VERIFY(status)
         call MPI_Recv(worker_rank,1,MPI_INTEGER,back_ranks(1), &
              front_ranks(1),connector_comm,MPI_STAT,status)
         _VERIFY(status)
      end if
      call MPI_Bcast(worker_rank,1,MPI_INTEGER,front_ranks(1),front_comm,status)
      _VERIFY(status)

      ! transfer prototype
      
      server_size = size(front_pets)
      allocate(originPetList(server_size+1))
      allocate(targetPetList(server_size+1))
      originPetList(1:server_size)=front_pets
      targetPetList(1:server_size)=front_pets
      originPetList(server_size+1)=back_pets(1)
      targetPetList(server_size+1)=worker_rank
      this%writer_conn = this%writer_prototype_conn%transfer_rh(originPetList,targetPetList,_RC)
      
      _RETURN(_SUCCESS)
   end subroutine get_writer

   subroutine offload_data(this,rc)
      class(server), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i
      integer :: fieldCount
      type(ESMF_Field) :: field
      type(ESMF_Array) :: array
      character(len=ESMF_MAXSTR), allocatable :: fieldNames(:)

      ! transfer arrays
      call ESMF_FieldBundleGet(this%bundle,fieldCount=fieldCount,_RC)
      allocate(fieldNames(fieldCount))
      call ESMF_FieldBundleGet(this%bundle,fieldNameList=fieldNames,_RC)
      do i=1,fieldCount
         call ESMF_FieldBundleGet(this%bundle,trim(fieldNames(i)),field=field,_RC)
         call ESMF_FieldGet(field,array=array,_RC)
         call this%writer_conn%redist_arrays(srcArray=array,_RC)
      enddo

      call this%writer_conn%destroy(_RC)
     
      _RETURN(_SUCCESS)
   end subroutine offload_data

end module AEIO_Server
