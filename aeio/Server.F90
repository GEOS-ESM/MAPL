#include "MAPL_Generic.h"


module AEIO_Server
   use MPI
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
      integer :: collection_id
      integer :: connector_comm
      integer :: server_comm
      integer, allocatable :: pet_list(:,:)
      integer, allocatable :: server_ranks(:)
      integer, allocatable :: writer_ranks(:)
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

      integer :: rank,status
      call MPI_COMM_RANK(this%connector_comm,rank,status)
      if (rank==0) then
         am_root=.true.
      else
         am_root=.false.
      end if
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

   function new_Server(hist_collection,collection_id,pet_list,connector_comm,server_comm,server_ranks,writer_ranks,rc) result(c)
      type(Collection), intent(in) :: hist_collection
      integer, intent(in)          :: collection_id 
      integer, intent(in)          :: pet_list(:,:)
      integer, intent(in)          :: connector_comm
      integer, intent(in)          :: server_comm
      integer, intent(in)          :: server_ranks(:)
      integer, intent(in)          :: writer_ranks(:)
      integer, optional, intent(out) :: rc
      type(Server) :: c
      integer :: status

      c%hist_collection = hist_collection
      c%collection_id = collection_id
      allocate(c%pet_list,source=pet_list,stat=status)
      _VERIFY(status)
      c%connector_comm = connector_comm
      c%server_comm = server_comm
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
      integer :: i,server_size,worker_rank
      integer :: MPI_STAT(MPI_STATUS_SIZE)

      if (this%i_am_server_root()) then
         call MPI_Send(this%collection_id,1,MPI_INTEGER,this%writer_ranks(1), &
             this%writer_ranks(1),this%connector_comm,status)
         _VERIFY(status)
         call MPI_Recv(worker_rank,1,MPI_INTEGER,this%writer_ranks(1), &
              this%server_ranks(1),this%connector_comm,MPI_STAT,status)
         write(*,*)"bmaa got worker rank back: ",worker_rank
         _VERIFY(status)
      end if
      write(*,*)"bmaa start telling!"
      call MPI_Bcast(worker_rank,1,MPI_INTEGER,this%server_ranks(1),this%server_comm,status)
      write(*,*)"bmaa end telling!"
      _VERIFY(status)

      ! transfer prototype
      server_size = this%pet_list(2,2)-this%pet_list(2,1)+1
      allocate(originPetList(server_size+1))
      allocate(targetPetList(server_size+1))
      do i=1,server_size
         originPetList(i)=this%pet_list(2,1)+i-1
         targetPetList(i)=this%pet_list(2,1)+i-1
      enddo
      originPetList(server_size+1)=this%pet_list(3,1)
      targetPetList(server_size+1)=this%pet_list(3,1)+worker_rank
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
