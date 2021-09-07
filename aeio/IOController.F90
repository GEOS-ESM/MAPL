#include "MAPL_Generic.h"

module AEIO_IOController
   use MPI
   use MAPL_ExceptionHandling
   use ESMF
   use AEIO_Client
   use AEIO_ClientMap
   use AEIO_Server
   use AEIO_ServerMap
   use yafYaml
   use HistoryConfigMod
   use CollectionMod
   use CollectionRegistryMod
   use gFTL_StringVector
   use AEIO_TransferGridComp
   use AEIO_RHConnector
   
   implicit none
   private

   public IOController

   type IOController
      private
      type(ClientMap) :: clients
      type(ServerMap) :: servers
      integer, allocatable :: pet_list(:,:)
      type(StringVector) :: enabled
      integer :: server_comm
      integer :: front_comm
      integer :: back_comm

   contains
      procedure :: initialize
      procedure :: transfer_grid_to_front
      procedure :: transfer_grids_to_front
      procedure :: transfer_grid_to_back
      procedure :: transfer_grids_to_back
      procedure :: connect_client_server
!     procedure :: connect_server_writer
      procedure :: create_comms
   end type

contains

   subroutine initialize(this,state,configuration_file,clock,pet_list,rc)
      class(IOController), intent(inout) :: this
      type(ESMF_State), intent(inout) :: state
      character(len=*), intent(inout) :: configuration_file
      integer, intent(in) :: pet_list(:,:)
      type(ESMF_Clock), intent(in) :: clock
      integer, optional, intent(out) :: rc


      type(HistoryConfig) :: hist_config
      type(CollectionRegistry) :: coll_registry
      type(StringVectorIterator) :: enabled_iter
      type(Collection) :: hist_coll
      character(:), allocatable :: key
      type(Server) :: output_server
      type(Client) :: output_client
      type(Client), pointer :: client_ptr
      type(Server), pointer :: server_ptr


      integer :: status

      allocate(this%pet_list,source=pet_list)
      call this%create_comms()

      call hist_config%import_yaml_file(configuration_file,rc=status)
      _VERIFY(status)

      ! create client and server for each collection
      this%enabled = hist_config%get_enabled()
      coll_registry=hist_config%get_collections()
      enabled_iter = this%enabled%begin()
      do while(enabled_iter /= this%enabled%end())
         key=enabled_iter%get()
         hist_coll=coll_registry%at(key)
         output_server=Server(hist_coll,pet_list,rc=status)
         _VERIFY(status)
         call this%servers%insert(key,output_server)
         output_client=Client(hist_coll,pet_list,rc=status)
         _VERIFY(status)
         call this%clients%insert(key,output_client)
         call enabled_iter%next()
      enddo

      ! initialize client and server for each collection
      enabled_iter = this%enabled%begin()
      do while(enabled_iter /= this%enabled%end())
         key=enabled_iter%get()
         client_ptr => this%clients%at(key)
         call client_ptr%initialize(state,rc=status)
         _VERIFY(status)
         call enabled_iter%next()
      enddo

      ! first communication - send grid
      call this%transfer_grids_to_Front(rc=status)
      _VERIFY(status)

      ! fill bundle on front end of server for each collection
      enabled_iter = this%enabled%begin()
      do while(enabled_iter /= this%enabled%end())
         key=enabled_iter%get()
         server_ptr => this%servers%at(key)
         call server_ptr%initialize(state,rc=status)
         _VERIFY(status)
         call enabled_iter%next()
      enddo
   
      ! second communication - created RH
      enabled_iter = this%enabled%begin()
      do while(enabled_iter /= this%enabled%end())
         key=enabled_iter%get()
         call this%connect_client_server(key,rc=status)
         _VERIFY(status)
         call enabled_iter%next()
      enddo
      
         
      _RETURN(_SUCCESS)
   end subroutine initialize

   subroutine connect_client_server(this,collection_name,rc)
      class(IOController), intent(inout) :: this
      character(len=*), intent(in) :: collection_name
      integer, optional, intent(out) :: rc

      type(Client), pointer :: client_ptr
      type(Server), pointer :: server_ptr
      type(ESMF_FieldBundle) :: client_bundle, server_bundle
      type(RHConnector) :: connector
      integer :: status
      
      server_ptr => this%servers%at(collection_name)
      client_ptr => this%clients%at(collection_name)
      client_bundle = client_ptr%get_bundle()
      server_bundle = server_ptr%get_bundle()
      call connector%regrid_store_fieldBundles(client_bundle,server_bundle,rc=status)
      _VERIFY(status)
       

   end subroutine connect_client_server

   subroutine transfer_grids_to_front(this,rc)
      class(IOController), intent(inout) :: this
      integer, optional, intent(out) :: rc

      type(StringVectorIterator) :: enabled_iter
      integer :: status
      character(:), allocatable :: coll_name 
      
      enabled_iter = this%enabled%begin()
      do while(enabled_iter /= this%enabled%end())
         coll_name=enabled_iter%get()
         call this%transfer_grid_to_front(coll_name,rc=status)
         _VERIFY(status)
         call enabled_iter%next()
      enddo
       

   end subroutine transfer_grids_to_front

   subroutine transfer_grid_to_front(this,coll_name,rc)
      class(IOController), intent(inout) :: this
      character(len=*), intent(in) :: coll_name
      integer, optional, intent(out) :: rc

      type(ESMF_Grid) :: front_server_grid
      type(ESMF_Grid) :: client_grid
      type(Client), pointer :: collection_client
      type(Server), pointer :: collection_server
      integer :: status
      integer :: i
      integer, allocatable :: pets(:)

      allocate(pets(this%pet_list(2,2)-this%pet_list(2,1)+1))
      do i=1,size(pets)
         pets(i)=this%pet_list(2,1)+i-1
      enddo

      collection_client =>  this%clients%at(coll_name)
      client_grid = collection_client%get_grid(rc=status)
      _VERIFY(status)

      front_server_grid = transfer_grid_to_pets(client_grid,pets,__RC__)
      collection_server => this%servers%at(coll_name)
      call collection_server%set_grid(front_server_grid,__RC__)

      _RETURN(_SUCCESS)
   end subroutine transfer_grid_to_front

   subroutine transfer_grids_to_back(this,rc)
      class(IOController), intent(inout) :: this
      integer, optional, intent(out) :: rc

      type(StringVectorIterator) :: enabled_iter
      integer :: status
      character(:), allocatable :: coll_name 
      
      enabled_iter = this%enabled%begin()
      do while(enabled_iter /= this%enabled%end())
         coll_name=enabled_iter%get()
         call this%transfer_grid_to_back(coll_name,rc=status)
         _VERIFY(status)
         call enabled_iter%next()
      enddo
       

   end subroutine transfer_grids_to_back

   subroutine transfer_grid_to_back(this,coll_name,rc)
      class(IOController), intent(inout) :: this
      character(len=*), intent(in) :: coll_name
      integer, optional, intent(out) :: rc

      type(ESMF_Grid) :: back_server_grid
      type(ESMF_Grid) :: server_grid
      type(Server), pointer :: collection_server
      integer :: status
      integer :: pet

      pet=this%pet_list(2,1)

      collection_server =>  this%servers%at(coll_name)
      !server_grid = collection_server%get_grid(rc=status)
      !_VERIFY(status)

      !back_server_grid = transfer_grid_to_pets(server_grid,[pet],__RC__)

      _RETURN(_SUCCESS)
   end subroutine transfer_grid_to_back

   function transfer_grid_to_pets(grid,pets,rc) result(redistributed_grid)
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in) :: pets(:)
      integer, optional, intent(out) :: rc

      type(ESMF_Grid) :: redistributed_grid
      integer :: status
      type(ESMF_GridComp) fake_gridcomp
      type(ESMF_VM) :: server_vm,vm
      type(ESMF_DistGrid) :: input_distGrid,output_distgrid,balanced_distGrid
      character(len=ESMF_MAXSTR) :: grid_name
      type(ESMF_State) :: fake_state
      type(ESMF_Field) :: fake_field
      type(ESMF_Grid) :: empty_grid
      integer :: myPet

      call ESMF_VMGetCurrent(vm)
      call ESMF_VMGet(vm,localPet=myPet,rc=status)
      _VERIFY(status)

      fake_gridcomp = ESMF_GridCompCreate(petList=pets,rc=status)
      _VERIFY(status)
      call ESMF_GridCompSetServices(fake_gridcomp,set_services,rc=status)
      _VERIFY(status)
      fake_state=ESMF_StateCreate(__RC__)

      call ESMF_GridCompInitialize(fake_gridcomp,importState=fake_state,phase=1,__RC__)
      call ESMF_StateReconcile(fake_state,__RC__)
      call ESMF_StateGet(fake_state,"empty",fake_field,__RC__)
      call ESMF_FieldGet(fake_field,vm=server_vm,__RC__)

      call ESMF_GridGet(grid,distGrid=input_distGrid,rc=status)
      _VERIFY(status)
      call ESMF_GridGet(grid,name=grid_name,rc=status)
      _VERIFY(status)
      output_distgrid=ESMF_DistGridCreate(input_distGrid,vm=server_vm,rc=status)
      _VERIFY(status)

      empty_grid = ESMF_GridEmptyCreate(vm=server_vm,__RC__)
      call ESMF_GridSet(empty_grid,name="temp_grid",distGrid=output_distgrid,vm=server_vm,__RC__)
      call ESMF_FieldEmptySet(fake_field,grid=empty_grid,vm=server_vm,__RC__)
      call ESMF_GridCompInitialize(fake_gridcomp,importState=fake_state,phase=2,__RC__)

      call ESMF_StateReconcile(fake_state,__RC__)

      call ESMF_FieldGet(fake_field,grid=empty_grid,__RC__)
      call ESMF_GridGet(empty_grid,distGrid=balanced_distGrid,__RC__)
      redistributed_grid = ESMF_GridCreate(grid,balanced_distGrid,copyAttributes=.true.,__RC__)

      call ESMF_StateReconcile(fake_state,__RC__)

      call ESMF_GridCompDestroy(fake_gridcomp,__RC__)
      _RETURN(_SUCCESS)
   end function transfer_grid_to_pets

   subroutine create_comms(this,rc)
      class(IOController), intent(inout) :: this
      integer, optional, intent(out) :: rc
    
      type(ESMF_VM) :: vm
      integer :: status, mpi_comm,rank
      integer :: server_color,front_color,back_color
     
      server_color = MPI_UNDEFINED
      front_color  = MPI_UNDEFINED
      back_color   = MPI_UNDEFINED
      call ESMF_VMGetCurrent(vm,__RC__)
      call ESMF_VMGet(vm,mpiCommunicator=mpi_comm,__RC__)
      call mpi_comm_rank(mpi_comm,rank,status)
      _VERIFY(status)
      if (rank <= this%pet_list(2,2) .and. rank >= this%pet_list(2,1) ) front_color=2
      if (rank <= this%pet_list(3,2) .and. rank >= this%pet_list(3,1) ) back_color=3
      if (rank <= this%pet_list(3,2) .and. rank >= this%pet_list(2,1) ) server_color=1
      

      this%server_comm = MPI_COMM_NULL
      this%front_comm = MPI_COMM_NULL
      this%back_comm = MPI_COMM_NULL

      call mpi_comm_split(mpi_comm,server_color,0,this%server_comm,status)
      _VERIFY(status)
      call mpi_comm_split(mpi_comm,front_color,0,this%front_comm,status)
      _VERIFY(status)
      call mpi_comm_split(mpi_comm,back_color,0,this%back_comm,status)
      _VERIFY(status)

   end subroutine create_comms

end module AEIO_IOController
