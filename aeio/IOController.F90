#include "MAPL_Generic.h"

module AEIO_IOController
   use MPI
   use MAPL_ExceptionHandling
   use MAPL_BaseMod
   use MAPL_StringTemplate
   use ESMF
   use pFIO
   use AEIO_Client
   use AEIO_ClientMap
   use AEIO_Server
   use AEIO_ServerMap
   use yafYaml
   use HistoryConfigMod
   use CollectionMod
   use CollectionRegistryMod
   use GridRegistryMod
   use gFTL_StringVector
   use AEIO_TransferGridComp
   use AEIO_RHConnector
   use AEIO_Writer
   use AEIO_MpiConnection
   use MAPL_Profiler
   use AEIO_IOProfiler
   
   implicit none
   private

   public IOController

   type IOController
      private
      type(ClientMap) :: clients
      type(ServerMap) :: servers
      type(writer) :: writer_comp
      type(StringVector) :: enabled
      type(GridRegistry) :: grids
      type(MpiConnection) :: mpi_connection
   contains
      procedure :: initialize
      procedure :: run
      procedure :: transfer_grid_to_front
      procedure :: transfer_grids_to_front
      !procedure :: transfer_grid_to_back
      !procedure :: transfer_grids_to_back
      procedure :: connect_client_server
      procedure :: transfer_data_client_server
      procedure :: generate_server_writer_prototype
      procedure :: offload_server_data
      procedure :: setup_writers
      procedure :: create_comms
      procedure :: start_writer
      procedure :: stop_writer
      procedure :: generate_kill_message
      procedure :: serialize_coll_information
   end type

contains

   subroutine initialize(this,state,configuration_file,clock,model_pets,writers_per_node,rc)
      class(IOController), intent(inout) :: this
      type(ESMF_State), intent(inout) :: state
      character(len=*), intent(inout) :: configuration_file
      integer, intent(in) :: model_pets
      integer, intent(in) :: writers_per_node
      type(ESMF_Clock), intent(inout) :: clock
      integer, optional, intent(out) :: rc


      type(HistoryConfig) :: hist_config
      integer :: collection_id,ncolls
      type(CollectionRegistry) :: coll_registry
      type(StringVectorIterator) :: enabled_iter
      type(Collection), pointer :: hist_coll
      character(:), allocatable :: key
      type(Server) :: output_server
      type(Client) :: output_client
      type(Client), pointer :: client_ptr
      type(Server), pointer :: server_ptr

      integer :: status

      call this%create_comms(model_pets,writers_per_node,_RC)

      call hist_config%import_yaml_file(configuration_file,_RC)

      ! create client and server for each collection
      this%enabled = hist_config%get_enabled()

      ncolls = this%enabled%size()
      call start_io_prof(MPI_COMM_WORLD,ncolls)
      call io_prof%start('io_initialize')

      coll_registry=hist_config%get_collections()
      this%grids = hist_config%get_grids()
      enabled_iter = this%enabled%begin()
      collection_id = 0
      do while(enabled_iter /= this%enabled%end())
         key=enabled_iter%get()
         hist_coll => coll_registry%at(key)
         call hist_coll%initialize_frequency(clock,_RC)
         collection_id=collection_id+1
         output_server=Server(hist_coll,collection_id,this%mpi_connection,_RC)
         call this%servers%insert(key,output_server)
         output_client=Client(hist_coll,_RC)
         call this%clients%insert(key,output_client)
         call enabled_iter%next()
      enddo

      ! initialize client and server for each collection
      enabled_iter = this%enabled%begin()
      do while(enabled_iter /= this%enabled%end())
         key=enabled_iter%get()
         client_ptr => this%clients%at(key)
         call client_ptr%initialize(state,_RC)
         call enabled_iter%next()
      enddo

      ! first communication - send grid
      call io_prof%start('trans_grid_to_front')       
      call this%transfer_grids_to_front(_RC)
      call io_prof%stop('trans_grid_to_front')       
      !call this%transfer_grids_to_back(_RC)

      ! fill bundle on front end of server for each collection
      enabled_iter = this%enabled%begin()
      do while(enabled_iter /= this%enabled%end())
         key=enabled_iter%get()
         server_ptr => this%servers%at(key)
         call server_ptr%initialize(state,_RC)
         call enabled_iter%next()
      enddo
   
      ! second communication - created RH
      enabled_iter = this%enabled%begin()
      do while(enabled_iter /= this%enabled%end())
         key=enabled_iter%get()
         call io_prof%start('client_server_rh_gen')
         call this%connect_client_server(key,_RC)
         call io_prof%stop('client_server_rh_gen')
         call io_prof%start('server_writer_rh_gen')
         call this%generate_server_writer_prototype(key,_RC)
         call io_prof%stop('server_writer_rh_gen')
         call enabled_iter%next()
      enddo

      ! create writer
      this%writer_comp = writer(this%mpi_connection,_RC)
      enabled_iter = this%enabled%begin()
      do while(enabled_iter /= this%enabled%end())
         key=enabled_iter%get()
         call this%setup_writers(key,_RC)
         call enabled_iter%next()
      enddo
         
      call io_prof%stop('io_initialize')
      _RETURN(_SUCCESS)
   end subroutine initialize

   subroutine start_writer(this,rc)
      class(IOController), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      integer :: writer_comm
      writer_comm = this%mpi_connection%get_back_comm()

      if (writer_comm/=MPI_COMM_NULL) then
         call this%writer_comp%start_writer(_RC)
      end if
      _RETURN(_SUCCESS)
   end subroutine start_writer

   subroutine stop_writer(this,rc)
      class(IOController), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      integer, allocatable :: writer_ranks(:)
      integer :: server_comm!,num_collections
      integer, allocatable :: buffer(:)

      !num_collections = this%enabled%size()
      !allocate(terminate(num_collections),source=-1)
     
      buffer = this%generate_kill_message(_RC) 
      server_comm = this%mpi_connection%get_connection_comm()
      if (server_comm /= MPI_COMM_NULL) then 
         writer_ranks = this%mpi_connection%get_back_mpi_ranks()
         if (this%mpi_connection%am_i_front_root()) then
            !call MPI_Send(terminate,num_collections,MPI_INTEGER,writer_ranks(1), &
                 !writer_ranks(1),server_comm,status)
            call MPI_Send(buffer,size(buffer),MPI_INTEGER,writer_ranks(1), &
                 writer_ranks(1),server_comm,status)
            _VERIFY(status)
         end if
      end if
      _RETURN(_SUCCESS)
   end subroutine stop_writer

   subroutine run(this,clock,rc)
      class(IOController), intent(inout) :: this
      type(ESMF_Clock), intent(in) :: clock
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Time) :: current_time
      integer :: model_comm,front_comm

      front_comm = this%mpi_connection%get_front_comm()
      model_comm = this%mpi_connection%get_model_comm()
      if (model_comm /= MPI_COMM_NULL .or. front_comm /= MPI_COMM_NULL) then
         call io_prof%start('server_run')
         ! here the client might do some work

         ! then we would probably want to check if it is time to write

         ! if time to write
         call io_prof%start('client-server-trans')
         call this%transfer_data_client_server(_RC)
         call io_prof%stop('client-server-trans')

         call io_prof%start('server-writer-trans')
         ! on correct Pets offload data
         call ESMF_ClockGet(clock,currTime=current_time,_RC)
         call this%offload_server_data(current_time,_RC)
         call io_prof%stop('server-writer-trans')
         call io_prof%stop('server_run')
      end if
      _RETURN(_SUCCESS)
   end subroutine run

   subroutine setup_writers(this,collection_name,rc)
      class(IOController), intent(inout) :: this
      character(len=*), intent(in) :: collection_name
      integer, optional, intent(out) :: rc

      type(Server), pointer :: server_ptr
      type(ESMF_FieldBundle) :: bundle
      type(RHConnector) :: rh
      integer :: status

      server_ptr => this%servers%at(collection_name)
      bundle =  server_ptr%get_bundle()
      call server_ptr%get_server_writer_prototype(rh)
      call this%writer_comp%add_collection(collection_name,bundle,rh,_RC)
      _RETURN(_SUCCESS)
      
   end subroutine setup_writers

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
      server_bundle = server_ptr%get_bundle()
      client_bundle = client_ptr%get_bundle()
    
      !call connector%redist_store_fieldBundles(client_bundle,server_bundle,_RC)
      call connector%regrid_store_fieldBundles(client_bundle,server_bundle,_RC)
      call client_ptr%set_client_server_connector(connector)
      call server_ptr%set_client_server_connector(connector)

   end subroutine connect_client_server

   subroutine generate_server_writer_prototype(this,collection_name,rc)
      class(IOController), intent(inout) :: this
      character(len=*), intent(in) :: collection_name
      integer, optional, intent(out) :: rc

      type(Server), pointer :: server_ptr
      type(ESMF_FieldBundle) :: server_bundle
      type(RHConnector) :: connector
      type(ESMF_Grid) :: grid
      type(ESMF_DistGrid) :: dist_grid_in,dist_grid_out
      type(ESMF_Array) :: array_in,array_out
      type(ESMF_DELayout) :: de_layout
      integer :: grid_size(3)
      integer :: status
      integer, allocatable :: back_pets(:)
      character(len=ESMF_MAXSTR), allocatable :: field_names(:)
      integer :: field_count,ub(1),lb(1),i,rank
      type(ESMF_Field) :: field
      !type(ESMF_ArrayBundle) :: array_bundle_in,array_bundle_out

      back_pets = this%mpi_connection%get_back_pets()
     
      ! For now make an array redist for htis 
      server_ptr => this%servers%at(collection_name)
      server_bundle = server_ptr%get_bundle()
      call ESMF_FieldBundleGet(server_bundle,grid=grid,_RC)
      call ESMF_GridGet(grid,distGrid=dist_grid_in,_RC)
      call MAPL_GridGet(grid,globalCellCountPerDim=grid_size,_RC)

      ! create a delayout the "controller" proces on the writer pet
      de_layout=ESMF_DELayoutCreate(deCount=1,petList=[back_pets(1)],_RC)

      ! now create a distgrid on this delayout using the global size of the grid
      dist_grid_out=ESMF_DistGridCreate([1,1],[grid_size(1),grid_size(2)],regDecomp=[1,1],delayout=de_layout,_RC)

      !call ESMF_FieldBundleGet(server_bundle,fieldCount=field_count,_RC)
      !allocate(field_names(field_count))
      !call ESMF_FieldBundleGet(server_bundle,fieldNameList=field_names,_RC)

      !array_bundle_in = ESMF_ArrayBundleCreate(_RC)
      !array_bundle_out = ESMF_ArrayBundleCreate(_RC)

      !do i=1,field_count
         !call ESMF_FieldBundleGet(server_bundle,trim(field_names(i)),field=field,_RC)
         !call ESMF_FieldGet(field,array=array_in,_RC)
         !call ESMF_FieldGet(field,rank=rank,_RC)
         !if (rank==2) then
            !array_out = ESMF_ArrayCreate(dist_grid_out,ESMF_TYPEKIND_R4,name=trim(field_names(i)),_RC)
         !else if (rank==3) then
            !call ESMF_FieldGet(field,ungriddedLBound=lb,ungriddedUBound=ub,_RC)
            !array_out = ESMF_ArrayCreate(dist_grid_out,ESMF_TYPEKIND_R4,name=trim(field_names(i)),undistLBound=lb,undistUBound=ub,_RC)
         !end if
         !call ESMF_ArrayBundleAdd(array_bundle_in,[Array_in],_RC)
         !call ESMF_ArrayBundleAdd(array_bundle_out,[Array_out],_RC)
      !enddo

      array_in = ESMF_ArrayCreate(dist_grid_in,ESMF_TYPEKIND_R4,_RC)
      array_out = ESMF_ArrayCreate(dist_grid_out,ESMF_TYPEKIND_R4,_RC)

      call connector%redist_store_arrays(array_in,array_out,_RC)
      !call connector%redist_store_arraybundles(array_bundle_in,array_bundle_out,_RC)
      call connector%set_sender(.true.)
      call server_ptr%set_server_writer_prototype(connector)
      !do i=1,field_count
         !call ESMF_ArrayBundleGet(array_bundle_out,trim(field_names(i)),array=array_out,_RC)
         !call ESMF_ArrayDestroy(array_out,nogarbage=.true.,_RC)
      !enddo
      !call ESMF_ArrayBundleDestroy(array_bundle_out,nogarbage=.true.,_RC)
      !call ESMF_ArrayBundleDestroy(array_bundle_in,nogarbage=.true.,_RC)
      call ESMF_ArrayDestroy(array_in,nogarbage=.true.,_RC)
      call ESMF_ArrayDestroy(array_out,nogarbage=.true.,_RC)

   end subroutine generate_server_writer_prototype

   subroutine transfer_grids_to_front(this,rc)
      class(IOController), intent(inout) :: this
      integer, optional, intent(out) :: rc

      type(StringVectorIterator) :: enabled_iter
      integer :: status
      character(:), allocatable :: coll_name 
      
      enabled_iter = this%enabled%begin()
      do while(enabled_iter /= this%enabled%end())
         coll_name=enabled_iter%get()
         call this%transfer_grid_to_front(coll_name,_RC)
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
      integer, allocatable :: front_pets(:)
      type(Collection) :: hist_coll
      character(len=:), allocatable :: grid_name

      front_pets = this%mpi_connection%get_front_pets()

      collection_client =>  this%clients%at(coll_name)
      hist_coll = collection_client%get_collection()
      grid_name = hist_coll%get_grid()
      if (trim(grid_name)/="native") then
         front_server_grid = this%grids%make_grid(grid_name,front_pets,_RC) 
      else
         client_grid = collection_client%get_grid(_RC)
         front_server_grid = transfer_grid_to_pets(client_grid,front_pets,_RC)
      end if
      collection_server => this%servers%at(coll_name)
      call collection_server%set_grid(front_server_grid,_RC)

      _RETURN(_SUCCESS)
   end subroutine transfer_grid_to_front

   !subroutine transfer_grids_to_back(this,rc)
      !class(IOController), intent(inout) :: this
      !integer, optional, intent(out) :: rc

      !type(StringVectorIterator) :: enabled_iter
      !integer :: status
      !character(:), allocatable :: coll_name 
      
      !enabled_iter = this%enabled%begin()
      !do while(enabled_iter /= this%enabled%end())
         !coll_name=enabled_iter%get()
         !call this%transfer_grid_to_back(coll_name,_RC)
         !call enabled_iter%next()
      !enddo
       

   !end subroutine transfer_grids_to_back

   !subroutine transfer_grid_to_back(this,coll_name,rc)
      !class(IOController), intent(inout) :: this
      !character(len=*), intent(in) :: coll_name
      !integer, optional, intent(out) :: rc

      !type(ESMF_Grid) :: back_server_grid
      !type(ESMF_Grid) :: server_grid
      !type(Server), pointer :: collection_server
      !integer :: status
      !integer :: pet

      !pet=this%pet_list(2,1)

      !collection_server =>  this%servers%at(coll_name)
      !server_grid = collection_server%get_grid(_RC)

      !back_server_grid = transfer_grid_to_pets(server_grid,[pet],_RC)
      !call collection_server%set_prototype_grid(back_server_grid,_RC)

      !_RETURN(_SUCCESS)
   !end subroutine transfer_grid_to_back

   function transfer_grid_to_pets(grid,pets,rc) result(redistributed_grid)
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in) :: pets(:)
      integer, optional, intent(out) :: rc

      type(ESMF_Grid) :: redistributed_grid
      integer :: status
      type(ESMF_GridComp) fake_gridcomp
      type(ESMF_VM) :: target_vm,vm
      type(ESMF_DistGrid) :: input_distGrid,output_distgrid,balanced_distGrid
      character(len=ESMF_MAXSTR) :: grid_name
      type(ESMF_State) :: fake_state
      type(ESMF_Field) :: fake_field
      type(ESMF_Grid) :: empty_grid
      integer :: myPet

      call ESMF_VMGetCurrent(vm)
      call ESMF_VMGet(vm,localPet=myPet,_RC)

      ! Create a fake grid comp for the sole purpose of getting a VM
      fake_gridcomp = ESMF_GridCompCreate(petList=pets,_RC)
      call ESMF_GridCompSetServices(fake_gridcomp,set_services,_RC)
      fake_state=ESMF_StateCreate(_RC)

      ! Run fake state phase 1 init, which creates an empty field in the
      ! import state so I can get the vm out
      call ESMF_GridCompInitialize(fake_gridcomp,importState=fake_state,phase=1,_RC)
      call ESMF_StateReconcile(fake_state,_RC)
      call ESMF_StateGet(fake_state,"empty",fake_field,_RC)
      call ESMF_FieldGet(fake_field,vm=target_vm,_RC)

      ! Get the dist grid of the input field
      call ESMF_GridGet(grid,distGrid=input_distGrid,_RC)
      call ESMF_GridGet(grid,name=grid_name,_RC)
      ! Now create a new distgrid on the target_vm
      output_distgrid=ESMF_DistGridCreate(input_distGrid,vm=target_vm,_RC)

      ! Now create a grid on the new distgrid on target vm and set in field in import state
      empty_grid = ESMF_GridEmptyCreate(vm=target_vm,_RC)
      call ESMF_GridSet(empty_grid,name="temp_grid",distGrid=output_distgrid,vm=target_vm,_RC)
      call ESMF_FieldEmptySet(fake_field,grid=empty_grid,vm=target_vm,_RC)
      ! Now run phase two, this takes the dist grid I just created
      ! and creates a new distgrid with the balance flag
      ! Create a grid with this distgrid and set in the field in the improt state
      call ESMF_GridCompInitialize(fake_gridcomp,importState=fake_state,phase=2,_RC)

      !reconcile fake_state 
      call ESMF_StateReconcile(fake_state,_RC)

      ! now create a new grid with this so called balanced distgrid from the 
      ! field in the grid comp
      call ESMF_FieldGet(fake_field,grid=empty_grid,_RC)
      call ESMF_GridGet(empty_grid,distGrid=balanced_distGrid,_RC)
      redistributed_grid = ESMF_GridCreate(grid,balanced_distGrid,copyAttributes=.true.,_RC)

      ! finally one more reconcile so that the grid gets reconcile
      ! why can't we reconcile fields and grids individually?
      call ESMF_StateReconcile(fake_state,_RC)

      ! destroy gridcomp, no need for it
      call ESMF_GridCompDestroy(fake_gridcomp,_RC)
      _RETURN(_SUCCESS)
   end function transfer_grid_to_pets

   function transfer_distgrid_to_pets(input_distgrid,pets,rc) result(redistributed_distgrid)
      type(ESMF_DistGrid), intent(in) :: input_distgrid
      integer, intent(in) :: pets(:)
      integer, optional, intent(out) :: rc

      type(ESMF_DistGrid) :: redistributed_distgrid
      integer :: status
      type(ESMF_GridComp) fake_gridcomp
      type(ESMF_VM) :: target_vm,vm
      type(ESMF_DistGrid) :: output_distgrid
      type(ESMF_State) :: fake_state
      type(ESMF_Field) :: fake_field
      type(ESMF_Grid) :: empty_grid
      integer :: myPet

      call ESMF_VMGetCurrent(vm)
      call ESMF_VMGet(vm,localPet=myPet,_RC)

      fake_gridcomp = ESMF_GridCompCreate(petList=pets,_RC)
      call ESMF_GridCompSetServices(fake_gridcomp,set_services,_RC)
      fake_state=ESMF_StateCreate(_RC)

      call ESMF_GridCompInitialize(fake_gridcomp,importState=fake_state,phase=1,_RC)
      call ESMF_StateReconcile(fake_state,_RC)
      call ESMF_StateGet(fake_state,"empty",fake_field,_RC)
      call ESMF_FieldGet(fake_field,vm=target_vm,_RC)

      output_distgrid=ESMF_DistGridCreate(input_distGrid,vm=target_vm,_RC)

      empty_grid = ESMF_GridEmptyCreate(vm=target_vm,_RC)
      call ESMF_GridSet(empty_grid,name="temp_grid",distGrid=output_distgrid,vm=target_vm,_RC)
      call ESMF_FieldEmptySet(fake_field,grid=empty_grid,vm=target_vm,_RC)
      call ESMF_GridCompInitialize(fake_gridcomp,importState=fake_state,phase=2,_RC)

      call ESMF_StateReconcile(fake_state,_RC)

      call ESMF_FieldGet(fake_field,grid=empty_grid,_RC)
      call ESMF_GridGet(empty_grid,distGrid=redistributed_distGrid,_RC)

      call ESMF_GridCompDestroy(fake_gridcomp,_RC)
      _RETURN(_SUCCESS)
   end function transfer_distgrid_to_pets

   subroutine create_comms(this,model_pets,writers_per_node,rc)
      class(IOController), intent(inout) :: this
      integer, intent(in) :: model_pets
      integer, intent(in) :: writers_per_node
      integer, optional, intent(out) :: rc
    
      type(ESMF_VM) :: vm
      integer :: status, mpi_comm
     
      call ESMF_VMGetCurrent(vm,_RC)
      call ESMF_VMGet(vm,mpiCommunicator=mpi_comm,_RC)
     
      this%mpi_connection = MpiConnection(mpi_comm,model_pets,writers_per_node,vm,_RC)

      _RETURN(_SUCCESS)
   end subroutine create_comms

   subroutine transfer_data_client_server(this,rc)
      class(IOController), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(Client), pointer :: client_ptr
      type(Server), pointer :: server_ptr
      type(StringVectorIterator) :: enabled_iter
      character(:), allocatable :: coll_name
      integer :: model_comm,front_comm
      type(Collection) :: hist_coll
      logical :: is_time_to_write
      integer :: collection_id
      character(len=1) :: ic

      model_comm = this%mpi_connection%get_model_comm()
      front_comm = this%mpi_connection%get_front_comm() 

      if (model_comm /= MPI_COMM_NULL .or. front_comm /= MPI_COMM_NULL) then

         call ESMF_VMEpochEnter(epoch=ESMF_VMEPOCH_BUFFER)!,throttle=1)

         enabled_iter = this%enabled%begin()
         collection_id=0
         do while(enabled_iter /= this%enabled%end())
            collection_id=collection_id+1
            write(ic,"(I1)")collection_id
            coll_name=enabled_iter%get()
            server_ptr => this%servers%at(coll_name)
            client_ptr => this%clients%at(coll_name)
            hist_coll = server_ptr%get_collection()
            is_time_to_write = hist_coll%is_time_to_write(_RC)
            if (this%mpi_connection%am_i_front_root() .and. is_time_to_write) write(*,*)"writing coll: ",trim(coll_name)
            if (is_time_to_write) then
               if (model_comm /= MPI_COMM_NULL) then
                  call io_prof%start('data_to_server_'//ic)
                  call client_ptr%transfer_data_to_server(_RC)
                  call io_prof%stop('data_to_server_'//ic)
               end if
               if (front_comm /= MPI_COMM_NULL) then
                  call io_prof%start('data_from_client_'//ic)
                  call server_ptr%get_data_from_client(_RC)
                  call io_prof%stop('data_from_client_'//ic)
               end if
            end if
            call enabled_iter%next()
         enddo

         call ESMF_VMEpochExit (keepAlloc=.false.)
         
      end if

      _RETURN(_SUCCESS)
   end subroutine transfer_data_client_server

   subroutine offload_server_data(this,current_time,rc)
      class(IOController), intent(inout) :: this
      type(ESMF_Time), intent(in) :: current_time
      integer, optional, intent(out) :: rc

      integer :: status,front_comm,connector_comm,num_collections
      type(Server), pointer :: server_ptr
      type(StringVectorIterator) :: enabled_iter
      character(:), allocatable :: coll_name
      integer, allocatable :: back_ranks(:),front_ranks(:),worker_pets(:)
      integer :: i
      logical, allocatable :: writing(:)
      integer :: MPI_STAT(MPI_STATUS_SIZE)
      type(Collection) :: hist_coll
      character(len=1) :: ic
      integer, allocatable :: buffer(:)

      front_comm = this%mpi_connection%get_front_comm()
      connector_comm = this%mpi_connection%get_connection_comm()
      front_ranks = this%mpi_connection%get_front_mpi_ranks()
      back_ranks = this%mpi_connection%get_back_mpi_ranks()
      num_collections = this%enabled%size()
      allocate(worker_pets(num_collections))

      if (front_comm/=MPI_COMM_NULL) then
         allocate(writing(num_collections),source=.false.)
         enabled_iter = this%enabled%begin()
         i=0
         do while(enabled_iter /= this%enabled%end())
            i=i+1
            coll_name = enabled_iter%get()
            server_ptr => this%servers%at(coll_name)
            hist_coll = server_ptr%get_collection()
            if (hist_coll%is_time_to_write()) writing(i)=.true.
            call enabled_iter%next()
         enddo

         if (any(writing)) then
            if (this%mpi_connection%am_i_front_root()) then
               buffer = this%serialize_coll_information(writing,current_time,_RC)
               !call MPI_Send(writing,num_collections,MPI_INTEGER,back_ranks(1), &
                   !back_ranks(1),connector_comm,status)
               call MPI_Send(buffer,size(buffer),MPI_INTEGER,back_ranks(1), &
                   back_ranks(1),connector_comm,status)
               _VERIFY(status)
               call MPI_Recv(worker_pets,num_collections,MPI_INTEGER,back_ranks(1), &
                    front_ranks(1),connector_comm,MPI_STAT,status)
               _VERIFY(status)
            end if
            call MPI_Bcast(worker_pets,num_collections,MPI_INTEGER,front_ranks(1),front_comm,status)
            _VERIFY(status)
         end if

         enabled_iter = this%enabled%begin()
         i = 0
         do while(enabled_iter /= this%enabled%end())
            i=i+1
            if (writing(i)) then
               write(ic,"(I1)")i
               coll_name=enabled_iter%get()
               server_ptr => this%servers%at(coll_name)
               !call server_ptr%get_writer(_RC)
               call io_prof%start('transfer_rh_'//ic) 
               call server_ptr%create_rh_from_proto(worker_pets(i))
               call io_prof%stop('transfer_rh_'//ic) 
            end if
            call enabled_iter%next()
         enddo
         ! second round enter epoch
         call ESMF_VMEpochEnter(epoch=ESMF_VMEPOCH_BUFFER,keepAlloc=.false.)!,throttle=1)
         !call ESMF_VMEpochEnter(epoch=ESMF_VMEPOCH_BUFFER)!,throttle=1)
         enabled_iter = this%enabled%begin()
         i=0
         do while(enabled_iter /= this%enabled%end())
            i=i+1
            if (writing(i)) then
               write(ic,"(I1)")i
               coll_name=enabled_iter%get()
               server_ptr => this%servers%at(coll_name)
               hist_coll = server_ptr%get_collection()
               call io_prof%start('offload_data_'//ic)
               call server_ptr%offload_data(_RC)
               call io_prof%stop('offload_data_'//ic)
            end if
            call enabled_iter%next()
         enddo
         call ESMF_VMEpochExit( keepAlloc=.false.)
      end if

      _RETURN(_SUCCESS)
   end subroutine offload_server_data

   function serialize_coll_information(this,writing,current_time,rc) result(buffer)
      class(IOController), intent(inout) :: this
      logical, intent(in) :: writing(:)
      type(ESMF_Time), intent(in) :: current_time
      integer, optional, intent(out) :: rc

      integer, allocatable :: buffer(:)

      character(len=:), allocatable :: filename
      character(len=:), allocatable :: file_template
      integer :: collection_id,num_writing,i
      type(Server), pointer :: server_ptr
      type(StringVectorIterator) :: enabled_iter
      type(Collection) :: hist_coll
      character(:), allocatable :: coll_name
      integer :: status

      num_writing = count(writing)
      buffer = [serialize_intrinsic(num_writing)]
      
      enabled_iter = this%enabled%begin()
      i = 0
      do while(enabled_iter /= this%enabled%end())
         i=i+1
         if (writing(i)) then
            coll_name=enabled_iter%get()
            server_ptr => this%servers%at(coll_name)
            hist_coll = server_ptr%get_collection()
            file_template = hist_coll%get_file_template()
            filename = create_filename_from_template(file_template,time=current_time,_RC)
            filename = trim(coll_name)//"_"//filename 
            collection_id=i
            buffer = [buffer,serialize_intrinsic(collection_id)]
            buffer = [buffer,serialize_intrinsic(filename)]
         end if
         call enabled_iter%next()
      enddo
      _RETURN(_SUCCESS)

   end function serialize_coll_information

   function generate_kill_message(this,rc) result(buffer)
      class(IOController), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer, allocatable :: buffer(:)

      character(len=:), allocatable :: filename
      integer :: status

      buffer = [serialize_intrinsic(1)]
      filename = "dummy"
      buffer = [buffer,serialize_intrinsic(-1)]
      buffer = [buffer,serialize_intrinsic(filename)]
      _RETURN(_SUCCESS)

   end function generate_kill_message
 
end module AEIO_IOController
