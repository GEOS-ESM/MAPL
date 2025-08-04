#include "MAPL.h"

! The derived type "MaplFramework" is intended to encapsulate all of the singletons used within MAPL-based
! codes.   This limits the scope of the singleton "sin", which will allow proper object passing
! at some later date if justified.


module mapl3g_MaplFramework
   use mapl_ErrorHandling
   use mapl_KeywordEnforcerMod
   use mapl_profiler, only: DistributedProfiler
   use pfio_DirectoryServiceMod, only: DirectoryService
   use pfio_ClientManagerMod
   use pfio_MpiServerMod, only: MpiServer
   use pfio_ClientThreadMod, only: ClientThread
   use pfio_AbstractDirectoryServiceMod, only: PortInfo
   use udunits2f, only: UDUNITS_Initialize => Initialize
   use pflogger, only: logging
   use pflogger, only: Logger
   use mpi
   use esmf
   implicit none
   private

   public :: MaplFramework
   public :: MAPL_initialize
   public :: MAPL_finalize
   public :: MAPL_Get

   type :: MaplFramework
      private
      logical :: mapl_initialized = .false.
      logical :: esmf_internally_initialized = .false.
      type(ESMF_VM) :: mapl_vm
      integer :: model_comm

      type(ESMF_HConfig) :: mapl_hconfig
      type(DirectoryService) :: directory_service
      type(MpiServer), pointer :: o_server => null()
      type(MpiServer), pointer :: i_server => null()
      type(DistributedProfiler) :: time_profiler
   contains
      procedure :: initialize
      procedure :: initialize_esmf
#ifdef BUILD_WITH_PFLOGGER
      procedure :: initialize_pflogger
#endif
      procedure :: initialize_profilers
      procedure :: initialize_udunits
      procedure :: initialize_servers
      procedure :: initialize_simple_servers

      procedure :: finalize
      procedure :: finalize_servers
      procedure :: finalize_profiler
      procedure :: finalize_pflogger
      procedure :: finalize_esmf
      procedure :: get
      procedure :: is_initialized
   end type MaplFramework

   ! Private singleton object.  Used
   type(MaplFramework), target :: the_mapl_object

   interface MAPL_Get
      procedure :: mapl_get
      procedure :: mapl_get_mapl
   end interface MAPL_Get

   interface MAPL_Initialize
      procedure :: mapl_initialize
   end interface MAPL_Initialize

contains

   ! Type-bound procedures

   ! Note: HConfig is an output if ESMF is not already initialized.  Otherwise it is an input.
   subroutine initialize(this, hconfig, unusable, is_model_pet, servers, mpiCommunicator, level_name, configFilenameFromArgNum, rc)
      class(MaplFramework), intent(inout) :: this
      type(ESMF_HConfig), optional, intent(inout) :: hconfig
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: is_model_pet
      type(ESMF_GridComp), optional, allocatable, intent(out) :: servers(:)
      integer, optional, intent(in) :: mpiCommunicator
      character(*), optional, intent(in) :: level_name
      integer, optional, intent(in) :: configFilenameFromArgNum
      integer, optional, intent(out) :: rc

      integer :: status


      _ASSERT(.not. this%mapl_initialized, "MaplFramework object is already initialized")
      this%mapl_initialized = .true.

      if (present(hconfig)) this%mapl_hconfig = hconfig

      call this%initialize_esmf(hconfig=hconfig, mpiCommunicator=mpiCommunicator, configFilenameFromArgNum=configFilenameFromArgNum, _RC)
      call ESMF_VMGetCurrent(this%mapl_vm, _RC)

#ifdef BUILD_WITH_PFLOGGER
      call this%initialize_pflogger(level_name=level_name, _RC)
#endif
      call this%initialize_profilers(_RC)
      call this%initialize_servers(is_model_pet=is_model_pet, servers=servers, _RC)
      call this%initialize_udunits(_RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine initialize

   ! If ESMF is already initialized, then we expect hconfig to be
   ! externally provided.  Otherwise, we retrieve the top level
   ! hconfig from ESMF_Initialize and return that.
   subroutine initialize_esmf(this, hconfig, unusable, mpiCommunicator, configFilenameFromArgNum, rc)
      class(MaplFramework), intent(inout) :: this
      type(ESMF_HConfig), optional, intent(inout) :: hconfig
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: mpiCommunicator
      integer, optional, intent(in) :: configFilenameFromArgNum
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Config) :: config
      logical :: esmf_is_initialized
      integer :: argNum
      

      esmf_is_initialized = ESMF_IsInitialized(_RC)
      _RETURN_IF(esmf_is_initialized)

      this%esmf_internally_initialized = .true.

      argNum = 0
      if (present(configFilenameFromArgNum)) argNum = configFilenameFromArgNum

      if (argNum > 0) then
         call ESMF_Initialize(configFilenameFromArgNum=argNum, configKey=['esmf'], config=config, &
              defaultDefaultCalKind=ESMF_CALKIND_GREGORIAN, &
              mpiCommunicator=mpiCommunicator, _RC)
         call ESMF_ConfigGet(config, hconfig=hconfig, _RC)
         this%mapl_hconfig = get_subconfig(hconfig, keystring='mapl', _RC)
      else
         call ESMF_Initialize(mpiCommunicator=mpiCommunicator, defaultDefaultCalKind=ESMF_CALKIND_GREGORIAN, _RC)
         this%mapl_hconfig = ESMF_HConfigCreate(content='{}', _RC)
      end if


      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   contains

      ! Return an empty mapping unless named dictionary is found.
      function get_subconfig(hconfig, keystring, rc) result(subcfg)
         type(ESMF_HConfig) :: subcfg
         type(ESMF_HConfig), intent(in) :: hconfig
         character(*), intent(in) :: keystring
         integer, optional, intent(out) :: rc

         integer :: status
         logical :: has_keystring

         has_keystring = ESMF_HConfigIsDefined(hconfig, keystring=keystring, _RC)
         if (has_keystring) then
            subcfg = ESMF_HConfigCreateAt(hconfig, keystring='mapl', _RC)
            _RETURN(_SUCCESS)
         end if

         subcfg = ESMF_HConfigCreate(content='{}', _RC)
         _RETURN(_SUCCESS)
      end function get_subconfig

   end subroutine initialize_esmf

#ifdef BUILD_WITH_PFLOGGER
   subroutine initialize_pflogger(this, unusable, level_name, rc)
      use PFL_Formatter, only: get_sim_time
      use pflogger, only: pfl_initialize => initialize
      use mapl_SimulationTime, only: fill_time_dict

      class(MaplFramework), intent(inout) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(*), optional, intent(in) :: level_name
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: world_comm
      logical :: has_pflogger_cfg_file
      character(:), allocatable :: pflogger_cfg_file

      call pfl_initialize()
      get_sim_time => fill_time_dict

      has_pflogger_cfg_file = ESMF_HConfigIsDefined(this%mapl_hconfig, keystring="pflogger_cfg_file", _RC)
      if (has_pflogger_cfg_file) then
         pflogger_cfg_file = ESMF_HConfigAsString(this%mapl_hconfig, keystring="pflogger_cfg_file", _RC)
         call logging%load_file(pflogger_cfg_file)
         _RETURN(_SUCCESS)
      end if

      call ESMF_VMGet(this%mapl_vm, mpiCommunicator=world_comm, _RC)
      call default_initialize_pflogger(world_comm=world_comm, level_name=level_name, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine initialize_pflogger
#endif


   subroutine initialize_profilers(this, unusable, rc)
      class(MaplFramework), target, intent(inout) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: world_comm
      call ESMF_VMGet(this%mapl_vm, mpiCommunicator=world_comm, _RC)
!#      call initialize_profiler(comm=world_comm, enable_global_timeprof=enable_global_timeprof, &
!#      enable_global_memprof=enable_global_memprof, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine initialize_profilers

   subroutine initialize_servers(this, unusable, is_model_pet, servers, rc)
      class(MaplFramework), target, intent(inout) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: is_model_pet
      type(ESMF_GridComp), allocatable, optional, intent(out) :: servers(:)
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_HConfig) :: servers_hconfig
      logical :: has_server_section
      integer :: model_petcount
      integer :: world_group, model_group, server_group, model_server_group
      integer :: world_comm, server_comm, model_server_comm
      integer :: ssiCount ! total number of nodes participating
      integer, allocatable :: ssiMap(:)
      integer, allocatable :: model_pets(:), server_pets(:), model_server_pets(:)
      integer, allocatable :: ssis_per_server(:)
      integer :: required_ssis
      integer :: num_model_ssis
      type(ESMF_HConfig), allocatable :: server_hconfigs(:)
      integer :: n
      integer :: ssi_0, ssi_1, i_server
      class(Logger), pointer :: lgr
      integer :: ignore ! workaround for ESMF bug in  v8.6.0

      call ESMF_VMGet(this%mapl_vm, ssiMap=ssiMap, ssiCount=ssiCount, mpiCommunicator=world_comm, petCount=ignore, _RC)
      call MPI_Comm_group(world_comm, world_group, _IERROR)
      model_petCount = get_model_petcount(this%mapl_vm, this%mapl_hconfig, _RC)

      has_server_section = ESMF_HConfigIsDefined(this%mapl_hconfig, keystring='servers', _RC)
      if (.not. has_server_section) then
         ! Should only run on model PETs
         call MPI_Group_range_incl(world_group, 1, reshape([0, model_petCount-1, 1], [3,1]), model_group, _IERROR)
         call MPI_Comm_create_group(world_comm, model_group, 0, this%model_comm, _IERROR)
         call MPI_Group_free(model_group, _IERROR)
         if (present(is_model_pet)) then
            is_model_pet = (this%model_comm /= MPI_COMM_NULL)
         end if
         _RETURN_IF(this%model_comm == MPI_COMM_NULL)
         this%directory_service = DirectoryService(this%model_comm)
         call this%initialize_simple_servers(_RC)
         _RETURN(_SUCCESS)
      end if

      if (.not. present(servers)) then
         _RETURN(_SUCCESS)
      end if

      num_model_ssis = get_num_ssis(model_petCount, ssiCount, ssiMap, ssiOffset=0, _RC)

      servers_hconfig = ESMF_HConfigCreateAt(this%mapl_hconfig, keystring='servers', _RC)
      server_hconfigs = get_server_hconfigs(servers_hconfig, _RC)

      ssis_per_server = get_ssis_per_server(server_hconfigs, _RC)
      required_ssis =  num_model_ssis + sum(ssis_per_server)

      _ASSERT(required_ssis <= ssiCount, "Insufficient resources for requested servers.")
      if (required_ssis < ssiCount) then
         call lgr%warning("Unused nodes.  Required %i0 nodes, but %i0 available.", required_ssis, ssicount)
      end if

      model_pets = pack([(n, n = 0, size(ssiMap)-1)], ssiMap <= num_model_ssis)
      call MPI_Group_incl(world_group, model_petCount, model_pets, model_group, _IERROR)
      call MPI_Comm_create_group(world_comm, model_group, 0, this%model_comm, _IERROR)
      is_model_pet = (this%model_comm /= MPI_COMM_NULL)


      ssi_0 = num_model_ssis
      allocate(servers(size(server_hconfigs)))
      do i_server = 1, size(server_hconfigs)
         ssi_1 = ssi_0 + ssis_per_server(i_server)
         server_pets = pack([(n, n = 0, size(ssiMap)-1)], ssiMap >= ssi_0 .and. ssiMap < ssi_1)

         call MPI_Group_incl(world_group, size(server_pets), server_pets, server_group, _IERROR)
         call MPI_Group_union(server_group, model_group, model_server_group, _IERROR)

         call MPI_Comm_create_group(world_comm, server_group, 0, server_comm, _IERROR)
         call MPI_Comm_create_group(world_comm, model_server_group, 0, model_server_comm, _IERROR)

         call MPI_Group_Free(model_group, _IERROR)
         call MPI_Group_Free(server_group, _IERROR)
         call MPI_Group_Free(model_server_group, _IERROR)

         model_server_pets = pack([(n, n = 0, size(ssiMap-1))], (model_server_comm /= MPI_COMM_NULL))
         servers(i_server) = make_server_gridcomp(server_hconfigs(i_server), model_server_pets, [model_server_comm, this%model_comm, server_comm], _RC)

         ssi_0 = ssi_1
      end do

      call MPI_Group_Free(world_group, _IERROR)
      call ESMF_HConfigDestroy(servers_hconfig, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine initialize_servers

   function make_server_gridcomp(hconfig, petList, comms, rc) result(gridcomp)
      use mapl_DSO_Utilities
      type(ESMF_GridComp) :: gridcomp
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, intent(in) :: petList(:)
      integer, intent(in) :: comms(3) ! world, model, server
      integer, optional, intent(out) :: rc

      integer :: status, user_status
      type(ESMF_HConfig) :: server_hconfig, comms_hconfig
      character(:), allocatable :: sharedObj
      character(:), allocatable :: userRoutine

      server_hconfig = ESMF_HConfigCreateAt(hconfig, _RC)
      comms_hconfig = ESMF_HConfigCreate(content='{}', _RC)
      call ESMF_HConfigAdd(comms_hconfig, comms(1), addKeyString='world_comm', _RC)
      call ESMF_HConfigAdd(comms_hconfig, comms(2), addKeyString='model_comm', _RC)
      call ESMF_HConfigAdd(comms_hconfig, comms(3), addKeyString='server_comm', _RC)
      call ESMF_HConfigAdd(server_hconfig, comms_hconfig, addKeyString='comms', _RC)

      gridcomp = ESMF_GridCompCreate(petList=petList, _RC)
      sharedObj = ESMF_HConfigAsString(server_hconfig, keystring='sharedOb', _RC)
      userRoutine = ESMF_HConfigAsString(server_hconfig, keystring='userRoutine', _RC)
      call ESMF_GridCompSetServices(gridcomp, sharedObj=adjust_dso_name(sharedObj), userRoutine=userRoutine, _USERRC)

      call ESMF_HConfigDestroy(comms_hconfig, _RC)
      call ESMF_HConfigDestroy(server_hconfig, _RC)

      _RETURN(_SUCCESS)
   end function make_server_gridcomp

   function get_server_hconfigs(servers_hconfig, rc) result(server_hconfigs)
      type(ESMF_HConfig), allocatable :: server_hconfigs(:)
      type(ESMF_HConfig), intent(in) :: servers_hconfig
      integer, optional, intent(out) :: rc

      integer :: status

      integer :: n_servers, i_server
      type(ESMF_HConfigIter) :: iter_begin, iter_end, iter

      n_servers = ESMF_HConfigGetSize(servers_hconfig, _RC)
      allocate(server_hconfigs(n_servers))

      iter_begin = ESMF_HConfigIterBegin(servers_hconfig,_RC)
      iter_end = ESMF_HConfigIterEnd(servers_hconfig, _RC)
      iter = iter_begin

      i_server = 0
      do while (ESMF_HConfigIterLoop(iter, iter_begin, iter_end, rc=status))
         i_server = i_server + 1
         ! server_hconfigs(i_server) = ESMF_HConfigCreateAtMapVal(iter, _RC)
         server_hconfigs(i_server) = ESMF_HConfigCreateAt(iter, _RC)
      end do

      _RETURN(_SUCCESS)
   end function get_server_hconfigs

   function get_ssis_per_server(server_hconfigs, rc) result(ssis_per_server)
      integer, allocatable :: ssis_per_server(:)
      type(ESMF_HConfig), intent(in) :: server_hconfigs(:)
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i_server

      associate (n_servers => size(server_hconfigs))
        allocate(ssis_per_server(n_servers))
        do i_server = 1, n_servers
           ssis_per_server(i_server) = ESMF_HConfigAsI4(server_hconfigs(i_server), keystring='num_nodes', _RC)
        end do
      end associate
      _RETURN(_SUCCESS)
   end function get_ssis_per_server


   integer function get_model_petCount(vm, hconfig, rc) result(model_petCount)
      type(ESMF_VM), intent(in) :: vm
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: has_model_petcount

      call ESMF_VMGet(vm, petcount=model_petCount, _RC)

      has_model_petcount = ESMF_HConfigIsDefined(hconfig, keystring='model_petcount', _RC)
      if (has_model_petcount) then
         model_petcount = ESMF_HConfigAsI4(hconfig, keystring='model_petcount', _RC)
      end if

      _RETURN(_SUCCESS)
   end function get_model_petCount

   subroutine initialize_simple_servers(this, unusable, rc)
      class(MaplFramework), target, intent(inout) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status, stat_alloc
      type(ClientThread), pointer :: clientPtr

      call init_IO_ClientManager(this%model_comm, _RC)

      ! o server
      allocate(this%o_server, source=MpiServer(this%model_comm, 'o_server', rc=status), stat=stat_alloc)
      _VERIFY(status)
      _VERIFY(stat_alloc)
      call this%directory_service%publish(PortInfo('o_server', this%o_server), this%o_server)
      clientPtr => o_Clients%current()
      call this%directory_service%connect_to_server('o_server', clientPtr, this%model_comm)

      ! i server
      allocate(this%i_server, source=MpiServer(this%model_comm, 'i_server', rc=status), stat=stat_alloc)
      _VERIFY(status)
      _VERIFY(stat_alloc)
      call this%directory_service%publish(PortInfo('i_server', this%i_server), this%i_server)
      clientPtr => i_Clients%current()
      call this%directory_service%connect_to_server('i_server', clientPtr, this%model_comm)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine initialize_simple_servers

   subroutine get(this, unusable, directory_service, rc)
      class(MaplFramework), target, intent(in) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(DirectoryService), pointer, optional, intent(out) :: directory_service
      integer, optional, intent(out) :: rc

      _ASSERT(this%is_initialized(), "MaplFramework object is not initialized")
      if (present(directory_service)) directory_service => this%directory_service

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine get

   logical function is_initialized(this)
      class(MaplFramework), intent(in) :: this
      is_initialized = this%mapl_initialized
   end function is_initialized

   subroutine finalize(this, unusable, rc)
      class(MaplFramework), intent(inout) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status

      if (this%model_comm /= MPI_COMM_NULL) then
         call this%directory_service%free_directory_resources()
         call MPI_Comm_free(this%model_comm, _IERROR)
      end if
      call this%finalize_servers(_RC)
!#         call server_comm%free_comms(_RC)
!#         if (server_comm /= MPI_COMM_NULL) then
!#            call MPI_Comm_free(server_comm, _IERROR)
!#         end if
!#         if (server_comm_model /= MPI_COMM_NULL) then
!#            call MPI_Comm_free(server_comm_model, _IERROR)
!#         end if

      call this%finalize_profiler(_RC)
      call this%finalize_pflogger(_RC)
      call this%finalize_esmf(_RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine finalize

   subroutine finalize_servers(this, unusable, rc)
      class(MaplFramework), intent(inout) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(this)
   end subroutine finalize_servers

   subroutine finalize_profiler(this, unusable, rc)
      class(MaplFramework), intent(inout) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(this)
   end subroutine finalize_profiler

   subroutine finalize_pflogger(this, unusable, rc)
      class(MaplFramework), intent(inout) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      call logging%free()

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(this)
   end subroutine finalize_pflogger

   subroutine finalize_esmf(this, unusable, rc)
      class(MaplFramework), intent(inout) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status

      _RETURN_UNLESS(this%esmf_internally_initialized)

      call ESMF_HConfigDestroy(this%mapl_hconfig, _RC)
      call ESMF_Finalize(_RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine finalize_esmf

   ! Public interfaces that rely on the singleton object
   subroutine mapl_get(unusable, directory_service, rc)
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(DirectoryService), pointer, optional, intent(out) :: directory_service
      integer, optional, intent(out) :: rc

      integer :: status

      call the_mapl_object%get(directory_service=directory_service, _RC)

      _RETURN(_SUCCESS)
       _UNUSED_DUMMY(unusable)
  end subroutine mapl_get

   subroutine mapl_get_mapl(mapl)
      type(MaplFramework), pointer, intent(out) :: mapl

      mapl => the_mapl_object
   end subroutine mapl_get_mapl


   subroutine mapl_initialize(hconfig, unusable, is_model_pet, servers, mpiCommunicator, configFilenameFromArgNum, level_name, rc)
      type(ESMF_HConfig), optional, intent(inout) :: hconfig
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: is_model_pet
      type(ESMF_GridComp), allocatable, optional, intent(out) :: servers(:)
      integer, optional, intent(in) :: mpiCommunicator
      integer, optional, intent(in) :: configFilenameFromArgNum
      character(*), optional, intent(in) :: level_name
      integer, optional, intent(out) :: rc

      integer :: status

      call the_mapl_object%initialize(hconfig=hconfig, is_model_pet=is_model_pet, servers=servers, mpiCommunicator=mpiCommunicator, &
           configFilenameFromArgNum=configFilenameFromArgNum, level_name=level_name, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine mapl_initialize

   subroutine mapl_finalize(rc)
      integer, optional, intent(out) :: rc

      integer :: status

      call the_mapl_object%finalize(_RC)

      _RETURN(_SUCCESS)
   end subroutine mapl_finalize

#ifdef BUILD_WITH_PFLOGGER
   subroutine default_initialize_pflogger(world_comm, unusable, level_name, rc)
      use pflogger, only: StreamHandler, FileHandler, HandlerVector
      use pflogger, only: MpiLock, MpiFormatter
      use pflogger, only: INFO, WARNING, name_to_level

      use, intrinsic :: iso_fortran_env, only: OUTPUT_UNIT

      integer, intent(in) :: world_comm
      class (KeywordEnforcer), optional, intent(in) :: unusable
      character(*), optional, intent(in) :: level_name
      integer, optional, intent(out) :: rc

      type (HandlerVector) :: handlers
      type (StreamHandler) :: console
      type (FileHandler) :: file_handler
      integer :: level,rank,status
      type(Logger), pointer :: lgr
      character(:), allocatable :: level_name_

      ! Default configuration if no file provided

      level_name_ = 'INFO'
      if (present(level_name)) level_name_ = level_name

      call MPI_COMM_Rank(world_comm,rank,status)
      level = WARNING ! except on root
      if (rank == 0) then
         level = name_to_level(level_name_)
      end if

      console = StreamHandler(OUTPUT_UNIT)
      call console%set_level(level)
      call console%set_formatter(MpiFormatter(world_comm, fmt='%(name)a15~: %(message)a'))
      call handlers%push_back(console)

      file_handler = FileHandler('warnings_and_errors.log')
      call file_handler%set_level(WARNING)
      call file_handler%set_formatter(MpiFormatter(world_comm, fmt='pe=%(mpi_rank)i5.5~: %(name)a~: %(message)a'))
      call file_handler%set_lock(MpiLock(world_comm))
      call handlers%push_back(file_handler)

      call logging%basic_config(level=level, handlers=handlers, rc=status)
      _VERIFY(status)

      if (rank == 0) then
         lgr => logging%get_logger('mapl')
         call lgr%info('No configure file specified for logging layer.  Using defaults.')
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine default_initialize_pflogger
#endif


   integer function get_num_ssis(petCount, ssiCount, ssiMap, ssiOffset, rc) result(num_ssis)
      integer, intent(in) :: petCount
      integer, intent(in) :: ssiCount
      integer, intent(in) :: ssiMap(:)
      integer, intent(in) :: ssiOffset
      integer, optional, intent(out) :: rc

      integer :: n
      integer :: found

      num_ssis = 0

      found = 0
      do n = ssiOffset, ssiCount - 1
         found = found + count(ssiMap == n)
         if (found >= petCount) exit
      end do

      _ASSERT(found >= petCount, 'Insufficient resources for running model.')
      num_ssis = 1 + (n - ssiOffset)

      _RETURN(_SUCCESS)
   end function get_num_ssis

   subroutine initialize_udunits(this, rc)
      class(MaplFramework), intent(in) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      call UDUNITS_Initialize(_RC)

      _RETURN(_SUCCESS)
   end subroutine initialize_udunits

end module mapl3g_MaplFramework

