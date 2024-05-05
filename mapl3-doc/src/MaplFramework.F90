#include "MAPL_Generic.h"

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
   use pflogger, only: logging
   use pflogger, only: Logger
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
      type(ESMF_HConfig) :: mapl_hconfig
      type(DirectoryService) :: directory_service
      type(MpiServer), pointer :: o_server => null()
      type(DistributedProfiler) :: time_profiler
   contains
      procedure :: initialize
      procedure :: initialize_esmf
      procedure :: initialize_mapl
      procedure :: initialize_simple_oserver
      procedure :: finalize
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
   subroutine initialize(this, hconfig, unusable, mpiCommunicator, rc)
      class(MaplFramework), intent(inout) :: this
      type(ESMF_HConfig), intent(inout) :: hconfig
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: mpiCommunicator
      integer, optional, intent(out) :: rc

      integer :: status

      _ASSERT(.not. this%mapl_initialized, "MaplFramework object is already initialized")
      this%mapl_hconfig = hconfig

      call this%initialize_esmf(hconfig, mpiCommunicator=mpiCommunicator, _RC)

      call this%initialize_mapl(_RC)
      this%mapl_initialized = .true.

      _RETURN(_SUCCESS)
   end subroutine initialize

   subroutine initialize_esmf(this, hconfig, unusable, mpiCommunicator, rc)
      class(MaplFramework), intent(inout) :: this
      type(ESMF_HConfig), intent(inout) :: hconfig
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: mpiCommunicator
      integer, optional, intent(out) :: rc
      
      integer :: status
      type(ESMF_Config) :: config
      logical :: esmf_is_initialized
      logical :: has_mapl_section

      esmf_is_initialized = ESMF_IsInitialized(_RC)
      _RETURN_IF(esmf_is_initialized)

      this%esmf_internally_initialized = .true.
      call ESMF_Initialize(configFilenameFromArgNum=1, configKey=['esmf'], config=config, mpiCommunicator=mpiCommunicator, _RC)

      ! If ESMF is externally initialized, then we expect the mapl hconfig to be passed in.   Otherwise, it
      ! must be extracted from the top level ESMF Config.
      
      call ESMF_ConfigGet(config, hconfig=hconfig, _RC)
      has_mapl_section = ESMF_HConfigIsDefined(hconfig, keystring='mapl', _RC)
      if (has_mapl_section) then
         this%mapl_hconfig = ESMF_HConfigCreateAt(hconfig, keystring='mapl', _RC)
         _RETURN(_SUCCESS)
      end if

      this%mapl_hconfig = ESMF_HConfigCreate(content='{}', _RC)

      _RETURN(_SUCCESS)
   end subroutine initialize_esmf

   subroutine initialize_mapl(this, unusable, rc)
      class(MaplFramework), intent(inout) :: this
      class(KeywordEnforcer), optional, intent(out) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: comm_world
      type(ESMF_VM) :: mapl_vm
      logical :: has_pflogger_cfg_file
      character(:), allocatable :: pflogger_cfg_file

      call ESMF_VMGetCurrent(mapl_vm, _RC)
      call ESMF_VMGet(mapl_vm, mpiCommunicator=comm_world, _RC)

#ifdef BUILD_WITH_PFLOGGER
      has_pflogger_cfg_file = ESMF_HConfigIsDefined(this%mapl_hconfig, keystring="pflogger_cfg_file", _RC)
      if (has_pflogger_cfg_file) then
         pflogger_cfg_file = ESMF_HConfigAsString(this%mapl_hconfig, keystring="pflogger_cfg_file", _RC)
      end if
      call initialize_pflogger(pflogger_cfg_file=pflogger_cfg_file, comm_world=comm_world, _RC)
#endif
!#      call initialize_profiler(comm=comm_world, enable_global_timeprof=enable_global_timeprof, enable_global_memprof=enable_global_memprof, _RC)

      call this%initialize_simple_oserver(_RC)

      _RETURN(_SUCCESS)
   end subroutine initialize_mapl

  subroutine initialize_simple_oserver(this, unusable, rc)
      class(MaplFramework), target, intent(inout) :: this
      class(KeywordEnforcer), optional, intent(out) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status, stat_alloc, comm_world
      type(ESMF_VM) :: vm
      type(ClientThread), pointer :: clientPtr

      call ESMF_VMGetCurrent(vm, _RC)
      call ESMF_VMGet(vm, mpiCommunicator=comm_world, _RC)

      this%directory_service = DirectoryService(comm_world)
      call init_IO_ClientManager(comm_world, _RC)
      allocate(this%o_server, source=MpiServer(comm_world, 'o_server', rc=status), stat=stat_alloc)
      _VERIFY(status)
      _VERIFY(stat_alloc)
      call this%directory_service%publish(PortInfo('o_server', this%o_server), this%o_server)
      clientPtr => o_Clients%current()
      call this%directory_service%connect_to_server('o_server', clientPtr, comm_world)
 
      _RETURN(_SUCCESS)

   end subroutine initialize_simple_oserver
 
   subroutine get(this, unusable, directory_service, rc)
      class(MaplFramework), target, intent(in) :: this
      class(KeywordEnforcer), optional, intent(out) :: unusable
      type(DirectoryService), pointer, optional, intent(out) :: directory_service
      integer, optional, intent(out) :: rc

      integer :: status

      _ASSERT(this%is_initialized(), "MaplFramework object is not initialized")
      if (present(directory_service)) directory_service => this%directory_service

      _RETURN(_SUCCESS)
   end subroutine get

   logical function is_initialized(this)
      class(MaplFramework), intent(in) :: this
      is_initialized = this%mapl_initialized
   end function is_initialized

   subroutine finalize(this, rc)
      class(MaplFramework), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

!#      call finalize_profiler(_RC)
      call logging%free()
      call this%directory_service%free_directory_resources()

      if (this%esmf_internally_initialized) then
         call ESMF_HConfigDestroy(this%mapl_hconfig, _RC)
         call ESMF_Finalize(_RC)
      end if
      
      _RETURN(_SUCCESS)
   end subroutine finalize

   ! Procedures using singleton object
   subroutine mapl_get(unusable, directory_service, rc)
      class(KeywordEnforcer), optional, intent(out) :: unusable
      type(DirectoryService), pointer, optional, intent(out) :: directory_service
      integer, optional, intent(out) :: rc

      integer :: status

      call the_mapl_object%get(directory_service=directory_service, _RC)

      _RETURN(_SUCCESS)
   end subroutine mapl_get

   subroutine mapl_get_mapl(mapl)
      type(MaplFramework), pointer, intent(out) :: mapl

      mapl => the_mapl_object
   end subroutine mapl_get_mapl


  subroutine mapl_initialize(hconfig, unusable, mpiCommunicator, rc)
      use mapl_KeywordEnforcerMod
      type(ESMF_HConfig), intent(inout) :: hconfig
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: mpiCommunicator
      integer, optional, intent(out) :: rc

      integer :: status

      call the_mapl_object%initialize(hconfig=hconfig, mpiCommunicator=mpiCommunicator, _RC)

      _RETURN(_SUCCESS)
   end subroutine mapl_initialize

   subroutine mapl_finalize(rc)
      integer, optional, intent(out) :: rc

      integer :: status

      call the_mapl_object%finalize(_RC)

      _RETURN(_SUCCESS)
   end subroutine mapl_finalize

#ifdef BUILD_WITH_PFLOGGER
   subroutine initialize_pflogger(comm_world, unusable, pflogger_cfg_file, rc)
      use pflogger, only: pfl_initialize => initialize
      use pflogger, only: StreamHandler, FileHandler, HandlerVector
      use pflogger, only: MpiLock, MpiFormatter
      use pflogger, only: INFO, WARNING
      use PFL_Formatter, only: get_sim_time
      use mapl_SimulationTime, only: fill_time_dict

      use, intrinsic :: iso_fortran_env, only: OUTPUT_UNIT


      integer, intent(in) :: comm_world
      class (KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), optional,intent(in) :: pflogger_cfg_file
      integer, optional, intent(out) :: rc

      type (HandlerVector) :: handlers
      type (StreamHandler) :: console
      type (FileHandler) :: file_handler
      integer :: level,rank,status
      type(Logger), pointer :: lgr


      call pfl_initialize()
      get_sim_time => fill_time_dict

      if (present(pflogger_cfg_file)) then
         call logging%load_file(pflogger_cfg_file)
         _RETURN(_SUCCESS)
      end if

      ! Default configuration if no file provided

      call MPI_COMM_Rank(comm_world,rank,status)
      console = StreamHandler(OUTPUT_UNIT)
      call console%set_level(INFO)
      call console%set_formatter(MpiFormatter(comm_world, fmt='%(short_name)a10~: %(message)a'))
      call handlers%push_back(console)
      
      file_handler = FileHandler('warnings_and_errors.log')
      call file_handler%set_level(WARNING)
      call file_handler%set_formatter(MpiFormatter(comm_world, fmt='pe=%(mpi_rank)i5.5~: %(short_name)a~: %(message)a'))
      call file_handler%set_lock(MpiLock(comm_world))
      call handlers%push_back(file_handler)
      
      level = WARNING
      if (rank == 0) then
         level = INFO
      end if
      
      call logging%basic_config(level=level, handlers=handlers, rc=status)
      _VERIFY(status)
      
      if (rank == 0) then
         lgr => logging%get_logger('MAPL')
         call lgr%warning('No configure file specified for logging layer.  Using defaults.')
      end if

      _RETURN(_SUCCESS)

       _UNUSED_DUMMY(unusable)
  end subroutine initialize_pflogger
#endif

end module mapl3g_MaplFramework
