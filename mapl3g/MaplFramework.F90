#include "MAPL_Generic.h"

! The derived type "MaplFramework" is intended to encapsulate all of the singletons used within MAPL-based
! codes.   This limits the scope of the singleton "sin", which will allow proper object passing
! at some later date if justified.


module mapl3g_MaplFramework
   use mapl_ErrorHandling
   use mapl_KeywordEnforcerMod
   use mapl_profiler, only: DistributedProfiler
   use pfio_DirectoryServiceMod, only: DirectoryService
   use pflogger, only: logging
   use pflogger, only: Logger
   use esmf, only: ESMF_IsInitialized
   use esmf, only: ESMF_VM, ESMF_VMGetCurrent, ESMF_VMGet
   implicit none
   private

   public :: MaplFramework
   public :: MAPL_initialize
   public :: MAPL_finalize
   public :: MAPL_Get

   type :: MaplFramework
      private
      logical :: initialized = .false.
      type(DirectoryService) :: directory_service
      type(DistributedProfiler) :: time_profiler
   contains
      procedure :: initialize
      procedure :: get
      procedure :: is_initialized
      procedure :: finalize
   end type MaplFramework

   ! Private singleton object.  Used 
   type(MaplFramework), target :: the_mapl_object

   interface MAPL_Get
      procedure :: mapl_get
      procedure :: mapl_get_mapl
   end interface MAPL_Get

contains

   ! Type-bound procedures

   subroutine initialize(this, unusable, logging_cfg_file, rc)
      class(MaplFramework), target, intent(inout) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(*), optional, intent(in) :: logging_cfg_file
      integer, optional, intent(out) :: rc

      logical :: esmf_is_initialized
      integer :: comm_world
      type(ESMF_VM) :: mapl_vm
      integer :: status

      esmf_is_initialized = ESMF_IsInitialized(_RC)
      _ASSERT(esmf_is_initialized, "ESMF must be initialized prior to initializing MAPL.")

      _ASSERT(.not. this%initialized, "MaplFramework object is already initialized")

      call ESMF_VMGetCurrent(mapl_vm, _RC)
      call ESMF_VMGet(mapl_vm, mpiCommunicator=comm_world, _RC)

#ifdef BUILD_WITH_PFLOGGER
      call initialize_pflogger(comm_world=comm_world,logging_cfg_file=logging_cfg_file, _RC)
#endif
!#      call initialize_profiler(comm=comm_world, enable_global_timeprof=enable_global_timeprof, enable_global_memprof=enable_global_memprof, _RC)

      _HERE
      this%initialized = .true.

      _RETURN(_SUCCESS)
   end subroutine initialize
      
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
      is_initialized = this%initialized
   end function is_initialized

   subroutine finalize(this, rc)
      class(MaplFramework), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

!#      call finalize_profiler(_RC)
!#      call pflogger_finalize()
      
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


  subroutine mapl_initialize(unusable, logging_cfg_file, rc)
      use mapl_KeywordEnforcerMod
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), optional, intent(in) :: logging_cfg_file
      integer, optional, intent(out) :: rc

      integer :: status

      call the_mapl_object%initialize(unusable, logging_cfg_file=logging_cfg_file, _RC)

      _RETURN(_SUCCESS)
   end subroutine mapl_initialize

   subroutine mapl_finalize(rc)
      integer, optional, intent(out) :: rc

      integer :: status

      call the_mapl_object%finalize(_RC)

      _RETURN(_SUCCESS)
   end subroutine mapl_finalize

#ifdef BUILD_WITH_PFLOGGER
   subroutine initialize_pflogger(comm_world, unusable, logging_cfg_file, rc)
      use pflogger, only: pfl_initialize => initialize
      use pflogger, only: StreamHandler, FileHandler, HandlerVector
      use pflogger, only: MpiLock, MpiFormatter
      use pflogger, only: INFO, WARNING
      use PFL_Formatter, only: get_sim_time
      use mapl_SimulationTime, only: fill_time_dict

      use, intrinsic :: iso_fortran_env, only: OUTPUT_UNIT


      integer, intent(in) :: comm_world
      class (KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), optional,intent(in) :: logging_cfg_file
      integer, optional, intent(out) :: rc

      type (HandlerVector) :: handlers
      type (StreamHandler) :: console
      type (FileHandler) :: file_handler
      integer :: level,rank,status
      type(Logger), pointer :: lgr


      call pfl_initialize()
      get_sim_time => fill_time_dict

      if (present(logging_cfg_file)) then
         call logging%load_file(logging_cfg_file)
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
