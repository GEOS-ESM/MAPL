#include "MAPL_Generic.h"

! The derived type "MaplFramework" is intended to encapsulate all of the singletons used within MAPL-based
! codes.   This limits the scope of the singleton "sin", which will allow proper object passing
! at some later date if justified.


module mapl3g_MaplFramework
   use mapl_ErrorHandling
   use mapl_KeywordEnforcerMod
   use mapl_profiler, only: DistributedProfiler
   use pfio_DirectoryServiceMod, only: DirectoryService
   use esmf, only: ESMF_Config, ESMF_ConfigGet
   use esmf, only: ESMF_HConfig, ESMF_HConfigDestroy
   use esmf, only: ESMF_Initialize, ESMF_Finalize
   use esmf, only: ESMF_VM
   use esmf, only: ESMF_VMGet
   use pflogger, only: pflogger_initialize => initialize
   use pfl_LoggerManager, only: LoggerManager
   implicit none
   private

   public :: MaplFramework
   public :: MAPL_initialize
   public :: MAPL_finalize
   public :: MAPL_Get

   type :: MaplFramework
      private
      logical :: initialized = .false.
      type(ESMF_HConfig) :: hconfig
      type(DirectoryService) :: directory_service
      type(LoggerManager) :: logger_manager
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
   subroutine initialize(this, unusable, configFilename, mpiCommunicator, configFilenameFromArgNum, rc)

      class(MaplFramework), target, intent(inout) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(*), optional, intent(in) :: configFilename
      integer, optional, intent(in) :: mpiCommunicator
      integer, optional, intent(in) :: configFilenameFromArgNum
      integer, optional, intent(out) :: rc

      integer :: status
      integer, allocatable :: configFilenameFromArgNum_
      type(ESMF_Config) :: config
      type(ESMF_VM) :: global_vm
      integer :: comm_world

      _ASSERT(.not. this%initialized, "MaplFramework object is already initialized")
      if (present(configFilenameFromArgNum)) then
         configFilenameFromArgNum_ = configFilenameFromArgNum
         _ASSERT(.not. present(configFilename), "Cannot specify both configFilename and ConfigFilenameFromArgNum")
      end if
      call ESMF_Initialize(configFilenameFromArgNum=configFilenameFromArgNum_, configFileName=configFilename, configKey=['esmf'], &
           mpiCommunicator=mpiCommunicator, &
           config=config, vm=global_vm, _RC)
      call ESMF_ConfigGet(config, hconfig=this%hconfig, _RC)
      call ESMF_VMGet(global_vm, mpiCommunicator=comm_world, _RC)

     call pflogger_initialize()
!#      call initialize_profiler(comm=comm_world, enable_global_timeprof=enable_global_timeprof, enable_global_memprof=enable_global_memprof, _RC)

      _HERE
      this%initialized = .true.

      _RETURN(_SUCCESS)
   end subroutine initialize
      
   subroutine get(this, unusable, hconfig, directory_service, logger_manager, rc)
      class(MaplFramework), target, intent(in) :: this
      class(KeywordEnforcer), optional, intent(out) :: unusable
      type(ESMF_HConfig), optional, intent(out) :: hconfig
      type(DirectoryService), pointer, optional, intent(out) :: directory_service
      type(LoggerManager), pointer, optional, intent(out) :: logger_manager
      integer, optional, intent(out) :: rc

      integer :: status

      _ASSERT(this%is_initialized(), "MaplFramework object is not initialized")
      if (present(hconfig)) hconfig = this%hconfig
      if (present(directory_service)) directory_service => this%directory_service
      if (present(logger_manager)) logger_manager => this%logger_manager

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
      call ESMF_HConfigDestroy(this%hconfig, _RC)
!#      call pflogger_finalize()
      call ESMF_Finalize(_RC)
      
      _RETURN(_SUCCESS)
   end subroutine finalize

   ! Procedures using singleton object
   subroutine mapl_get(unusable, hconfig, directory_service, logger_manager, rc)
      class(KeywordEnforcer), optional, intent(out) :: unusable
      type(ESMF_HConfig), optional, intent(out) :: hconfig
      type(DirectoryService), pointer, optional, intent(out) :: directory_service
      type(LoggerManager), pointer, optional, intent(out) :: logger_manager
      integer, optional, intent(out) :: rc

      integer :: status

      call the_mapl_object%get(hconfig=hconfig, directory_service=directory_service, logger_manager=logger_manager, _RC)

      _RETURN(_SUCCESS)
   end subroutine mapl_get

   subroutine mapl_get_mapl(mapl)
      type(MaplFramework), pointer, intent(out) :: mapl

      mapl => the_mapl_object
   end subroutine mapl_get_mapl


  subroutine mapl_initialize(unusable, configFilename, mpiCommunicator, configFilenameFromArgNum, rc)
      use pflogger, only: pflogger_initialize => initialize
      use mapl_KeywordEnforcerMod

      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(*), optional, intent(in) :: configFilename
      integer, optional, intent(in) :: mpiCommunicator
      integer, optional, intent(in) :: configFilenameFromArgNum
      integer, optional, intent(out) :: rc

      integer :: status

      call the_mapl_object%initialize(unusable, configFilename=configFilename, mpiCommunicator=mpiCommunicator, configFilenameFromArgNum=configFilenameFromArgNum, _RC)

      _RETURN(_SUCCESS)
   end subroutine mapl_initialize

   subroutine mapl_finalize(rc)
      integer, optional, intent(out) :: rc

      integer :: status

      call the_mapl_object%finalize(_RC)

      _RETURN(_SUCCESS)
   end subroutine mapl_finalize

end module mapl3g_MaplFramework
