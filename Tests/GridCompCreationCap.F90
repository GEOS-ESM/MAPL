#include "MAPL_Generic.h"
module grid_comp_creation_cap

!_   use, intrinsic :: iso_fortran_env, only: int32, int64, real32, real64
   implicit none
   private
!_   public ::
!_ INTERFACES
!_ TYPES
!_ VARIABLES
   type(ESMF_LogKindFlag), parameter :: ESMF_LOGGING_MODE = ESMF_LOGKIND_NONE

   procedure, pointer :: cap_set_services => null()
   type(MAPL_CapOptions) :: cap_options = MAPL_CapOptions()
   type(ServerManager) :: cap_server
   type(SplitCommunicator) :: split_comm
   integer :: rank
   integer :: comm_world
   character(len=:), allocatable :: cap_name
   integer :: npes_world

contains

   subroutine initialize_cap(name, set_services, unusable, options, rc)
      character(len=*), intent(in) :: name
      procedure, intent(in) :: set_services
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(MAPL_CapOptions), optional, intent(in) :: options
      integer, optional, intent(out) :: rc
      integer :: status

      _UNUSED_DUMMY(unusable)
      cap_name = name
      cap_set_services => set_services
      if(present(options)) cap_options = options
      call initialize_mpi(_RC)
      call MAPL_Initialize(comm=MPI_COMM_WORLD, logging_config=cap_options%logging_config, _RC)
      _RETURN(_SUCCESS)

   end subroutine initialize_cap

   subroutine initialize_mpi(rc) 
      integer, optional, intent(out) :: rc
      integer :: ierror
      integer :: npes_world


      call MPI_Init(ierror)

      comm_world=MPI_COMM_WORLD
      call MPI_Comm_rank(comm_world, rank, ierror); _VERIFY(ierror)
      call MPI_Comm_size(comm_world, npes_world, ierror); _VERIFY(ierror)

      if (cap_options%npes_model == -1) then
         ! just a feed back to cap_options to maintain integrity
          cap_options%npes_model = npes_world
      endif
      _ASSERT(npes_world >= cap_options%npes_model, "npes_world is smaller than npes_model")

      _RETURN(_SUCCESS)

   end subroutine initialize_mpi

   subroutine finalize_cap(comm_cap, rc)
      integer, intent(in) :: comm_cap
      integer, optional, intent(out) :: rc
      integer :: status
      integer, parameter :: FUNIT = 99
      character(len=:), parameter :: EGRESS_FILE = 'egress'
      character(len=:), parameter :: EGRESS_FORM = 'formatted'
      integer :: rank

      call MPI_Comm_Rank(comm_cap, rank, _RC)
      if(rank==0) then
         close(FUNIT)
         open(FUNIT, file=EGRESS_FILE, form=EGRESS_FORM)
      end if

      call MAPL_Finalize(_RC)
      call mpi_finalize(_RC)

      _RETURN(_SUCCESS)

   end subroutine finalize_cap

   subroutine run_cap(rc)
      integer, optional, intent(out) :: rc
      integer :: status
      character(len=:), parameter :: SPLITCOMM_NAME = 'model'
      character(len=:), parameter :: CAP_FILE = 'GridCompCreationCap.rc'
      integer :: comm_cap
      type(SplitCommunicator) :: split_comm
      type (ESMF_VM) :: vm

      comm_cap = MPI_COMM_WORLD
      call cap_server%get_splitcomm(split_comm)
      if(split%get_name() == SPLITCOMM_NAME) then
         call ESMF_Initialize (vm=vm, logKindFlag=cap_options%esmf_logging_mode, &
            & mpiCommunicator=split_comm%get_subcommunicator(), _RC)
         config = ESMF_ConfigCreate(_RC)
         call ESMF_ConfigLoadFile(config, CAP_FILE, _RC)

      end if


      call finalize_cap(comm_cap, _RC)

   end subroutine run_cap

end module grid_comp_creation_cap
