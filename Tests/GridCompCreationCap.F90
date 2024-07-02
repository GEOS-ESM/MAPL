#include "MAPL_Generic.h"
module grid_comp_creation_cap

   use, intrinsic :: iso_fortran_env, only: int32, int64, real32, real64
   implicit none
   private

   type :: TestParameters
      type(ESMF_LogKind_Flag), parameter :: esmf_logging_mode = ESMF_LOGKIND_NONE
      character(len=*), allocatable :: name
      integer :: npes_model = -1
      type(TestParameters), allocatable :: subtest_parameters => null()
      character(:), allocatable :: logging_config
   end type TestParameters

   interface TestParameters
      module procedure :: construct_test_parameters
   end interface TestParameters

   procedure, pointer :: cap_set_services => null()
   type(ServerManager) :: cap_server
   type(SplitCommunicator) :: split_comm
   integer :: rank
   integer :: comm_world
   character(len=:), allocatable :: cap_name
   integer :: npes_world

contains

   function construct_test_parameters(name, npes_model) result(parameters)
      Type(TestParameters) :: parameters
      character(len=*), intent(in) :: name
      integer, intent(in) :: npes_model

      parameters%name = name
      parameters%npes_model = npes_model

   end function construct_test_parameters

   subroutine initialize_cap(set_services, parameters, unusable, rc)
      procedure, intent(in) :: set_services
      class(TestParameters), intent(inout) :: parameters
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      integer :: status

      _UNUSED_DUMMY(unusable)
      cap_name = parameters%name
      cap_set_services => set_services
      call initialize_mpi(_RC)
      call MAPL_Initialize(comm=MPI_COMM_WORLD, logging_config=parameters%logging_config, _RC)
      _RETURN(_SUCCESS)

   end subroutine initialize_cap

   subroutine initialize_mpi(parameters, rc) 
      type(TestParameters), intent(in) :: parameters
      integer, optional, intent(out) :: rc
      integer :: ierror
      integer :: npes_world

      call MPI_Init(ierror)

      comm_world=MPI_COMM_WORLD
      call MPI_Comm_rank(comm_world, rank, ierror); _VERIFY(ierror)
      call MPI_Comm_size(comm_world, npes_world, ierror); _VERIFY(ierror)

      if (parameters%npes_model == -1) then
         ! just a feed back to parameters to maintain integrity
          parameters%npes_model = npes_world
      endif
      _ASSERT(npes_world >= parameters%npes_model, "npes_world is smaller than npes_model")

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

   subroutine run_cap(parametes, rc)
      type(TestParameters), intent(in) :: parameters
      integer, optional, intent(out) :: rc
      integer :: status
      character(len=:), parameter :: SPLITCOMM_NAME = 'model'
      character(len=:), parameter :: CAP_FILE = 'GridCompCreationCap.yaml'
      integer :: comm_cap
      type(SplitCommunicator) :: split_comm
      type(ESMF_VM) :: vm

      comm_cap = MPI_COMM_WORLD
      call cap_server%get_splitcomm(split_comm)
      if(split%get_name() == SPLITCOMM_NAME) then
         call ESMF_Initialize(vm=vm, logKindFlag=parameters%esmf_logging_mode,&
            & mpiCommunicator=split_comm%get_subcommunicator(), _RC)
         call read_test_parameters(CAP_FILE, _RC)
      end if
      ! Read HConfig
      call finalize_cap(comm_cap, _RC)

   end subroutine run_cap

   subroutine read_test_parameters(filename, rc)
      character(len=*), intent(in) :: filename
      integer, optional, intent(out) :: rc
      integer :: status

      hconfig = ESMF_HConfigCreate(filename, _RC)
      call ESMF_HConfigDestroy(hconfig, _RC)
   end subroutine read_test_parameters
      
end module grid_comp_creation_cap
