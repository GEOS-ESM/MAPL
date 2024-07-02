#include "MAPL_Generic.h"
module grid_comp_creator

   use MPI
   use ESMF
   use MAPL
   use mapl3g_Cap
   use, intrinsic :: iso_fortran_env, only: int32, int64, real32, real64
   implicit none
   private

   public :: MAXSTR

   type :: KeyValuePair
      character(len=:), allocatable :: key
      character(len=*), allocatable :: value
      logical :: is_not_empty = .TRUE..
   end type

   integer, parameter :: MAXSTR = 256
   procedure, pointer :: cap_set_services => null()
   type(ServerManager) :: cap_server
   type(SplitCommunicator) :: split_comm
   integer :: rank
   integer :: comm_world
   character(len=:), allocatable :: cap_name
   integer :: npes_model
   character(len=:), allocatable :: logging_config

   type(KeyValuePair), parameter, target :: NULL_PAIR = KeyValuePair('', '', .FALSE.)

contains

   subroutine initialize(set_services, parameter_file, parameters, unusable, rc)
      procedure, intent(in) :: set_services
      character(len=*), intent(in) :: parameter_file
      type(KeyValuePair), intent(out) :: parameters(:)
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      integer :: status
      type(KeyValuePair), pointer :: pair
      character(len=:), allocatable :: key
      character(len=:), parameter :: NOT_FOUND = ' was not found.'

      _UNUSED_DUMMY(unusable)

      cap_set_services => set_services
      parameters = read_parameters(parameter_file, _RC)

      key = 'cap_name'
      pair => find_pair(key, parameters)
      _ASSERT(pair%is_not_empty, key // NOT_FOUND)
      cap_name = pair%value

      key = 'npes_model'
      pair => find_pair(key, parameters)
      _ASSERT(pair%is_not_empty, key // NOT_FOUND)
      npes_model = pair%value
      
      key = 'logging_config'
      pair => find_pair(key, parameters)
      _ASSERT(pair%is_not_empty, key // NOT_FOUND)
      logging_config = pair%value

      call initialize_mpi(npes_model, _RC)
      call MAPL_Initialize(comm=MPI_COMM_WORLD, logging_config=logging_config, _RC)
      _RETURN(_SUCCESS)

   end subroutine initialize

   subroutine initialize_mpi(npes, rc) 
      integer, intent(inout) :: npes
      integer, optional, intent(out) :: rc
      integer :: ierror
      integer :: npes_world

      call MPI_Init(ierror)

      comm_world=MPI_COMM_WORLD
      call MPI_Comm_rank(comm_world, rank, ierror); _VERIFY(ierror)
      call MPI_Comm_size(comm_world, npes_world, ierror); _VERIFY(ierror)
      if (npes == -1) npes = npes_world
      _ASSERT(npes_world >= npes, "npes_world is smaller than npes_model")

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

   subroutine run_cap(parameters, rc)
      type(KeyValuePairs), intent(in) :: parameters(:)
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

   function construct_null_pair() result(null_pair)
      type(NullPair) :: null_pair

      null_pair%key = ''
      null_pair%value = ''

   end function construct_null_pair

   function read_parameters(filename, rc) result(parameters)
      type(KeyValuePair), allocatable :: parameters(:)
      character(len=*), intent(in) :: filename
      integer, optional, intent(out) :: rc
      integer :: status, funit
      character(len=MAXSTR) :: raw
      character(len=:), parameter :: FMT_ = '(A)'
      integer :: numlines
      character(len=:), allocatable :: key, value

      open(newunit=funit, file=trim(filename), status='old', action='read', _IOSTAT)

      numlines = 0
      do while (.not. EOF(funit))
         numlines = numlines + 1
         read(funit, fmt=FMT_) raw
         if(len_trim(raw) == 0) cycle
      end do
      allocate(parameters(numlines))

      rewind(funit, _IOSTAT)
      i = 0
      do while (.not. EOF(funit))
         read(funit, fmt=FMT_) raw
         raw = adjustl(raw)
         if(len_trim(raw) == 0) cycle
         call split(raw, ':', key, value)
         if(len(key) == 0) cycle
         i = i+1
         parameters(i) = KeyValuePair(key, value)
      end do

      parameters = parameters(1:i)

   end function read_parameters

   subroutine split(str, token, remain, delim) 
      character(len=*), intent(in) :: str
      character(len=*), intent(in) :: delim
      character(len=:), allocatable, intent(out) :: token
      character(len=:), allocatable, intent(out) :: remain
      integer :: i

      token = trim(adjustl(str))
      i = index(token, delim)
      remain = token((i+len(delim)):)
      token = token(:(i-1))

   end subroutine split

   function find_pair(key, pairs) result(ptr)
      type(KeyValuePair), pointer :: ptr
      character(len=*), intent(in) :: key
      class(KeyValuePair), intent(in) :: pairs(:)
      character(len=:), allocatable :: key_

      ptr => NULL_PAIR
      key_ = trim(adjustl(key))
      if(len(key_) == 0 .or. size(pairs) == 0) return
      do i=1, size(pairs)
         if(pairs(i)%key /= key_) cycle
         ptr => pairs(i)
      end do
      
   end function find_pair

end module grid_comp_creator
