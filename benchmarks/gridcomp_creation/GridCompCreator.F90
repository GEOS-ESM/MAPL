#include "MAPL_Generic.h"
module grid_comp_creator

   use ESMF
   use MAPL
   use mapl3g_Cap
   use key_value_pairs
   use shared_constants

   implicit none
   private

   public :: initialize
   public :: run

   abstract interface
      function GenerateHConfig(rc) result(hconfig)
         type(ESMF_HConfig) :: hconfig
         integer, optional, intent(out) :: rc
      end function GenerateHConfig
      function GenerateGridComp(hconfig, rc) result(gc)
         type(ESMF_GridComp) :: gc
         type(ESMF_HConfig), intent(in) :: hconfig
         integer, optional, intent(out) :: rc
      end function GenerateGridComp
   end abstract interface

   character(len=MAXSTR) :: cap_filename = ''
   procedure, pointer :: cap_set_services => null()
   type(ServerManager) :: cap_server
   type(SplitCommunicator) :: split_comm
   integer :: rank
   integer :: comm_world
   character(len=:), allocatable :: cap_name
   integer :: npes_model
   character(len=:), allocatable :: logging_config
   type(ESMF_HConfig) :: cap_gridcomp

contains

   function generate_gridcomp_hconfig(hconfig, rc) result(gc)
      type(ESMF_GridComp) :: gc
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc
      integer :: status
      character(len=MAXSTR) :: name

      name = ESMF_HConfigAsString(hconfig, keyString = 'name', _RC)
      gc = ESMF_GridCompCreate(name=name, _RC)

   end function generate_gridcomp_hconfig

   subroutine initialize(parameter_filename, unusable, rc)
      procedure, intent(in) :: set_services
      character(len=*), intent(in) :: parameter_filename
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      integer :: status
      type(KeyValuePair), allocatable :: parameters(:)
      type(KeyValuePair), pointer :: pair
      character(len=:), allocatable :: key
      character(len=:), parameter :: NOT_FOUND = ' was not found.'

      _UNUSED_DUMMY(unusable)

      parameters = read_parameters(parameter_filename, _RC)

      key = 'cap_filename'
      pair => find_pair(key, parameters)
      _ASSERT(pair%is_not_empty, key // NOT_FOUND)
      cap_filename = pair%value

      key = 'npes_model'
      pair => find_pair(key, parameters)
      _ASSERT(pair%is_not_empty, key // NOT_FOUND)
      npes_model = pair%value
      
      call ESMF_Initialize(_RC)

      cap_hconfig = ESMF_HConfig(cap_filename, _RC)
      cap_name = ESMF_HConfigAsString(cap_hconfig, _RC)
      logging_config = ESMF_HConfigAsString(cap_hconfig, _RC)
      call initialize_mpi(npes_model, _RC)
      call MAPL_Initialize(comm=MPI_COMM_WORLD, logging_config=logging_config, _RC)
      _RETURN(_SUCCESS)

   end subroutine initialize

   subroutine finalize(rc)
      integer, optional, intent(out) :: rc
      integer :: status

      call ESMF_Finalize(_RC)
      _RETURN(_SUCCESS)

   end subroutine finalize_cap

   subroutine run(rc)
      integer, optional, intent(out) :: rc
      integer :: status
      character(len=:), parameter :: SPLITCOMM_NAME = 'model'
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
      call finalize(_RC)
      _RETURN(_SUCCESS)

   end subroutine run

   function make_generator_hconfig(hconfig, rc) result(gen)
      type(GridCompGenerator) :: gen
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc
      integer :: status
      
   end function make_generator_hconfig
   
   function make_gridcomp(rc) result(gc)
      type(ESMF_GridComp) :: gc
      integer, optional, intent(out) :: rc
      integer :: status

      
   end function make_gridcomp

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

   subroutine split(str, delim , token, remain) 
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

end module grid_comp_creator
