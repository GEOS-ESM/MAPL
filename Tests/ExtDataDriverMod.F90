#include "MAPL_Generic.h"

module ExtDataDriverMod

   use MPI  
   use ESMF
   use MAPL
   use ExtData_DriverGridCompMod, only: ExtData_DriverGridComp, new_ExtData_DriverGridComp
   use ExtDataUtRoot_GridCompMod, only:  ROOT_SetServices => SetServices
   use gFTL_StringVector
   use MAPL_ApplicationSupport
   use, intrinsic :: iso_fortran_env, only: output_unit, REAL64, INT64
   implicit none

   public :: ExtDataDriver

   type :: ExtDataDriver
      private
      procedure(), nopass, pointer :: set_services => null()
      integer :: rank
      integer :: comm_world
      character(:), allocatable :: name
      type(ServerManager) :: cap_server
      type (ESMF_LogKind_Flag) :: esmf_logging_mode = ESMF_LOGKIND_NONE
      type(MpiServer), pointer :: i_server=>null()
      type(MpiServer), pointer :: o_server=>null()
      type(DirectoryService) :: directory_service
      class (MAPL_CapOptions), allocatable :: cap_options
      type(SplitCommunicator) :: split_comm

   contains
      procedure :: run
      procedure :: initialize_mpi
   end type ExtDataDriver

   interface ExtDataDriver
      module procedure newExtDataDriver
   end interface

contains

   function newExtDataDriver(name,set_services, unusable, cap_options, rc) result(driver)
      type(ExtDataDriver) :: driver
      character(*), intent(in) :: name
      procedure() :: set_services
      class (KeywordEnforcer),  optional, intent(in) :: unusable
      class ( MAPL_CapOptions), optional, intent(in) :: cap_options
      integer, optional, intent(out) :: rc
     
      integer :: status
      _UNUSED_DUMMY(unusable)

      driver%name = name
      driver%set_services => set_services
      if (present(cap_options)) then
         allocate(driver%cap_options, source = cap_options)
      else
         allocate(driver%cap_options, source = MAPL_CapOptions())
      endif
      call driver%initialize_mpi()
      call MAPL_Initialize(rc=status)
      _VERIFY(status)
      _RETURN(_SUCCESS)
   end function newExtDataDriver

   subroutine run(this,RC)

      class(ExtDataDriver), intent(inout) :: this
      integer,       optional, intent(OUT) :: rc

      integer                      :: STATUS
      type (ESMF_VM) :: VM
      type(ExtData_DriverGridComp), target :: cap
      integer :: mypet,i,pet_count
      integer, allocatable :: pet_list(:,:)
      integer, allocatable :: model_pets(:)
      logical :: model,front,back
      type(ESMF_State) :: export
      type(IOController) :: io_controller
      type(ESMF_Clock) :: clock
      character(len=:), allocatable :: hist_config

      model=.false.
      front=.false.
      back=.false.
      allocate(pet_list(3,2))

      pet_list(1,1)=0
      pet_list(1,2)=this%cap_options%npes_model-1
      pet_list(2,1)=this%cap_options%npes_model
      pet_list(2,2)=this%cap_options%npes_model+this%cap_options%npes_input_server(1)-1
      pet_list(3,1)=this%cap_options%npes_model+this%cap_options%npes_input_server(1)
      pet_list(3,2)=this%cap_options%npes_model+this%cap_options%npes_input_server(1)+this%cap_options%npes_output_server(1)-1

      call ESMF_Initialize (vm=vm, logKindFlag=this%cap_options%esmf_logging_mode, rc=status)
      _VERIFY(STATUS)
      call ESMF_VMGet(vm,localPet=mypet,petCount=pet_count,__RC__)
      model = mypet <= pet_list(1,2)
      allocate(model_pets(this%cap_options%npes_model))
      do i=1,this%cap_options%npes_model
         model_pets(i) = i-1
      enddo

      if (this%cap_options%npes_input_server(1) > 0 .and. this%cap_options%npes_output_server(1) > 0) then
         front = mypet >= pet_list(2,1) .and. mypet <= pet_list(2,2)
         back = mypet >= pet_list(3,1) .and. mypet <= pet_list(3,2)
      end if

      export = ESMF_StateCreate()
      write(*,*)"bmaa model pets ",model_pets
      cap = new_ExtData_DriverGridComp(root_setservices, name=this%name, configFileName="CAP.rc",pet_list=model_pets)
      call cap%set_services(rc = status)
      _VERIFY(status)
      call cap%initialize(export,clock,rc = status)
      _VERIFY(status)

      call ESMF_VMBarrier(vm,__RC__)
      call ESMF_StateReconcile(export,__RC__)
      call ESMF_VMBarrier(vm,__RC__)
      hist_config="newhist.yaml"
      call io_controller%initialize(export,hist_config,clock,pet_list,rc=status)
      _VERIFY(status)
      call io_controller%transfer_grids_to_front(rc=status)
      _VERIFY(status) 

      do i=1,10
         if (model) then
            call cap%run(export,clock, rc=status)
            _VERIFY(status)
         end if
         
      enddo 

      call cap%finalize(rc = status)
      _VERIFY(status)

      call MAPL_Finalize(rc=status)
      _VERIFY(status) 

      _RETURN(ESMF_SUCCESS)


   end subroutine run

   subroutine initialize_mpi(this, unusable, rc) 
      class (ExtDataDriver), intent(inout) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: ierror
      integer :: npes_world

      _UNUSED_DUMMY(unusable)

      call MPI_Init(ierror)

      this%comm_world=MPI_COMM_WORLD
      call MPI_Comm_rank(this%comm_world, this%rank, ierror); _VERIFY(ierror)
      call MPI_Comm_size(this%comm_world, npes_world, ierror); _VERIFY(ierror)

      if ( this%cap_options%npes_model == -1) then
         ! just a feed back to cap_options to maintain integrity
          this%cap_options%npes_model = npes_world
      endif
      _ASSERT(npes_world >= this%cap_options%npes_model, "npes_world is smaller than npes_model")

      _RETURN(_SUCCESS)

   end subroutine initialize_mpi


end module ExtDataDriverMod

