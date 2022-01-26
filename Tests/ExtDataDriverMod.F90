#include "MAPL_Generic.h"

module ExtDataDriverMod

   use MPI  
   use ESMF
   use MAPL
   use ExtData_DriverGridCompMod, only: ExtData_DriverGridComp, new_ExtData_DriverGridComp, driver_clockInit
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
      type (ESMF_LogKind_Flag) :: esmf_logging_mode = ESMF_LOGKIND_NONE
      class (MAPL_CapOptions), allocatable :: cap_options
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
      integer :: mypet,i,pet_count,rank
      logical :: model
      type(ESMF_State) :: export
      type(IOController) :: io_controller
      type(ESMF_Clock) :: clock
      integer :: comm
      character(len=:), allocatable :: hist_config
      integer, allocatable :: model_pets(:)
      integer :: nstep
      type(ESMF_Config) :: cf

      call ESMF_Initialize (vm=vm, logKindFlag=this%cap_options%esmf_logging_mode, rc=status)
      _VERIFY(STATUS)
      call ESMF_VMGet(vm,localPet=mypet,petCount=pet_count,mpiCommunicator=comm,__RC__)
      call MPI_COMM_RANK(comm,rank,status)
      _VERIFY(status)

      nstep = this%cap_options%nsteps
      model = (rank < this%cap_options%npes_model)
      allocate(model_pets(this%cap_options%npes_model))
      do i=1,this%cap_options%npes_model
         model_pets(i)=i-1
      enddo

      !call start_io_prof(MPI_COMM_WORLD)
      export = ESMF_StateCreate()
      cap = new_ExtData_DriverGridComp(root_setservices, name=this%name, configFileName="CAP.rc",pet_list=model_pets)
      
      cf = ESMF_ConfigCreate()
      call ESMF_ConfigLoadFile(cf,"CAP.rc",_RC)
      call driver_clockInit(cf,clock,_RC)
      call cap%set_services(rc = status)
      _VERIFY(status)
      call cap%initialize(export,clock,rc = status)
      _VERIFY(status)

      call ESMF_VMBarrier(vm,__RC__)
      call ESMF_StateReconcile(export,__RC__)
      call ESMF_VMBarrier(vm,__RC__)
      hist_config="newhist.yaml"
      call ESMF_VMBarrier(vm,__RC__)
      call MPI_Barrier(MPI_COMM_WORLD,status)
      call io_controller%initialize(export,hist_config,clock,this%cap_options%npes_model,this%cap_options%npes_backend_pernode,rc=status)
      _VERIFY(status)
      call ESMF_VMBarrier(vm,__RC__)
      call MPI_Barrier(MPI_COMM_WORLD,status)

      call io_controller%start_writer(_RC)
      do i=1,nstep
         if (model) then
            call cap%run(export,clock,_RC)
         end if
         call ESMF_ClockAdvance(clock,_RC)
         call io_controller%run(clock,_RC)
      enddo 
     
      if (model) then 
         call cap%finalize(clock,rc = status)
         _VERIFY(status)
      end if

      call io_controller%stop_writer(_RC)
      call MPI_Barrier(MPI_COMM_WORLD,status)
      call generate_io_summary(rank)

      call MAPL_Finalize(rc=status)
      _VERIFY(status)
      call MPI_Finalize(status)
 
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

