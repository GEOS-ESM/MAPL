#include "MAPL_ErrLog.h"
#include "unused_dummy.H"


module MAPL_CapMod
   use MPI
   use ESMF
   use MAPL_SimpleCommSplitterMod
   use MAPL_SplitCommunicatorMod
   use MAPL_KeywordEnforcerMod
   use MAPL_CapGridCompMod
   use MAPL_BaseMod
   use MAPL_ExceptionHandling
   use pFIO
   use MAPL_CapOptionsMod
   use MAPL_ServerManager
   use MAPL_ApplicationSupport
   use, intrinsic :: iso_fortran_env, only: REAL64, INT64, OUTPUT_UNIT
   implicit none
   private

   public :: MAPL_Cap

   type :: MAPL_Cap
      private
      character(:), allocatable :: name
      procedure(), nopass, pointer :: set_services => null()
      logical :: non_dso = .false.
      integer :: comm_world
      integer :: rank
      integer :: npes_member
      character(:), allocatable :: root_dso

      type (MAPL_CapOptions), allocatable :: cap_options
      ! misc
      logical :: mpi_already_initialized = .false.
      type(MAPL_CapGridComp), public :: cap_gc
      type(ServerManager) :: cap_server
      type(SimpleCommSplitter), public :: splitter
   contains
      procedure :: run
      procedure :: run_ensemble
      procedure :: run_member
      procedure :: run_model
      procedure :: step_model
      procedure :: rewind_model

      procedure :: create_member_subcommunicator
      procedure :: initialize_io_clients_servers
      procedure :: finalize_io_clients_servers
      procedure :: initialize_cap_gc
      procedure :: initialize_mpi
      procedure :: finalize_mpi


      !getters
      procedure :: get_npes_model
      procedure :: get_comm_world
      procedure :: get_n_members
      procedure :: get_cap_gc
      procedure :: get_cap_rc_file
      procedure :: get_egress_file

   end type MAPL_Cap

   interface MAPL_Cap
      module procedure new_MAPL_Cap_from_set_services
      module procedure new_MAPL_Cap_from_dso
   end interface MAPL_Cap


   interface
      integer function c_chdir(path) bind(C,name="chdir")
         use iso_c_binding
         character(kind=c_char) :: path(*)
      end function c_chdir
   end interface

contains

   function new_MAPL_Cap_from_set_services(name, set_services, unusable, cap_options, rc) result(cap)
      type (MAPL_Cap) :: cap
      character(*), intent(in) :: name
      procedure() :: set_services
      class (KeywordEnforcer),  optional, intent(in) :: unusable
      type ( MAPL_CapOptions), optional, intent(in) :: cap_options
      integer, optional, intent(out) :: rc
      integer :: status

      cap%name = name
      cap%set_services => set_services
      cap%non_dso = .true.

      if (present(cap_options)) then
         allocate(cap%cap_options, source = cap_options)
      else
         allocate(cap%cap_options, source = MAPL_CapOptions())
      endif

      if (cap%cap_options%use_comm_world) then
         cap%comm_world       = MPI_COMM_WORLD
         cap%cap_options%comm = MPI_COMM_WORLD
      else
         cap%comm_world = cap%cap_options%comm
      endif

      call cap%initialize_mpi(rc=status)
      _VERIFY(status)

      call MAPL_Initialize(comm=cap%comm_world, &
                           logging_config=cap%cap_options%logging_config, &
                           rc=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

    end function new_MAPL_Cap_from_set_services

   function new_MAPL_Cap_from_dso(name, unusable, cap_options, rc) result(cap)
      type (MAPL_Cap) :: cap
      character(*), intent(in) :: name
      class (KeywordEnforcer),  optional, intent(in) :: unusable
      type ( MAPL_CapOptions), optional, intent(in) :: cap_options
      integer, optional, intent(out) :: rc
      integer :: status

      cap%name = name

      if (present(cap_options)) then
         allocate(cap%cap_options, source = cap_options)
      else
         allocate(cap%cap_options, source = MAPL_CapOptions())
      endif

      if (cap%cap_options%use_comm_world) then
         cap%comm_world       = MPI_COMM_WORLD
         cap%cap_options%comm = MPI_COMM_WORLD
      else
         cap%comm_world = cap%cap_options%comm
      endif

      call cap%initialize_mpi(rc=status)
      _VERIFY(status)

      call MAPL_Initialize(comm=cap%comm_world, &
                           logging_config=cap%cap_options%logging_config, &
                           rc=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

    end function new_MAPL_Cap_from_dso


   ! 3. Run the ensemble (default is 1 member)
   ! 4. Finalize MPI if initialized locally.
   subroutine run(this, unusable, rc)
      class (MAPL_Cap), intent(inout) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      integer :: status
!


      _UNUSED_DUMMY(unusable)

      call this%run_ensemble(rc=status); _VERIFY(status)
      call this%finalize_mpi(rc=status); _VERIFY(status)

      _RETURN(_SUCCESS)

    end subroutine run


   ! This layer splits the communicator to support running a
   ! multi-member ensemble.
   subroutine run_ensemble(this, unusable, rc)
      class (MAPL_Cap), target, intent(inout) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: subcommunicator

      _UNUSED_DUMMY(unusable)

      subcommunicator = this%create_member_subcommunicator(this%comm_world, rc=status); _VERIFY(status)
      if (subcommunicator /= MPI_COMM_NULL) then
         call this%initialize_io_clients_servers(subcommunicator, rc = status); _VERIFY(status)
         call this%run_member(rc=status); _VERIFY(status)
         call this%finalize_io_clients_servers()
         call this%splitter%free_sub_comm()
      end if

      _RETURN(_SUCCESS)

   end subroutine run_ensemble


   subroutine finalize_io_clients_servers(this, unusable, rc)
     class (MAPL_Cap), target, intent(inout) :: this
     class (KeywordEnforcer), optional, intent(in) :: unusable
     integer, optional, intent(out) :: rc
     type(SplitCommunicator) :: split_comm

     _UNUSED_DUMMY(unusable)
     call this%cap_server%get_splitcomm(split_comm)
     select case(split_comm%get_name())
     case('model')
        call i_Clients%terminate()
        call o_Clients%terminate()
     end select
     call this%cap_server%finalize()
     _RETURN(_SUCCESS)

   end subroutine finalize_io_clients_servers

   subroutine initialize_io_clients_servers(this, comm, unusable, rc)
     class (MAPL_Cap), target, intent(inout) :: this
     integer, intent(in) :: comm
     class (KeywordEnforcer), optional, intent(in) :: unusable
     integer, optional, intent(out) :: rc
     integer :: status

     _UNUSED_DUMMY(unusable)
     call this%cap_server%initialize(comm, &
         application_size=this%cap_options%npes_model, &
         nodes_input_server=this%cap_options%nodes_input_server, &
         nodes_output_server=this%cap_options%nodes_output_server, &
         npes_input_server=this%cap_options%npes_input_server, &
         npes_output_server=this%cap_options%npes_output_server, &
         oserver_type=this%cap_options%oserver_type, &
         npes_backend_pernode=this%cap_options%npes_backend_pernode, &
         isolate_nodes = this%cap_options%isolate_nodes, &
         fast_oclient  = this%cap_options%fast_oclient, &
         with_profiler = this%cap_options%with_io_profiler, &
         rc=status)
     _VERIFY(status)
     _RETURN(_SUCCESS)

   end subroutine initialize_io_clients_servers

   ! This layer splits the communicator to support separate i/o servers
   ! and runs the model via a CapGridComp.
   subroutine run_member(this, rc)
      use MAPL_CFIOMod
      class (MAPL_Cap), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(SplitCommunicator) :: split_comm

      call this%cap_server%get_splitcomm(split_comm)
      select case(split_comm%get_name())
      case('model')
         call this%run_model(comm=split_comm%get_subcommunicator(), rc=status); _VERIFY(status)
      end select

     _RETURN(_SUCCESS)

   end subroutine run_member


   subroutine run_model(this, comm, unusable, rc)
      use pFlogger, only: logging, Logger
      class (MAPL_Cap), target, intent(inout) :: this
      integer, intent(in) :: comm
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) ::rc

      integer(kind=INT64) :: start_tick, stop_tick, tick_rate
      integer :: rank, ierror
      integer :: status
      class(Logger), pointer :: lgr
      logical :: esmfConfigFileExists
      type (ESMF_VM) :: vm
      character(len=:), allocatable :: esmfComm, esmfConfigFile
      integer :: esmfConfigFileLen

      _UNUSED_DUMMY(unusable)

      call start_timer()

      ! Look for a file called "ESMF.rc" but we want to do this on root and then
      ! broadcast the result to the other ranks

      call MPI_COMM_RANK(comm, rank, status)
      _VERIFY(status)

      ! We look to see if the user has set an environment variable for the
      ! name of the ESMF configuration file. If they have, we use that. If not,
      ! we use the default of "ESMF.rc" for backward compatibility

      ! Step one: default to ESMF.rc

      esmfConfigFile = 'ESMF.rc'
      esmfConfigFileLen = len(esmfConfigFile)

      ! Step two: get the length of the environment variable
      call get_environment_variable('ESMF_CONFIG_FILE', length=esmfConfigFileLen, status=status)
      ! Step three: if the environment variable exists, get the value of the environment variable
      if (status == 0) then ! variable exists
         ! We need to deallocate so we can reallocate
         deallocate(esmfConfigFile)
         allocate(character(len = esmfConfigFileLen) :: esmfConfigFile)
         call get_environment_variable('ESMF_CONFIG_FILE', value=esmfConfigFile, status=status)
         _VERIFY(status)
      end if

      if (rank == 0) then
         inquire(file=esmfConfigFile, exist=esmfConfigFileExists)
      end if
      call MPI_BCAST(esmfConfigFileExists, 1, MPI_LOGICAL, 0, comm, status)
      _VERIFY(status)
      call MPI_BCAST(esmfConfigFile, esmfConfigFileLen, MPI_CHARACTER, 0, comm, status)
      _VERIFY(status)

      lgr => logging%get_logger('MAPL')

      ! If the file exists, we pass it into ESMF_Initialize, else, we
      ! use the one from the command line arguments
      if (esmfConfigFileExists) then
         call lgr%info("Using ESMF configuration file: %a", esmfConfigFile)
         call ESMF_Initialize (configFileName=esmfConfigFile, mpiCommunicator=comm, vm=vm, _RC)
      else
         call ESMF_Initialize (logKindFlag=this%cap_options%esmf_logging_mode, mpiCommunicator=comm, vm=vm, _RC)
      end if

      ! We check to see if ESMF_COMM was built as mpiuni which is not allowed for MAPL
      call ESMF_VmGet(vm, esmfComm = esmfComm, _RC)
      _ASSERT( esmfComm /= 'mpiuni', 'ESMF_COMM=mpiuni is not allowed for MAPL')

      ! Note per ESMF this is a temporary routine as eventually MOAB will
      ! be the only mesh generator. But until then, this allows us to
      ! test it
      call ESMF_MeshSetMOAB(this%cap_options%with_esmf_moab, rc=status)
      _VERIFY(status)

      call lgr%info("Running with MOAB library for ESMF Mesh: %l1", this%cap_options%with_esmf_moab)

      call this%initialize_cap_gc(rc=status)
      _VERIFY(status)

      call this%cap_gc%set_services(rc = status)
      _VERIFY(status)
      call this%cap_gc%initialize(rc=status)
      _VERIFY(status)
      call this%cap_gc%run(rc=status)
      _VERIFY(status)
      call this%cap_gc%finalize(rc=status)
      _VERIFY(status)

      call ESMF_Finalize(endflag=ESMF_END_KEEPMPI, rc=status)
      _VERIFY(status)
      call stop_timer()

      call report_throughput()

      _RETURN(_SUCCESS)
   contains

      subroutine start_timer()
         call system_clock(start_tick, count_rate=tick_rate)
      end subroutine start_timer

      subroutine stop_timer()
         call system_clock(stop_tick)
      end subroutine stop_timer

      subroutine report_throughput(rc)
         integer, optional, intent(out) :: rc

         integer :: rank, ierror
         real(kind=REAL64) :: model_duration, wall_time, model_days_per_day

         call MPI_Comm_rank(this%comm_world, rank, ierror)
         _VERIFY(ierror)

         if (rank == 0) then
            model_duration = this%cap_gc%get_model_duration()
            wall_time = (stop_tick - start_tick) / real(tick_rate, kind=REAL64)

            model_days_per_day = model_duration / wall_time


            lgr => logging%get_logger('MAPL.profiler')
            call lgr%info("Model Throughput: %f12.3 days per day", model_days_per_day)
         end if

      end subroutine report_throughput

   end subroutine run_model

   subroutine initialize_cap_gc(this, unusable, n_run_phases, rc)
     class(MAPL_Cap), target, intent(inout) :: this
     class (KeywordEnforcer), optional, intent(in) :: unusable
     integer, optional, intent(in) :: n_run_phases
     integer, optional, intent(out) :: rc

     integer :: status
     type(ESMF_PIN_Flag) :: pinflag

     _UNUSED_DUMMY(unusable)

     pinflag = GetPinFlagFromConfig(this%cap_options%cap_rc_file, _RC)
        call MAPL_PinFlagSet(pinflag)

     if (this%non_dso) then
        call MAPL_CapGridCompCreate(this%cap_gc, this%get_cap_rc_file(), &
           this%name, this%get_egress_file(), n_run_phases=n_run_phases, root_set_services = this%set_services,rc=status)
     else
        _ASSERT(this%cap_options%root_dso /= 'none',"No set services specified, must pass a dso")
        call MAPL_CapGridCompCreate(this%cap_gc, this%get_cap_rc_file(), &
           this%name, this%get_egress_file(), n_run_phases=n_run_phases, root_dso = this%cap_options%root_dso,rc=status)
     end if
     _VERIFY(status)
     _RETURN(_SUCCESS)
   end subroutine initialize_cap_gc


   subroutine step_model(this, rc)
     class(MAPL_Cap), intent(inout) :: this
     integer, intent(out) :: rc
     integer :: status
     call this%cap_gc%step(rc = status); _VERIFY(status)
     _RETURN(_SUCCESS)
   end subroutine step_model

   subroutine rewind_model(this, time, rc)
     class(MAPL_Cap), intent(inout) :: this
     type(ESMF_Time), intent(inout) :: time
     integer, intent(out) :: rc
     integer :: status
     call this%cap_gc%rewind_clock(time,rc = status); _VERIFY(status)
     _RETURN(_SUCCESS)
   end subroutine rewind_model

   integer function create_member_subcommunicator(this, comm, unusable, rc) result(subcommunicator)
      class (MAPL_Cap), intent(inout) :: this
      integer, intent(in) :: comm
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type (SplitCommunicator) :: split_comm

      integer :: status
      character(:), allocatable :: dir_name
!!$      external :: chdir

      _UNUSED_DUMMY(unusable)

      subcommunicator = MPI_COMM_NULL ! in case of failure
      this%splitter = SimpleCommSplitter(comm, this%cap_options%n_members, this%npes_member, base_name=this%cap_options%ensemble_subdir_prefix)
      split_comm = this%splitter%split(rc=status); _VERIFY(status)
      subcommunicator = split_comm%get_subcommunicator()

      if (this%cap_options%n_members > 1) then
         dir_name = split_comm%get_name()
         status = c_chdir(dir_name)
         _VERIFY(status)
      end if

      _RETURN(_SUCCESS)

   end function create_member_subcommunicator


   subroutine initialize_mpi(this, unusable, rc)
      class (MAPL_Cap), intent(inout) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: ierror, status
      integer :: provided
      integer :: npes_world

      _UNUSED_DUMMY(unusable)

      call MPI_Initialized(this%mpi_already_initialized, ierror)
      _VERIFY(ierror)

      if (.not. this%mpi_already_initialized) then

         call ESMF_InitializePreMPI(_RC)
         call MPI_Init_thread(MPI_THREAD_MULTIPLE, provided, ierror)
         _VERIFY(ierror)
      else
         ! If we are here, then MPI has already been initialized by the user
         ! and we are just using it. But we need to make sure that the user
         ! has initialized MPI with the correct threading level.
         call MPI_Query_thread(provided, ierror)
         _VERIFY(ierror)
      end if
      _ASSERT(provided >= MPI_THREAD_SERIALIZED, 'ESMF requires minimum thread level is MPI_THREAD_SERIALIZED. Please replace MPI lib or use MPI (initialize MPI or launch MPI) in an appropriate way.')

      call MPI_Comm_rank(this%comm_world, this%rank, status)
      _VERIFY(status)
      call MPI_Comm_size(this%comm_world, npes_world, status)
      _VERIFY(status)

      if ( this%cap_options%npes_model == -1) then
         ! just a feed back to cap_options to maintain integrity
          this%cap_options%npes_model = npes_world
      endif
      _ASSERT(npes_world >= this%cap_options%npes_model, "npes_world is smaller than npes_model")

      this%npes_member = npes_world / this%cap_options%n_members


      _RETURN(_SUCCESS)

   end subroutine initialize_mpi


   ! From  https://stackoverflow.com/questions/26730836/change-of-directory-in-fortran-in-a-non-compiler-specific-way
   subroutine chdir(path, err)
      use iso_c_binding
      character(*) :: path
      integer, optional, intent(out) :: err
      integer :: loc_err

      loc_err =  c_chdir(path//c_null_char)

      if (present(err)) err = loc_err

   end subroutine chdir

   subroutine finalize_mpi(this, unusable, rc)
      class (MAPL_Cap), intent(in) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      _UNUSED_DUMMY(unusable)

      call MAPL_Finalize(comm=this%comm_world)
      if (.not. this%mpi_already_initialized) then
         call MPI_Finalize(status)
      end if

      _RETURN(_SUCCESS)

   end subroutine finalize_mpi

   function get_npes_model(this) result(npes_model)
     class(MAPL_Cap), intent(in) :: this
     integer :: npes_model
     npes_model = this%cap_options%npes_model
   end function get_npes_model

   function get_comm_world(this) result(comm_world)
     class(MAPL_Cap), intent(in) :: this
     integer :: comm_world
     comm_world = this%comm_world
   end function get_comm_world

   function get_n_members(this) result(n_members)
     class(MAPL_Cap), intent(in) :: this
     integer :: n_members
     n_members = this%cap_options%n_members
   end function get_n_members

   function get_cap_gc(this) result(cap_gc)
     class(MAPL_Cap), intent(in) :: this
     type(MAPL_CapGridComp) :: cap_gc
     cap_gc = this%cap_gc
   end function get_cap_gc

   function get_cap_rc_file(this) result(cap_rc_file)
     class(MAPL_Cap), intent(in) :: this
     character(len=:), allocatable :: cap_rc_file
     allocate(cap_rc_file, source=this%cap_options%cap_rc_file)
   end function get_cap_rc_file

   function get_egress_file(this) result(egress_file)
     class(MAPL_Cap), intent(in) :: this
     character(len=:), allocatable :: egress_file
     allocate(egress_file, source=this%cap_options%egress_file)
   end function get_egress_file

   function GetPinFlagFromConfig(rcfile, rc) result(pinflag)
     character(len=*), intent(in) :: rcfile
     integer, optional, intent(out) :: rc
     type(ESMF_PIN_Flag) :: pinflag

     character(len=ESMF_MAXSTR) :: pinflag_str
     integer :: status
     type(ESMF_Config) :: config

     config = ESMF_ConfigCreate(_RC)
     call ESMF_ConfigLoadFile(config,rcfile, _RC)
     call ESMF_ConfigGetAttribute(config, value=pinflag_str, &
          label='ESMF_PINFLAG:', default='SSI_CONTIG', _RC)

     select case (pinflag_str)
     case ('PET')
        pinflag = ESMF_PIN_DE_TO_PET
     case ('VAS')
        pinflag = ESMF_PIN_DE_TO_VAS
     case ('SSI')
        pinflag = ESMF_PIN_DE_TO_SSI
     case ('SSI_CONTIG')
        pinflag = ESMF_PIN_DE_TO_SSI_CONTIG
     case default
        _ASSERT(.false.,'Unsupported PIN flag')
     end select

     call ESMF_ConfigDestroy(config, _RC)
     _RETURN(_SUCCESS)
   end function GetPinFlagFromConfig

end module MAPL_CapMod

