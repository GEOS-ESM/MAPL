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
   implicit none
   private

   public :: MAPL_Cap

   type :: MAPL_Cap
      private
      character(:), allocatable :: name
      procedure(), nopass, pointer :: set_services => null()
      integer :: comm_world 
      integer :: rank
      integer :: npes_member
 
      class (MAPL_CapOptions), allocatable :: cap_options
      ! misc
      logical :: mpi_already_initialized = .false.

      type(MAPL_CapGridComp), public :: cap_gc
      type(ServerManager) :: cap_server

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
      module procedure new_MAPL_Cap
   end interface MAPL_Cap


   interface
      integer function c_chdir(path) bind(C,name="chdir")
         use iso_c_binding
         character(kind=c_char) :: path(*)
      end function c_chdir
   end interface

contains
    
   function new_MAPL_Cap(name, set_services, unusable, cap_options, rc) result(cap)
      type (MAPL_Cap) :: cap
      character(*), intent(in) :: name
      procedure() :: set_services
      class (KeywordEnforcer),  optional, intent(in) :: unusable
      class ( MAPL_CapOptions), optional, intent(in) :: cap_options
      integer, optional, intent(out) :: rc
      integer :: status

      cap%name = name
      cap%set_services => set_services

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

    end function new_MAPL_Cap

   
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
      type(SplitCommunicator) :: split_comm

      _UNUSED_DUMMY(unusable)
      
      subcommunicator = this%create_member_subcommunicator(this%comm_world, rc=status); _VERIFY(status)
      if (subcommunicator /= MPI_COMM_NULL) then
         call this%initialize_io_clients_servers(subcommunicator, rc = status); _VERIFY(status)
         call this%cap_server%get_splitcomm(split_comm)
         call this%run_member(rc=status); _VERIFY(status)
         call this%cap_server%finalize()
      end if

      _RETURN(_SUCCESS)
      
   end subroutine run_ensemble


   subroutine finalize_io_clients_servers(this, unusable, rc)
     class (MAPL_Cap), target, intent(inout) :: this
     class (KeywordEnforcer), optional, intent(in) :: unusable
     integer, optional, intent(out) :: rc

     _UNUSED_DUMMY(unusable)
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
         npes_output_backend=this%cap_options%npes_output_backend, &
         isolate_nodes = this%cap_options%isolate_nodes, &
         fast_oclient  = this%cap_options%fast_oclient, &
         rc=status)
     _VERIFY(status)

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
         call i_Clients%terminate()
         call o_Clients%terminate()
      end select
                  
   end subroutine run_member


   subroutine run_model(this, comm, unusable, rc)
      use pFlogger, only: logging, Logger
      class (MAPL_Cap), intent(inout) :: this
      integer, intent(in) :: comm
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) ::rc

      type (ESMF_VM) :: vm
      integer :: start_tick, stop_tick, tick_rate
      integer :: status
      class(Logger), pointer :: lgr
      
      _UNUSED_DUMMY(unusable)

      call start_timer()
      call ESMF_Initialize (vm=vm, logKindFlag=this%cap_options%esmf_logging_mode, mpiCommunicator=comm, rc=status)
      _VERIFY(status)

      call this%initialize_cap_gc()

      call this%cap_gc%set_services(rc = status)
      _VERIFY(status)
      call this%cap_gc%initialize(rc=status)
      _VERIFY(status)
      call this%cap_gc%run(rc=status)
      _VERIFY(status)
      call this%cap_gc%finalize(rc=status)
      _VERIFY(status)

      !call ESMF_Finalize(rc=status)
      !_VERIFY(status)
      call stop_timer()

      ! W.J note : below reporting will be remove soon
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
         use, intrinsic :: iso_fortran_env, only: REAL64, OUTPUT_UNIT
         integer, optional, intent(out) :: rc

         integer :: rank, ierror
         real(kind=REAL64) :: model_duration, wall_time, model_days_per_day

         call MPI_Comm_rank(this%comm_world, rank, ierror)
         _VERIFY(ierror)

         if (rank == 0) then
            model_duration = this%cap_gc%get_model_duration()
            wall_time = (stop_tick - start_tick) / real(tick_rate, kind=REAL64)

            model_days_per_day = model_duration / wall_time


            lgr => logging%get_logger('MAPL')
            call lgr%info("Model Throughput: %f12.3 days per day", model_days_per_day)
         end if
         
      end subroutine report_throughput

   end subroutine run_model
   
   subroutine initialize_cap_gc(this)
     class(MAPL_Cap), intent(inout) :: this

     call MAPL_CapGridCompCreate(this%cap_gc, this%set_services, this%get_cap_rc_file(), &
           this%name, this%get_egress_file())     
   end subroutine initialize_cap_gc
   

   subroutine step_model(this, rc)
     class(MAPL_Cap), intent(inout) :: this
     integer, intent(out) :: rc
     integer :: status
     call this%cap_gc%step(rc = status); _VERIFY(status)
   end subroutine step_model
  
   subroutine rewind_model(this, time, rc)
     class(MAPL_Cap), intent(inout) :: this
     type(ESMF_Time), intent(inout) :: time
     integer, intent(out) :: rc
     integer :: status
     call this%cap_gc%rewind_clock(time,rc = status); _VERIFY(status)
   end subroutine rewind_model 

   integer function create_member_subcommunicator(this, comm, unusable, rc) result(subcommunicator)
      class (MAPL_Cap), intent(in) :: this
      integer, intent(in) :: comm
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      
      type (SimpleCommSplitter) :: splitter
      type (SplitCommunicator) :: split_comm

      integer :: status
      character(:), allocatable :: dir_name
!!$      external :: chdir

      _UNUSED_DUMMY(unusable)
      
      subcommunicator = MPI_COMM_NULL ! in case of failure
      splitter = SimpleCommSplitter(comm, this%cap_options%n_members, this%npes_member, base_name=this%cap_options%ensemble_subdir_prefix)
      split_comm = splitter%split(rc=status); _VERIFY(status)
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

      integer :: ierror
      integer :: provided
      integer :: npes_world

      _UNUSED_DUMMY(unusable)

      call MPI_Initialized(this%mpi_already_initialized, ierror)
      _VERIFY(ierror)

      if (.not. this%mpi_already_initialized) then
!!$         call MPI_Init_thread(MPI_THREAD_MULTIPLE, provided, ierror)
!!$         _ASSERT(provided == MPI_THREAD_MULTIPLE, 'MPI_THREAD_MULTIPLE not supporte by this MPI.')
         call MPI_Init_thread(MPI_THREAD_SINGLE, provided, ierror)
         _VERIFY(ierror)
         _ASSERT(provided == MPI_THREAD_SINGLE, "MPI_THREAD_SINGLE not supported by this MPI.")
      end if

      call MPI_Comm_rank(this%comm_world, this%rank, ierror); _VERIFY(ierror)
      call MPI_Comm_size(this%comm_world, npes_world, ierror); _VERIFY(ierror)

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

      integer :: ierror
      _UNUSED_DUMMY(unusable)

      if (.not. this%mpi_already_initialized) then
         call MAPL_Finalize(comm=this%comm_world)
         call MPI_Finalize(ierror)
         _VERIFY(ierror)
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

end module MAPL_CapMod

