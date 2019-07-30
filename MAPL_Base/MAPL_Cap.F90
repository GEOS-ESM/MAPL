#include "MAPL_ErrLog.h"
#include "unused_dummy.H"


module MAPL_CapMod
   use MPI
   use ESMF
   use FLAP
   use MAPL_SimpleCommSplitterMod
   use MAPL_SplitCommunicatorMod
   use MAPL_KeywordEnforcerMod
   use MAPL_CapGridCompMod
   use MAPL_CFIOServerMod
   use MAPL_BaseMod
   use MAPL_ErrorHandlingMod
   use pFIO
   use MAPL_ServerManagerMod
   implicit none
   private

   public :: MAPL_Cap

   type :: MAPL_Cap
      private
      character(:), allocatable :: name
      procedure(), nopass, pointer :: set_services => null()
      integer :: comm_world = MPI_COMM_WORLD
      integer :: rank
      ! command line options
      character(:), allocatable :: egress_file, cap_rc_file
      type (ESMF_LogKind_Flag) :: esmf_logging_mode = ESMF_LOGKIND_NONE
      integer :: npes_model
      integer :: npes_input_server
      integer :: npes_output_server
      integer :: nodes_input_server
      integer :: nodes_output_server
      ! ensemble options
      integer :: n_members = 1
      integer :: npes_member
      character(:), allocatable :: ensemble_subdir_prefix

      ! misc
      logical :: mpi_already_initialized = .false.

      type(MAPL_CapGridComp), public :: cap_gc
      type (SplitCommunicator) :: split_comm
      type(MAPL_Communicators) :: mapl_comm
      type(MpiServer), pointer :: i_server=>null()
      type(MpiServer), pointer :: o_server=>null()
      type(DirectoryService) :: directory_service

   contains
      procedure :: run
      procedure :: run_ensemble
      procedure :: run_member
      procedure :: run_model
      procedure :: step_model

      procedure :: create_member_subcommunicator
      procedure :: initialize_io_servers
      procedure :: initialize_cap_gc
      procedure :: initialize_mpi
      procedure :: finalize_mpi

      ! Methods for processing command line arguments
      procedure, nopass :: add_command_line_options
      procedure :: parse_command_line_arguments
      procedure :: set_esmf_logging_mode

      !setters and getters
      procedure :: set_npes_model
      procedure :: set_npes_input_server
      procedure :: set_npes_output_server
      procedure :: set_nodes_input_server
      procedure :: set_nodes_output_server
      procedure :: set_n_members
      procedure :: set_npes_member
      procedure :: set_comm_world
      procedure :: set_ensemble_subdir_prefix

      procedure :: get_npes_model
      procedure :: get_comm_world
      procedure :: get_n_members
      procedure :: get_mapl_comm
      procedure :: get_cap_gc
      
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
    
   function new_MAPL_Cap(name, set_services, comm, cap_rc_file) result(cap)
      type (MAPL_Cap) :: cap
      character(*), intent(in) :: name
      procedure() :: set_services
      integer, optional, intent(in) :: comm
      character(*), optional, intent(in) :: cap_rc_file

      cap%name = name
      cap%set_services => set_services
      cap%cap_rc_file = cap_rc_file
      
      if (present(comm)) then
         cap%comm_world = comm
      else
         cap%comm_world = MPI_COMM_WORLD
      end if
      
      ! Defaults that can be overridden by command line arguments
      cap%egress_file = 'EGRESS'
      cap%esmf_logging_mode = ESMF_LOGKIND_NONE          

    end function new_MAPL_Cap

   
   ! 1. Start MPI if necessary
   ! 2. Parse command line options
   ! 3. Run the ensemble (default is 1 member)
   ! 4. Finalize MPI if initialized locally.
   subroutine run(this, options, unusable, rc)
      class (MAPL_Cap), intent(inout) :: this
      type (command_line_interface), intent(inout) :: options
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status

      _UNUSED_DUMMY(unusable)

      call this%initialize_mpi(rc=status); _VERIFY(status)
      call this%parse_command_line_arguments(options, rc=status); _VERIFY(status)
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
         call this%initialize_io_servers(subcommunicator, rc = status); _VERIFY(status)
         call this%run_member(rc=status); _VERIFY(status)
      end if

      _RETURN(_SUCCESS)
      
   end subroutine run_ensemble


   subroutine initialize_io_servers(this, comm, unusable, rc)
     use MAPL_CFIOMod
     class (MAPL_Cap), target, intent(inout) :: this
     integer, intent(in) :: comm
     class (KeywordEnforcer), optional, intent(in) :: unusable
     integer, optional, intent(out) :: rc

     type (SimpleCommSplitter) :: splitter

      integer :: status
      logical :: running_o_server ! relevant only for "old" o-server

     _UNUSED_DUMMY(unusable)

     this%directory_service = DirectoryService(comm)
     splitter = SimpleCommSplitter(comm)
     call splitter%add_group(this%npes_model, name='model', isolate_nodes=.true.)

     if (this%npes_input_server > 0 .or. this%nodes_input_server > 0) then
        call splitter%add_group(this%npes_input_server, nnodes=this%nodes_input_server, name='i_server', isolate_nodes=.true.)
     end if

     running_o_server = .false.
     if (this%npes_output_server > 0 .or. this%nodes_output_server > 0) then
        running_o_server = .true.
        call splitter%add_group(this%npes_output_server,nnodes=this%nodes_output_server, name='o_server', isolate_nodes=.true.)
     end if

     this%split_comm = splitter%split(rc=status); _VERIFY(status)

     call fill_mapl_comm(this%split_comm, comm, running_o_server, this%mapl_comm, rc=status)
     _VERIFY(status)
     
     ! New server
!!$      o_server = MpiServer(this%split_comm%subcommunicator, 'o_server')
!!$      call this%directory_service%publish(PortInfo('o_server',o_server), o_server)

     select case (this%split_comm%get_name())
     case ('o_server')
        allocate(this%o_server, source = MpiServer(this%split_comm%get_subcommunicator(), 'o_server'))
        call this%directory_service%publish(PortInfo('o_server',this%o_server), this%o_server)
        call this%directory_service%connect_to_client('o_server', this%o_server)
        call this%o_server%start()
!!!  this is for old_o server
!!$     call MAPL_CFIOServerStart(this%mapl_Comm,rc=status)
!!$     _VERIFY(STATUS)

    case ('i_server')
        allocate(this%i_server, source = MpiServer(this%split_comm%get_subcommunicator(), 'i_server'))
        call this%directory_service%publish(PortInfo('i_server',this%i_server), this%i_server)
        call this%directory_service%connect_to_client('i_server', this%i_server)
        call this%i_server%start()
    case ('model')
        if (this%npes_input_server == 0 .and. this%nodes_input_server == 0) then
           allocate(this%i_server, source = MpiServer(this%split_comm%get_subcommunicator(), 'i_server'))
           call this%directory_service%publish(PortInfo('i_server',this%i_server), this%i_server)
        end if
        if (this%npes_output_server == 0 .and. this%nodes_output_server == 0) then
           allocate(this%o_server, source = MpiServer(this%split_comm%get_subcommunicator(), 'o_server'))
           call this%directory_service%publish(PortInfo('o_server',this%o_server), this%o_server)
        end if

        call init_servers()
        call this%directory_service%connect_to_server('i_server', i_ClientPtr, this%split_comm%get_subcommunicator())  
        call this%directory_service%connect_to_server('o_server', o_ClientPtr, this%split_comm%get_subcommunicator())  
    end select
     
   end subroutine initialize_io_servers

   
   ! This layer splits the communicator to support separate i/o servers
   ! and runs the model via a CapGridComp.
   subroutine run_member(this, rc)
      use MAPL_CFIOMod
      class (MAPL_Cap), intent(inout) :: this
      integer, optional, intent(out) :: rc
      
      integer :: status

      select case(this%split_comm%get_name())
      case('model')
         call this%run_model(this%mapl_comm, rc=status); _VERIFY(status)
         call i_ClientPtr%terminate()
         call o_ClientPtr%terminate()
      end select
                  
   end subroutine run_member


    subroutine fill_mapl_comm(split_comm, gcomm, running_o_server, mapl_comm, unusable, rc)
      type (SplitCommunicator), intent(in) :: split_comm
      integer, intent(in) :: gcomm
      logical, intent(in) :: running_o_server
      type (MAPL_Communicators), intent(OUT) :: mapl_comm
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: comm
      integer :: status
      integer :: grank
      integer :: source
      integer, parameter :: MAPL_TAG_GLOBAL_IOROOT_RANK = 987654
      integer :: stat(MPI_STATUS_SIZE)

      _UNUSED_DUMMY(unusable)

      mapl_comm%mapl%comm = gcomm
      mapl_comm%global%comm = gcomm
      mapl_comm%esmf%comm = MPI_COMM_NULL
      mapl_comm%io%comm = MPI_COMM_NULL

      comm = split_comm%get_subcommunicator()

      select case (split_comm%get_name())
      case ('o_server')
         mapl_comm%io%comm = comm
      case ('model')
         mapl_comm%esmf%comm = comm
      end select

      call fill_comm(mapl_comm%mapl, rc=status); _VERIFY(status)
      call fill_comm(mapl_comm%global, rc=status); _VERIFY(status)
      call fill_comm(mapl_comm%esmf, rc=status); _VERIFY(status)
      call fill_comm(mapl_comm%io, rc=status); _VERIFY(status)

! Find the global rank of root in the io communicator      
      if (running_o_server) then
         if(mapl_comm%io%rank == 0) then
            grank=mapl_comm%global%rank
            call MPI_Send(grank,1,MPI_INTEGER,0,MAPL_TAG_GLOBAL_IOROOT_RANK, &
                          mapl_comm%global%comm, status)
            _VERIFY(status)
         endif

         if(mapl_comm%global%rank == 0) then
            call MPI_Recv(grank,1,MPI_INTEGER,MPI_ANY_SOURCE,&
                          MAPL_TAG_GLOBAL_IOROOT_RANK, mapl_comm%global%comm, &
                          stat, status)
            _VERIFY(status)
            source = stat(MPI_SOURCE)
            _ASSERT(source == grank, "Invalid rank error, most likely due to non-unique tag")
         endif

         call MPI_Bcast(grank,1,MPI_INTEGER,0,mapl_comm%global%comm, status)
         _VERIFY(status)

         mapl_comm%io%root = grank

         call MPI_Bcast(mapl_comm%io%size,1,MPI_INTEGER,grank,mapl_comm%global%comm, status)
         _VERIFY(status)

         call MAPL_CFIOServerInitMpiTypes()
      end if

      return
    end subroutine fill_mapl_comm

    subroutine fill_comm(mcomm, rc)
      type (MAPL_Communicator), intent(inout) :: mcomm
      integer, optional, intent(out) :: rc
      integer :: comm
      integer :: status

      comm = mcomm%comm
      if (comm == MPI_COMM_NULL) then
         mcomm%size = 0
         mcomm%rank = MPI_UNDEFINED
         mcomm%root = MPI_UNDEFINED
      else
         call MPI_Comm_Size(comm, mcomm%size,status)
         _VERIFY(status)
         call MPI_Comm_Rank(comm, mcomm%rank,status)
         _VERIFY(status)
         mcomm%root = 0
      end if
      _RETURN(ESMF_SUCCESS)
    end subroutine fill_comm
      
   subroutine run_model(this, mapl_comm, unusable, rc)
      class (MAPL_Cap), intent(inout) :: this
      type(MAPL_Communicators), intent(in) :: mapl_comm
!!$      integer, intent(in) :: comm
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) ::rc

      type (ESMF_VM) :: vm
      integer :: start_tick, stop_tick, tick_rate
      integer :: status
      _UNUSED_DUMMY(unusable)


      call start_timer()
      call ESMF_Initialize (vm=vm, logKindFlag=this%esmf_logging_mode, mpiCommunicator=mapl_comm%esmf%comm, rc=status)
      _VERIFY(status)

      call this%initialize_cap_gc(mapl_comm)

      call this%cap_gc%set_services(rc = status)
      _VERIFY(status)
      call this%cap_gc%initialize(rc=status)
      _VERIFY(status)
      call this%cap_gc%run(rc=status)
      _VERIFY(status)
      call this%cap_gc%finalize(rc=status)
      _VERIFY(status)
!!$      call ESMF_Finalize(rc=status)
!!$      _VERIFY(status)
      

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
            
            write(OUTPUT_UNIT,'("Model Throughput:",X,F12.3,X,"days per day")') model_days_per_day
         end if
         
      end subroutine report_throughput

   end subroutine run_model

   
   subroutine initialize_cap_gc(this, mapl_comm)
     class(MAPL_Cap), intent(inout) :: this
     type(MAPL_Communicators), intent(in) :: mapl_comm
     call MAPL_CapGridCompCreate(this%cap_gc, mapl_comm, this%set_services, this%cap_rc_file, &
           this%name, this%egress_file)     
   end subroutine initialize_cap_gc
   

   subroutine step_model(this, rc)
     class(MAPL_Cap), intent(inout) :: this
     integer, intent(out) :: rc
     integer :: status
     call this%cap_gc%step(rc = status); _VERIFY(status)
   end subroutine step_model
   

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
      splitter = SimpleCommSplitter(comm, this%n_members, this%npes_member, base_name=this%ensemble_subdir_prefix)
      split_comm = splitter%split(rc=status); _VERIFY(status)
      subcommunicator = split_comm%get_subcommunicator()

      if (this%n_members > 1) then
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

      _RETURN(_SUCCESS)

   contains

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
         call MPI_Finalize(ierror)
         _VERIFY(ierror)
      end if

      _RETURN(_SUCCESS)

   end subroutine finalize_mpi


   ! Static method
   subroutine add_command_line_options(options, unusable, rc)
      type (command_line_interface), intent(inout) :: options
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      _UNUSED_DUMMY(unusable)

      call options%add(switch='--esmf_logtype',                   &
           help='ESMF Logging type',                   &
           required=.false.,                           &
           choices='none,single,multi,multi_on_error', &
           def='none',                                 &
           act='store',                                &
           error=status)
      _VERIFY(status)
      call options%add(switch='--egress_file', &
                  help='Egress file name', &
                  required=.false.,        &
                  def='EGRESS',            &
                  act='store',             &
                  hidden=.true.,           &
                  error=status)
      _VERIFY(status)
      call options%add(switch='--cap_rc',            &
           help='CAP resource file name', &
           required=.false.,              &
           def='CAP.rc',                  &
           act='store',                   &
           error=status)
      _VERIFY(status)


      call options%add(switch='--npes', &
           help='# MPI processes used by model CapGridComp', &
           required=.false., &
           act='store', &
           def='*', &
           error=status)
      _VERIFY(status)

      call options%add(switch='--n_members', &
           help='# MPI processes used by model CapGridComp1', &
           required=.false., &
           act='store', &
           def='1', &
           error=status)
      _VERIFY(status)

      call options%add(switch='--prefix', &
           help='prefix for ensemble subdirectories', &
           required=.false., &
           act='store', &
           def='mem', &
           error=status)
      _VERIFY(status)

      call options%add(switch='--npes_input_server', &
           help='# MPI processes used by input server', &
           required=.false., &
           def='0', &
           exclude = '--nodes_input_server', &
           act='store', &
           error=status)
      _VERIFY(status)
      
      call options%add(switch='--npes_output_server', &
           help='# MPI processes used by output server', &
           required=.false., &
           def='0', &
           exclude = '--nodes_output_server', &
           act='store', &
           error=status)
      _VERIFY(status)

      call options%add(switch='--nodes_input_server', &
           help='# NCCS nodes (28 or more processors ) used by input server', &
           required=.false., &
           def='0', &
           exclude = '--npes_input_server', &
           act='store', &
           error=status)
      _VERIFY(status)
      
      call options%add(switch='--nodes_output_server', &
           help='# NCCS nodes (28 or more processors) used by output server', &
           required=.false., &
           def='0', &
           exclude = '--npes_output_server', &
           act='store', &
           error=status)
      _VERIFY(status)
      _RETURN(_SUCCESS)


   end subroutine add_command_line_options


   subroutine parse_command_line_arguments(this, options, unusable, rc)
      class (MAPL_Cap), intent(inout) :: this
      type (command_line_interface), intent(inout) :: options
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status, ierror
      character(80) :: buffer
      integer :: npes_world

      _UNUSED_DUMMY(unusable)

      call options%get(val=buffer, switch='--egress_file', error=status); _VERIFY(status)
      this%egress_file = trim(buffer)

      call MPI_Comm_size(this%comm_world, npes_world, ierror); _VERIFY(ierror)
      call options%get(val=buffer, switch='--npes', error=status); _VERIFY(status)
      select case (trim(buffer))
      case ('*')
         this%npes_model = npes_world
      case default
         call options%get(val=this%npes_model, switch='--npes', error=status); _VERIFY(status)
      end select

      call options%get(val=this%npes_input_server, switch='--npes_input_server', error=status); _VERIFY(status)
      call options%get(val=this%npes_output_server, switch='--npes_output_server', error=status); _VERIFY(status)
      call options%get(val=this%nodes_input_server, switch='--nodes_input_server', error=status); _VERIFY(status)
      call options%get(val=this%nodes_output_server, switch='--nodes_output_server', error=status); _VERIFY(status)

      call options%get(val=buffer, switch='--esmf_logtype', error=status); _VERIFY(status)
      call this%set_esmf_logging_mode(trim(buffer), rc=status); _VERIFY(status)

      ! Ensemble specific options
      call options%get(val=buffer, switch='--prefix', error=status); _VERIFY(status)
      this%ensemble_subdir_prefix = trim(buffer)
      call options%get(val=this%n_members, switch='--n_members', error=status); _VERIFY(status)
      
      call options%get(val=buffer, switch='--cap_rc', error=status); _VERIFY(status)
      this%cap_rc_file = trim(buffer)
      ! Note: we round down when computing the number of pe's in each
      ! member.  This is either eneough for all members or not enough
      ! for all members.
      this%npes_member = npes_world / this%n_members

   end subroutine parse_command_line_arguments


   subroutine set_esmf_logging_mode(this, flag_name, unusable, rc)
      class (MAPL_Cap), intent(inout) :: this
      character(*), intent(in) :: flag_name
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(unusable)

      select case (flag_name)
      case ('none')
         this%esmf_logging_mode = ESMF_LOGKIND_NONE
      case ('single')
         this%esmf_logging_mode = ESMF_LOGKIND_SINGLE
      case ('multi')
         this%esmf_logging_mode = ESMF_LOGKIND_MULTI
      case ('multi_on_error')
         this%esmf_logging_mode = ESMF_LOGKIND_MULTI_ON_ERROR
      case default
         _FAIL("Unsupported ESMF logging option: "//flag_name)
      end select

      _RETURN(_SUCCESS)
   end subroutine set_esmf_logging_mode


  subroutine set_npes_model(this, npes_model)
    class(MAPL_Cap), intent(inout) :: this
    integer, intent(in) :: npes_model
    this%npes_model = npes_model
  end subroutine set_npes_model

  subroutine set_npes_input_server(this, npes_input_server)
    class(MAPL_Cap), intent(inout) :: this
    integer, intent(in) :: npes_input_server
    this%npes_input_server = npes_input_server
  end subroutine set_npes_input_server

  subroutine set_nodes_input_server(this, nodes_input_server)
    class(MAPL_Cap), intent(inout) :: this
    integer, intent(in) :: nodes_input_server
    this%nodes_input_server = nodes_input_server
  end subroutine set_nodes_input_server

  subroutine set_npes_output_server(this, npes_output_server)
    class(MAPL_Cap), intent(inout) :: this
    integer, intent(in) :: npes_output_server
    this%npes_output_server = npes_output_server
  end subroutine set_npes_output_server

  subroutine set_nodes_output_server(this, nodes_output_server)
    class(MAPL_Cap), intent(inout) :: this
    integer, intent(in) :: nodes_output_server
    this%nodes_output_server = nodes_output_server
  end subroutine set_nodes_output_server

  subroutine set_n_members(this, n_members)
    class(MAPL_Cap), intent(inout) :: this
    integer, intent(in) :: n_members
    this%n_members = n_members
  end subroutine set_n_members

  subroutine set_npes_member(this, npes_member)
    class(MAPL_Cap), intent(inout) :: this
    integer, intent(in) :: npes_member
    this%npes_member = npes_member
  end subroutine set_npes_member

  subroutine set_comm_world(this, comm_world)
    class(MAPL_Cap), intent(inout) :: this
    integer, intent(in) :: comm_world
    this%comm_world = comm_world
  end subroutine set_comm_world

  subroutine set_ensemble_subdir_prefix(this, ensemble_subdir_prefix)
    class(MAPL_Cap), intent(inout) :: this
    character(*), intent(in) :: ensemble_subdir_prefix
    this%ensemble_subdir_prefix = ensemble_subdir_prefix
  end subroutine set_ensemble_subdir_prefix
  
  function get_npes_model(this) result(npes_model)
    class(MAPL_Cap), intent(in) :: this
    integer :: npes_model
    npes_model = this%npes_model
  end function get_npes_model
    
  function get_comm_world(this) result(comm_world)
    class(MAPL_Cap), intent(in) :: this
    integer :: comm_world
    comm_world = this%comm_world
  end function get_comm_world

  function get_n_members(this) result(n_members)
    class(MAPL_Cap), intent(in) :: this
    integer :: n_members
    n_members = this%n_members
  end function get_n_members

  function get_cap_gc(this) result(cap_gc)
    class(MAPL_Cap), intent(in) :: this
    type(MAPL_CapGridComp) :: cap_gc
    cap_gc = this%cap_gc
  end function get_cap_gc

  function get_mapl_comm(this) result(mapl_comm)
    class(MAPL_Cap), intent(in) :: this
    type(MAPL_Communicators) :: mapl_comm
    mapl_comm = this%mapl_comm
  end function get_mapl_comm

  
end module MAPL_CapMod

