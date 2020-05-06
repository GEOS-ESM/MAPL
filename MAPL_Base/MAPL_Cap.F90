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
   use MAPL_Profiler, only: get_global_time_profiler, BaseProfiler, TimeProfiler
   use MAPL_ioClientsMod
   use MAPL_CapOptionsMod
   use pflogger, only: logging
   use pflogger, only: Logger
   implicit none
   private

   public :: MAPL_Cap
   public :: init_MAPL_Cap

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
      type(SplitCommunicator)  :: split_comm
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
      procedure :: initialize_io_clients_servers
      procedure :: initialize_cap_gc
      procedure :: initialize_mpi
      procedure :: finalize_mpi


      !getters
      procedure :: get_npes_model
      procedure :: get_comm_world
      procedure :: get_n_members
      procedure :: get_mapl_comm
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
      type(Logger), pointer :: lgr

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

      call initialize_pflogger()

      _RETURN(_SUCCESS)     
      _UNUSED_DUMMY(unusable)

    contains

       subroutine initialize_pflogger()
          use pflogger, only: pfl_initialize => initialize
          use pflogger, only: StreamHandler, FileHandler, HandlerVector
          use pflogger, only: MpiLock, MpiFormatter
          use pflogger, only: INFO, WARNING
          use, intrinsic :: iso_fortran_env, only: OUTPUT_UNIT
          type (HandlerVector) :: handlers
          type (StreamHandler) :: console
          type (FileHandler) :: file_handler
          integer :: level

          logical :: has_logging_config

          call pfl_initialize()

          has_logging_config = .false. ! unless

          if (allocated(cap%cap_options%logging_config)) then
             if(cap%cap_options%logging_config /= '') then
                has_logging_config = .true.
             end if
          end if

          if (has_logging_config) then
             call logging%load_file(cap%cap_options%logging_config)
          else

             console = StreamHandler(OUTPUT_UNIT)
             call console%set_level(INFO)
             call console%set_formatter(MpiFormatter(cap%comm_world, fmt='%(short_name)a10~: %(message)a'))
             call handlers%push_back(console)

             file_handler = FileHandler('warnings_and_errors.log')
             call file_handler%set_level(WARNING)
             call file_handler%set_formatter(MpiFormatter(cap%comm_world, fmt='pe=%(mpi_rank)i5.5~: %(short_name)a~: %(message)a'))
             call file_handler%set_lock(MpiLock(cap%comm_world))
             call handlers%push_back(file_handler)
             
             if (cap%rank == 0) then
                level = INFO
             else
                level = WARNING
             end if

             call logging%basic_config(level=level, handlers=handlers, rc=status)
             _VERIFY(status)
             
             if (cap%rank == 0) then
                lgr => logging%get_logger('MAPL')
                call lgr%warning('No configure file specified for logging layer.  Using defaults.')
             end if

          end if

       end subroutine initialize_pflogger
     
    end function new_MAPL_Cap

   subroutine init_MAPL_Cap(cap, name, set_services, unusable, cap_options, rc)
      type (MAPL_Cap), intent(out) :: cap
      character(*), intent(in) :: name
      procedure() :: set_services
      class (KeywordEnforcer),  optional, intent(in) :: unusable
      class ( MAPL_CapOptions), optional, intent(in) :: cap_options
      integer, optional, intent(out) :: rc
      integer :: status
      type(Logger), pointer :: lgr

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

      call initialize_pflogger()

      _RETURN(_SUCCESS)     
      _UNUSED_DUMMY(unusable)

    contains

       subroutine initialize_pflogger()
          use pflogger, only: pfl_initialize => initialize
          use pflogger, only: StreamHandler, FileHandler, HandlerVector
          use pflogger, only: MpiLock, MpiFormatter
          use pflogger, only: INFO, WARNING
          use, intrinsic :: iso_fortran_env, only: OUTPUT_UNIT
          type (HandlerVector) :: handlers
          type (StreamHandler) :: console
          type (FileHandler) :: file_handler
          integer :: level

          logical :: has_logging_config

          call pfl_initialize()

          has_logging_config = .false. ! unless

          if (allocated(cap%cap_options%logging_config)) then
             if(cap%cap_options%logging_config /= '') then
                has_logging_config = .true.
             end if
          end if

          if (has_logging_config) then
             call logging%load_file(cap%cap_options%logging_config)
          else

             console = StreamHandler(OUTPUT_UNIT)
             call console%set_level(INFO)
             call console%set_formatter(MpiFormatter(cap%comm_world, fmt='%(short_name)a10~: %(message)a'))
             call handlers%push_back(console)

             file_handler = FileHandler('warnings_and_errors.log')
             call file_handler%set_level(WARNING)
             call file_handler%set_formatter(MpiFormatter(cap%comm_world, fmt='pe=%(mpi_rank)i5.5~: %(short_name)a~: %(message)a'))
             call file_handler%set_lock(MpiLock(cap%comm_world))
             call handlers%push_back(file_handler)
             
             if (cap%rank == 0) then
                level = INFO
             else
                level = WARNING
             end if

             call logging%basic_config(level=level, handlers=handlers, rc=status)
             _VERIFY(status)
             
             if (cap%rank == 0) then
                lgr => logging%get_logger('MAPL')
                call lgr%warning('No configure file specified for logging layer.  Using defaults.')
             end if

          end if

       end subroutine initialize_pflogger
     
    end subroutine init_MAPL_Cap

   
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
      logical, save :: first = .true.

      _UNUSED_DUMMY(unusable)
      
      subcommunicator = this%create_member_subcommunicator(this%comm_world, rc=status); _VERIFY(status)
      if (subcommunicator /= MPI_COMM_NULL) then
         if (first) then
           call this%initialize_io_clients_servers(subcommunicator, rc = status); _VERIFY(status)
           first = .false.
         end if
         call this%run_member(rc=status); _VERIFY(status)
      end if

      _RETURN(_SUCCESS)
      
   end subroutine run_ensemble


   subroutine initialize_io_clients_servers(this, comm, unusable, rc)
     use MAPL_CFIOMod
     class (MAPL_Cap), target, intent(inout) :: this
     integer, intent(in) :: comm
     class (KeywordEnforcer), optional, intent(in) :: unusable
     integer, optional, intent(out) :: rc

     type (SimpleCommSplitter) :: splitter
     integer :: status, i, rank
     logical :: running_old_o_server ! relevant only for "old" o-server
     character(len=:), allocatable :: s_name

     type(ClientThread), pointer :: clientPtr

     _UNUSED_DUMMY(unusable)

     this%directory_service = DirectoryService(comm)
     splitter = SimpleCommSplitter(comm)
     call splitter%add_group(npes=this%cap_options%npes_model, name='model', isolate_nodes=.true.)

     if (this%cap_options%npes_input_server(1) > 0) then
        do i = 1, this%cap_options%n_iserver_group
           s_name ='i_server'//trim(i_to_string(i))
           call splitter%add_group(npes=this%cap_options%npes_input_server(i), name=s_name, isolate_nodes=.true.)
        enddo
     elseif (this%cap_options%nodes_input_server(1) > 0) then
        do i = 1, this%cap_options%n_iserver_group
           s_name ='i_server'//trim(i_to_string(i))
           call splitter%add_group(nnodes=this%cap_options%nodes_input_server(i), name=s_name, isolate_nodes=.true.)
        enddo
     end if

     running_old_o_server = .false.

     if (this%cap_options%npes_output_server(1) > 0 ) then
        running_old_o_server = (this%cap_options%n_oserver_group ==1) ! otherwise need to combine all the o-server groups to form a group used by old_server
        do i = 1, this%cap_options%n_oserver_group
          s_name ='o_server'//trim(i_to_string(i))
          call splitter%add_group(npes=this%cap_options%npes_output_server(i), name=s_name, isolate_nodes=.true.)
        enddo
     else if(this%cap_options%nodes_output_server(1) > 0) then
        running_old_o_server = (this%cap_options%n_oserver_group ==1) ! otherwise need to combine all the o-server groups to form a group used by old_server
        do i = 1, this%cap_options%n_oserver_group
          s_name ='o_server'//trim(i_to_string(i))
          call splitter%add_group(nnodes=this%cap_options%nodes_output_server(i), name=s_name, isolate_nodes=.true.)
        enddo
     endif

     this%split_comm = splitter%split(rc=status); _VERIFY(status)

     call fill_mapl_comm(this%split_comm, comm, running_old_o_server, this%mapl_comm, rc=status)
     _VERIFY(status)
     
     s_name = this%split_comm%get_name()

     if ( index(s_name, 'model') /=0 ) then
        if (this%cap_options%npes_input_server(1) == 0 .and. this%cap_options%nodes_input_server(1) == 0) then
           allocate(this%i_server, source = MpiServer(this%split_comm%get_subcommunicator(), 'i_server'//trim(i_to_string(1))))
           call this%directory_service%publish(PortInfo('i_server'//trim(i_to_string(1)), this%i_server), this%i_server)
        end if
        if (this%cap_options%npes_output_server(1) == 0 .and. this%cap_options%nodes_output_server(1) == 0) then
           allocate(this%o_server, source = MpiServer(this%split_comm%get_subcommunicator(), 'o_server'//trim(i_to_string(1))))
           call this%directory_service%publish(PortInfo('o_server'//trim(i_to_string(1)), this%o_server), this%o_server)
        end if
        call io_client%init_io_clients(ni = this%cap_options%n_iserver_group, no = this%cap_options%n_oserver_group )
     endif

     ! establish i_server group one by one
     do i = 1, this%cap_options%n_iserver_group
        
        if ( trim(s_name) =='i_server'//trim(i_to_string(i)) ) then
           allocate(this%i_server, source = MpiServer(this%split_comm%get_subcommunicator(), s_name))
           call this%directory_service%publish(PortInfo(s_name,this%i_server), this%i_server)
           call this%directory_service%connect_to_client(s_name, this%i_server)
           call MPI_Comm_Rank(this%split_comm%get_subcommunicator(),rank,status)
           if (rank == 0 .and. this%cap_options%nodes_input_server(i) /=0 ) then
              write(*,'(A,I0,A)')"Starting pFIO input server on ",this%cap_options%nodes_input_server(i)," nodes"
           else if (rank==0 .and. this%cap_options%npes_input_server(i) /=0 ) then
              write(*,'(A,I0,A)')"Starting pFIO input server on ",this%cap_options%npes_input_server(i)," pes"
           end if
        endif

        if ( index(s_name, 'model') /=0 ) then
           clientPtr => i_Clients%current()
           call this%directory_service%connect_to_server('i_server'//trim(i_to_string(i)), clientPtr, this%split_comm%get_subcommunicator())
           call i_Clients%next()
        endif 

        call mpi_barrier(comm, status) 

     enddo

     ! establish o_server group one by one
     do i = 1, this%cap_options%n_oserver_group
        
        if ( trim(s_name) =='o_server'//trim(i_to_string(i)) ) then
           allocate(this%o_server, source = MpiServer(this%split_comm%get_subcommunicator(), s_name))
           call this%directory_service%publish(PortInfo(s_name,this%o_server), this%o_server)
           call this%directory_service%connect_to_client(s_name, this%o_server)
           call MPI_Comm_Rank(this%split_comm%get_subcommunicator(),rank,status)
           if (rank == 0 .and. this%cap_options%nodes_output_server(i) /=0 ) then
              write(*,'(A,I0,A)')"Starting pFIO output server on ",this%cap_options%nodes_output_server(i)," nodes"
           else if (rank==0 .and. this%cap_options%npes_output_server(i) /=0 ) then
              write(*,'(A,I0,A)')"Starting pFIO output server on ",this%cap_options%npes_output_server(i)," pes"
           end if
        endif

        if ( index(s_name, 'model') /=0 ) then
           clientPtr => o_Clients%current()
           call this%directory_service%connect_to_server('o_server'//trim(i_to_string(i)), clientPtr, this%split_comm%get_subcommunicator())
           call o_Clients%next()
        endif 

        call mpi_barrier(comm, status) 

     enddo

     if ( index(s_name, 'o_server') /=0 ) then
        call this%o_server%start()
     endif

     if ( index(s_name, 'i_server') /=0 ) then
        call this%i_server%start()
     endif

     if ( index(s_name, 'model') /=0 ) then
        call i_Clients%set_current(1) ! set current to be the first
        call o_Clients%set_current(1) ! set current to be the first
        if (this%cap_options%npes_output_server(1) >0) then
           call io_client%set_size(no = this%cap_options%npes_output_server,rc=status)
        else if (this%cap_options%nodes_output_server(1)>0) then
           call io_client%set_size(no = this%cap_options%nodes_output_server,rc=status)
        endif
        _VERIFY(status)
     end if
     
   end subroutine initialize_io_clients_servers

   
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
         call i_Clients%terminate()
         call o_Clients%terminate()
      end select
                  
   end subroutine run_member


    subroutine fill_mapl_comm(split_comm, gcomm, running_old_o_server, mapl_comm, unusable, rc)
      type (SplitCommunicator), intent(in) :: split_comm
      integer, intent(in) :: gcomm
      logical, intent(in) :: running_old_o_server
      type (MAPL_Communicators), intent(OUT) :: mapl_comm
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: comm
      integer :: status
      integer :: grank
      integer :: source
      integer, parameter :: MAPL_TAG_GLOBAL_IOROOT_RANK = 987
      integer :: stat(MPI_STATUS_SIZE)
      character(len=:), allocatable :: s_name

      _UNUSED_DUMMY(unusable)

      mapl_comm%mapl%comm = gcomm
      mapl_comm%global%comm = gcomm
      mapl_comm%esmf%comm = MPI_COMM_NULL
      mapl_comm%io%comm = MPI_COMM_NULL

      comm = split_comm%get_subcommunicator()
      s_name = split_comm%get_name()

      if (index(s_name,'o_server') /=0) then
         mapl_comm%io%comm = comm
      endif

      if (index(s_name,'model') /=0) then
         mapl_comm%esmf%comm = comm
      endif

      call fill_comm(mapl_comm%mapl, rc=status); _VERIFY(status)
      call fill_comm(mapl_comm%global, rc=status); _VERIFY(status)
      call fill_comm(mapl_comm%esmf, rc=status); _VERIFY(status)
      call fill_comm(mapl_comm%io, rc=status); _VERIFY(status)

! Find the global rank of root in the io communicator      
! WJ notes:  If the users want to use the old server, use defalut n_oserver_group = 1
!     
      if (running_old_o_server) then
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
!
!     profiler
!  
      class (BaseProfiler), pointer :: t_p

      _UNUSED_DUMMY(unusable)

      t_p => get_global_time_profiler()
      t_p = TimeProfiler('All', comm_world = mapl_comm%esmf%comm)
      call t_p%start()

      call start_timer()
      call ESMF_Initialize (vm=vm, logKindFlag=this%cap_options%esmf_logging_mode, mpiCommunicator=mapl_comm%esmf%comm, rc=status)
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

      !call ESMF_Finalize(rc=status)
      !_VERIFY(status)
      call stop_timer()

      call t_p%stop()
      call report_profiling()
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
            
            write(OUTPUT_UNIT,'("Model Throughput:",X,F12.3,X,"days per day")') model_days_per_day
         end if
         
      end subroutine report_throughput

      subroutine report_profiling(rc)
         use MAPL_Profiler
         integer, optional, intent(out) :: rc
         type (ProfileReporter) :: reporter
         integer :: i
         character(:), allocatable :: report_lines(:)
         type (MultiColumn) :: inclusive
         type (MultiColumn) :: exclusive
         integer :: npes, my_rank, rank, ierror
         character(1) :: empty(0)

         reporter = ProfileReporter(empty)
         call reporter%add_column(NameColumn(50, separator= " "))
         call reporter%add_column(FormattedTextColumn('#-cycles','(i5.0)', 5, NumCyclesColumn(),separator='-'))

         inclusive = MultiColumn(['Inclusive'], separator='=')
         call inclusive%add_column(FormattedTextColumn(' T (sec) ','(f9.3)', 9, InclusiveColumn(), separator='-'))
         call inclusive%add_column(FormattedTextColumn('   %  ','(f6.2)', 6, PercentageColumn(InclusiveColumn(),'MAX'),separator='-'))
         call reporter%add_column(inclusive)

         exclusive = MultiColumn(['Exclusive'], separator='=')
         call exclusive%add_column(FormattedTextColumn(' T (sec) ','(f9.3)', 9, ExclusiveColumn(), separator='-'))
         call exclusive%add_column(FormattedTextColumn('   %  ','(f6.2)', 6, PercentageColumn(ExclusiveColumn()), separator='-'))
         call reporter%add_column(exclusive)

!!$      call reporter%add_column(FormattedTextColumn('  std. dev ','(f12.4)', 12, StdDevColumn()))
!!$      call reporter%add_column(FormattedTextColumn('  rel. dev ','(f12.4)', 12, StdDevColumn(relative=.true.)))
!!$      call reporter%add_column(FormattedTextColumn('  max cyc ','(f12.8)', 12, MaxCycleColumn()))
!!$      call reporter%add_column(FormattedTextColumn('  min cyc ','(f12.8)', 12, MinCycleColumn()))
!!$      call reporter%add_column(FormattedTextColumn(' mean cyc','(f12.8)', 12, MeanCycleColumn()))
!!$         call mem_reporter%add_column(NameColumn(50,separator='-'))
!!$         call mem_reporter%add_column(MemoryTextColumn(['RSS'],'(i10,1x,a2)',13, InclusiveColumn(),separator='-'))
!!$         call mem_reporter%add_column(MemoryTextColumn(['Cyc RSS'],'(i10,1x,a2)',13, MeanCycleColumn(),separator='-'))

!!$         report_lines = reporter%generate_report(get_global_time_profiler())

         call MPI_Comm_size(mapl_comm%esmf%comm, npes, ierror)
         call MPI_Comm_Rank(mapl_comm%esmf%comm, my_rank, ierror)

         if (my_rank == 0) then
               report_lines = reporter%generate_report(t_p)
               write(*,'(a,1x,i0)')'Report on process: ', my_rank
               do i = 1, size(report_lines)
                  write(*,'(a)') report_lines(i)
               end do
          end if
          call MPI_Barrier(mapl_comm%esmf%comm, ierror)

      end subroutine report_profiling

   end subroutine run_model
   
   subroutine initialize_cap_gc(this, mapl_comm)
     class(MAPL_Cap), intent(inout) :: this
     type(MAPL_Communicators), intent(in) :: mapl_comm
     call MAPL_CapGridCompCreate(this%cap_gc, mapl_comm, this%set_services, this%get_cap_rc_file(), &
           this%name, this%get_egress_file())     
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

      integer :: ierror, local_comm_world
      _UNUSED_DUMMY(unusable)

      if (.not. this%mpi_already_initialized) then
#ifdef BUILD_TYPE_IS_NOT_DEBUG
         ! MPT 2.17 has a bug with its interface to MPI_Comm_set_errhandler
         ! defining comm as inout instead of in.
         local_comm_world = this%comm_world
         ! Intel MPI at NCCS seems to have spurious MPI_Finalize errors that do
         ! not affect the answer or even the finalize step. This call suppresses
         ! the errors.
         call MPI_Comm_set_errhandler(local_comm_world,MPI_ERRORS_RETURN,ierror)
         _VERIFY(ierror)
#endif
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

   function get_mapl_comm(this) result(mapl_comm)
     class(MAPL_Cap), intent(in) :: this
     type(MAPL_Communicators) :: mapl_comm
     mapl_comm = this%mapl_comm
   end function get_mapl_comm

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

