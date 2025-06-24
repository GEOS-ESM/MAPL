#include "MAPL_Generic.h"

module ExtDataDriverMod

   use MPI  
   use ESMF
   use MAPL
   use ExtData_DriverGridCompMod, only: ExtData_DriverGridComp, new_ExtData_DriverGridComp
   use ExtDataUtRoot_GridCompMod, only:  ROOT_SetServices => SetServices
   use gFTL_StringVector
   use MAPL_ApplicationSupport
   use MAPL_ServerManager
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
      type (MAPL_CapOptions) :: cap_options
      type(SplitCommunicator) :: split_comm

   contains
      procedure :: run
      procedure :: initialize_io_clients_servers
      procedure :: finalize_io_clients_servers
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
      class(KeywordEnforcer),  optional, intent(in) :: unusable
      class(MAPL_CapOptions), optional, intent(in) :: cap_options
      integer, optional, intent(out) :: rc
     
      integer :: status
      __UNUSED_DUMMY(unusable)

      driver%name = name
      driver%set_services => set_services
      if (present(cap_options)) then
         driver%cap_options = cap_options
      else
         driver%cap_options = MAPL_CapOptions()
      endif
      call driver%initialize_mpi()
      call MAPL_Initialize(comm=MPI_COMM_WORLD, &
           logging_config=driver%cap_options%logging_config, &
           rc=status)
      __VERIFY(status)
      __RETURN(__SUCCESS)
   end function newExtDataDriver

   subroutine run(this,RC)

      class(ExtDataDriver), intent(inout) :: this
      integer,       optional, intent(OUT) :: rc

      type(ESMF_Config)            :: config


      integer                      :: STATUS

      integer                  :: CommCap

      type (ESMF_VM) :: VM

      type(ExtData_DriverGridComp), target :: cap

      integer :: lineCount, columnCount,i,rank
      character(len=ESMF_MAXSTR) :: ctemp
      character(len=:), pointer :: cname
      type(StringVector) :: cases
      type(StringVectorIterator) :: iter  
      type(SplitCommunicator) :: split_comm

      CommCap = MPI_COMM_WORLD

      call this%initialize_io_clients_servers(commCap, rc = status); __VERIFY(status)
      call this%cap_server%get_splitcomm(split_comm)
      select case(split_comm%get_name())
      case('model')
         call ESMF_Initialize (vm=vm, logKindFlag=this%cap_options%esmf_logging_mode, &
              & mpiCommunicator=split_comm%get_subcommunicator(), rc=status)
         __VERIFY(STATUS)

         config = ESMF_ConfigCreate(rc=status)
         __VERIFY(status)
         call ESMF_ConfigLoadFile   ( config, 'CAP.rc', rc=STATUS )
         __VERIFY(status)
         call ESMF_ConfigGetDim(config,lineCount,columnCount,label='CASES::',rc=status)
         __VERIFY(status)
         call ESMF_ConfigFindLabel(config,label='CASES::',rc=status)
         __VERIFY(status)
         do i=1,lineCount
            call ESMF_ConfigNextLine(config,rc=status)
            __VERIFY(status)
            call ESMF_ConfigGetAttribute(config,ctemp,rc=status)
            __VERIFY(status)
            call cases%push_back(trim(ctemp))
         enddo
         call ESMF_ConfigDestroy(config, rc=status)
         __VERIFY(status)

         iter = cases%begin()
         do while (iter /= cases%end())

            if (mapl_am_I_root()) write(*,*)"Running new case"
            cname => iter%get()
            cap = new_ExtData_DriverGridComp(root_setservices, name=this%name, configFileName=cname)
            call cap%set_services(rc = status)
            __VERIFY(status)
            call cap%initialize(rc = status)
            __VERIFY(status)

            call cap%run(rc=status)
            __VERIFY(status)

            call cap%finalize(rc = status)
            __VERIFY(status)

            call iter%next()
         enddo

      end select

!  Finalize framework
!  ------------------

     call MPI_Comm_Rank(CommCap,rank,status)
     __VERIFY(status)
     if (rank==0) then
        close(99)
        open(99,file='egress',form='formatted')
        close(99)
     end if
   
      call this%finalize_io_clients_servers()
      call MAPL_Finalize(rc=status)
      __VERIFY(status) 
      call mpi_finalize(status)
      __VERIFY(STATUS)

      __RETURN(ESMF_SUCCESS)


   end subroutine run

   subroutine initialize_io_clients_servers(this, comm, unusable, rc)
     use MAPL_CFIOMod
     class (ExtDataDriver), target, intent(inout) :: this
     integer, intent(in) :: comm
     class (KeywordEnforcer), optional, intent(in) :: unusable
     integer, optional, intent(out) :: rc
  
     integer :: status

     __UNUSED_DUMMY(unusable)

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
     __VERIFY(status)
     __RETURN(__SUCCESS)

   end subroutine initialize_io_clients_servers

   subroutine finalize_io_clients_servers(this, unusable, rc)
     class (ExtDataDriver), target, intent(inout) :: this
     class (KeywordEnforcer), optional, intent(in) :: unusable
     integer, optional, intent(out) :: rc
     type(SplitCommunicator) :: split_comm

     __UNUSED_DUMMY(unusable)
     call this%cap_server%get_splitcomm(split_comm)
     select case(split_comm%get_name())
     case('model')
        call i_Clients%terminate()
        call o_Clients%terminate()
     end select
     call this%cap_server%finalize()
     __RETURN(__SUCCESS)

   end subroutine finalize_io_clients_servers

   subroutine initialize_mpi(this, unusable, rc) 
      class (ExtDataDriver), intent(inout) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: ierror
      integer :: npes_world

      __UNUSED_DUMMY(unusable)

      call MPI_Init(ierror)
      __VERIFY(ierror)

      this%comm_world=MPI_COMM_WORLD
      call MPI_Comm_rank(this%comm_world, this%rank, ierror); __VERIFY(ierror)
      call MPI_Comm_size(this%comm_world, npes_world, ierror); __VERIFY(ierror)

      if ( this%cap_options%npes_model == -1) then
         ! just a feed back to cap_options to maintain integrity
          this%cap_options%npes_model = npes_world
      endif
      __ASSERT(npes_world >= this%cap_options%npes_model, "npes_world is smaller than npes_model")

      __RETURN(__SUCCESS)

   end subroutine initialize_mpi


end module ExtDataDriverMod

