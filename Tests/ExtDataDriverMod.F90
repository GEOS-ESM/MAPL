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
      type(MpiServer), pointer :: i_server=>null()
      type(MpiServer), pointer :: o_server=>null()
      type(DirectoryService) :: directory_service
      class (MAPL_CapOptions), allocatable :: cap_options
      type(SplitCommunicator) :: split_comm
      type(MAPL_Communicators) :: mapl_comm

   contains
      procedure :: run
      procedure :: initialize_io_clients_servers
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

      call this%initialize_io_clients_servers(commCap, rc = status); _VERIFY(status)
      call this%cap_server%get_splitcomm(split_comm)
      call fill_mapl_comm(split_comm, commCap, this%mapl_comm, rc=status)
      select case(split_comm%get_name())
      case('model')
         call ESMF_Initialize (vm=vm, logKindFlag=this%cap_options%esmf_logging_mode, mpiCommunicator=this%mapl_comm%esmf%comm, rc=status)
         _VERIFY(STATUS)

         config = ESMF_ConfigCreate(rc=status)
         _VERIFY(status)
         call ESMF_ConfigLoadFile   ( config, 'CAP.rc', rc=STATUS )
         _VERIFY(status)
         call ESMF_ConfigGetDim(config,lineCount,columnCount,label='CASES::',rc=status)
         _VERIFY(status)
         call ESMF_ConfigFindLabel(config,label='CASES::',rc=status)
         _VERIFY(status)
         do i=1,lineCount
            call ESMF_ConfigNextLine(config,rc=status)
            _VERIFY(status)
            call ESMF_ConfigGetAttribute(config,ctemp,rc=status)
            _VERIFY(status)
            call cases%push_back(trim(ctemp))
         enddo
         call ESMF_ConfigDestroy(config, rc=status)
         _VERIFY(status)

         iter = cases%begin()
         do while (iter /= cases%end())

            if (mapl_am_I_root()) write(*,*)"Running new case"
            cname => iter%get()
            cap = new_ExtData_DriverGridComp(root_setservices, this%mapl_comm, name=this%name, configFileName=cname)
            call cap%set_services(rc = status)
            _VERIFY(status)
            call cap%initialize(rc = status)
            _VERIFY(status)

            call cap%run(rc=status)
            _VERIFY(status)

            call cap%finalize(rc = status)
            _VERIFY(status)

            call iter%next()
         enddo

         call this%cap_server%finalize()
      end select

      call MPI_Barrier(CommCap,status)
      _VERIFY(STATUS) 
!  Finalize framework
!  ------------------

     call MPI_Comm_Rank(CommCap,rank,status)
     _VERIFY(status)
     if (rank==0) then
        close(99)
        open(99,file='egress',form='formatted')
        close(99)
     end if
    
      call MAPL_Finalize(rc=status)
      _VERIFY(status) 
      call mpi_finalize(status)
      _VERIFY(STATUS)

      _RETURN(ESMF_SUCCESS)


   end subroutine run

   subroutine initialize_io_clients_servers(this, comm, unusable, rc)
     use MAPL_CFIOMod
     class (ExtDataDriver), target, intent(inout) :: this
     integer, intent(in) :: comm
     class (KeywordEnforcer), optional, intent(in) :: unusable
     integer, optional, intent(out) :: rc
  
     integer :: status

     call this%cap_server%initialize(comm, &
         application_size=this%cap_options%npes_model, &
         nodes_input_server=this%cap_options%nodes_input_server, &
         nodes_output_server=this%cap_options%nodes_output_server, &
         npes_input_server=this%cap_options%npes_input_server, &
         npes_output_server=this%cap_options%npes_output_server, &
         rc=status)
     _VERIFY(status)
     _RETURN(_SUCCESS)

   end subroutine initialize_io_clients_servers

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

    subroutine fill_mapl_comm(split_comm, gcomm, mapl_comm, unusable, rc) 
      type (SplitCommunicator), intent(in) :: split_comm
      integer, intent(in) :: gcomm
      type (MAPL_Communicators), intent(OUT) :: mapl_comm
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: comm
      integer :: status
      integer, parameter :: MAPL_TAG_GLOBAL_IOROOT_RANK = 987
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

end module ExtDataDriverMod

