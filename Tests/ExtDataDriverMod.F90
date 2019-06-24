#include "MAPL_Generic.h"

module ExtDataDriverMod

   use MPI  
   use ESMF
   use MAPL_BaseMod
   use MAPL_ConstantsMod
   use MAPL_ProfMod
   use MAPL_MemUtilsMod
   use MAPL_CommsMod
   use MAPL_GenericMod
   use ESMFL_Mod
   use MAPL_ShmemMod
   use MAPL_CFIOServerMod
   use MAPL_CFIOMod, only: ioc, init_cfio
   use ExtData_DriverGridCompMod, only: ExtData_DriverGridComp, new_ExtData_DriverGridComp
   use MAPL_ConfigMod
   use PFIO
   use ExtDataUtRoot_GridCompMod, only:  ROOT_SetServices => SetServices
   use FLAP
   use MAPL_KeywordEnforcerMod
   use MAPL_ErrorHandlingMod
   use, intrinsic :: iso_fortran_env, only: output_unit, REAL64, INT64
   implicit none

   public :: ExtDataDriver

   type :: ExtDataDriver
      private
      procedure(), nopass, pointer :: set_services => null()
      character(:), allocatable :: name
      type (ESMF_LogKind_Flag) :: esmf_logging_mode = ESMF_LOGKIND_NONE

   contains
      procedure :: run
      procedure, nopass :: add_command_line_options
      procedure :: parse_command_line_arguments
      procedure :: set_esmf_logging_mode

   end type ExtDataDriver

   interface ExtDataDriver
      module procedure newExtDataDriver
   end interface

contains

   function newExtDataDriver(name,set_services) result(driver)
      type(ExtDataDriver) :: driver
      character(*), intent(in) :: name
      procedure() :: set_services
      driver%name = name
      driver%set_services => set_services
   end function newExtDataDriver

   subroutine run(this,options,CommIn, RC)

      class(ExtDataDriver), intent(inout) :: this
      type (command_line_interface), intent(inout) :: options
      integer,       optional, intent(IN ) :: CommIn
      integer,       optional, intent(OUT) :: rc

      type(ESMF_Config)            :: config


      integer                      :: STATUS
      character(len=ESMF_MAXSTR)   :: Iam="ExtData_Driver"

      integer                  :: CommCap

      type (ESMF_VM) :: VM

      integer             :: mpi_thread_support

      type(ExtData_DriverGridComp), target :: cap

      integer :: lineCount, columnCount,i
      character(len=ESMF_MAXSTR) :: ctemp
      character(len=:), pointer :: cname
      type(StringVector) :: cases
      type(StringVectorIterator) :: iter   

!  Server stuff
      type(MPIserver), target :: i_server
      type(DirectoryService), target :: directory_service

      CommCap = MPI_COMM_WORLD
      call this%parse_command_line_arguments(options,rc=status)
      _VERIFY(status)

      if (.not.present(CommIn)) then
         call mpi_init_thread(MPI_THREAD_MULTIPLE,mpi_thread_support,status)
!call mpi_init(status)
         _VERIFY(STATUS) 
      end if

      call ESMF_Initialize (vm=vm, logKindFlag=this%esmf_logging_mode, mpiCommunicator=commCap, rc=status)
      _VERIFY(STATUS)


      directory_service = DirectoryService(CommCap)

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

      i_server = mpiServer(commCap,'i_server')
      call directory_service%publish(PortInfo('i_server',i_server),i_server)
      call init_cfio()
      call directory_service%connect_to_server('i_server', ioc, commCap)

      iter = cases%begin()
      do while (iter /= cases%end())

         if (mapl_am_I_root()) write(*,*)"Running new case"
         cname => iter%get()
         cap = new_ExtData_DriverGridComp(root_setservices, name=this%name, configFileName=cname)
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

      call ioc%terminate()


      call MPI_Barrier(CommCap,status)
      _VERIFY(STATUS) 
!  Finalize framework
!  ------------------

      if (.not.present(CommIn)) then   
#if 0
!ALT due to a bug in MAPL (or in the garbage collection of ESMF_VMFinalize)
!we have to bypass next line
         call ESMF_Finalize (RC=status)
         _VERIFY(STATUS)
#else
         call mpi_finalize(status)
         _VERIFY(STATUS)
#endif
      end if

      _RETURN(ESMF_SUCCESS)


   end subroutine run

   subroutine add_command_line_options(options, unusable, rc)
      type (command_line_interface), intent(inout) :: options
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: Iam=__FILE__//"::add_command_line_options"
      _UNUSED_DUMMY(unusable)

      call options%add(switch='--esmf_logtype',                   &
            help='ESMF Logging type',                   &
            required=.false.,                           &
            choices='none,single,multi,multi_on_error', &
            def='none',                                 &
            act='store',                                &
            error=status)
      _VERIFY(status)
      _RETURN(ESMF_SUCCESS)
   end subroutine add_command_line_options

   subroutine parse_command_line_arguments(this, options, unusable, rc)
      class (ExtDataDriver), intent(inout) :: this
      type (command_line_interface), intent(inout) :: options
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: Iam = __FILE__//"::parse_command_line_arguments"
      character(80) :: buffer

      _UNUSED_DUMMY(unusable)

      call options%get(val=buffer, switch='--esmf_logtype', error=status); _VERIFY(status)
      call this%set_esmf_logging_mode(trim(buffer), rc=status); _VERIFY(status)
      _RETURN(ESMF_SUCCESS)

   end subroutine parse_command_line_arguments

   subroutine set_esmf_logging_mode(this, flag_name, unusable, rc)
      class (ExtDataDriver), intent(inout) :: this
      character(*), intent(in) :: flag_name
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      character(len=ESMF_MAXSTR) :: Iam = __FILE__//"::set_esmf_logging_mode"

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
         _RETURN(ESMF_FAILURE)
      end select

      _RETURN(ESMF_SUCCESS)
   end subroutine set_esmf_logging_mode

end module ExtDataDriverMod

