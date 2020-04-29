#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module MAPL_FlapCapOptionsMod
   use MPI
   use ESMF
   use FLAP
   use MAPL_KeywordEnforcerMod
   use MAPL_ErrorHandlingMod
   use MAPL_CapOptionsMod
   use pflogger
   implicit none
   private

   public :: MAPL_FlapCapOptions

   type, extends(MAPL_CapOptions) :: MAPL_FlapCapOptions
     type(command_line_interface) :: cli_options
   contains
      procedure, nopass :: add_command_line_options
      procedure :: parse_command_line_arguments
      procedure :: set_esmf_logging_mode
   end type MAPL_FlapCapOptions

   interface MAPL_FlapCapOptions
      module procedure new_FlapCapOptions
   end interface

contains

   function new_FlapCapOptions(unusable, description, authors, rc) result (flapcap)
      type (MAPL_FlapCapOptions) :: flapcap
      class (KeywordEnforcer), optional, intent(in) :: unusable
      character(*), optional, intent(in) :: description
      character(*), optional, intent(in) :: authors
      integer, optional, intent(out) :: rc
      integer :: status
 
      _UNUSED_DUMMY(unusable)

      call flapcap%cli_options%init( &
        description = trim(description), &
        authors     = trim(authors))

      call flapcap%add_command_line_options(flapcap%cli_options, rc=status)
      _VERIFY(status)   
      call flapcap%parse_command_line_arguments(rc=status)
      _VERIFY(status)
      _RETURN(_SUCCESS)

   end function   

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


      call options%add(switch='--npes_model', &
           help='# MPI processes used by model CapGridComp', &
           required=.false., &
           act='store', &
           def='-1', &
           error=status)
      _VERIFY(status)

      call options%add(switch='--n_members', &
           help='# MPI processes used by model CapGridComp1', &
           required=.false., &
           act='store', &
           def='1', &
           error=status)
      _VERIFY(status)

      call options%add(switch='--use_comm_world', &
           help='# The model by default is using MPI_COMM_WORLD : .true. or .false.', &
           required=.false., &
           act='store',      &
           def='.true.',     &
           error=status)
      _VERIFY(status)

      call options%add(switch='--comm_model', &
           help='# The model will use the communitator passed in', &
           required=.false., &
           act='store',      &
           def='*',     &
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
           nargs ='*', &
           exclude = '--nodes_input_server', &
           act='store', &
           error=status)
      _VERIFY(status)
      
      call options%add(switch='--npes_output_server', &
           help='# MPI processes used by output server', &
           required=.false., &
           def='0', &
           nargs ='*', &
           exclude = '--nodes_output_server', &
           act='store', &
           error=status)
      _VERIFY(status)

      call options%add(switch='--nodes_input_server', &
           help='# NCCS nodes (28 or more processors ) used by input server', &
           required=.false., &
           def='0', &
           nargs ='*', &
           exclude = '--npes_input_server', &
           act='store', &
           error=status)
      _VERIFY(status)
      
      call options%add(switch='--nodes_output_server', &
           help='# NCCS nodes (28 or more processors) used by output server', &
           required=.false., &
           def='0', &
           nargs ='*', &
           exclude = '--npes_output_server', &
           act='store', &
           error=status)
      _VERIFY(status)

      call options%add(switch='--logging_config', &
           help='Configuration file for logging', &
           required=.false., &
           def='', &
           act='store', &
           error=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)

   end subroutine add_command_line_options

   subroutine parse_command_line_arguments(this, unusable, rc)
      class (MAPL_FlapCapOptions), intent(inout) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      character(80) :: buffer

      _UNUSED_DUMMY(unusable)

      call this%cli_options%parse(error=status); _VERIFY(status)

      call this%cli_options%get(val=buffer, switch='--egress_file', error=status); _VERIFY(status)
      this%egress_file = trim(buffer)

      call this%cli_options%get(val=this%use_comm_world, switch='--use_comm_world', error=status); _VERIFY(status)

      if ( .not. this%use_comm_world) then
         call this%cli_options%get(val=buffer, switch='--comm_model', error=status); _VERIFY(status)
         _ASSERT(trim(buffer) /= '*', "Should provide comm for model")
         call this%cli_options%get(val=this%comm, switch='--comm_model', error=status); _VERIFY(status)
      else
        ! comm will be set to MPI_COMM_WORLD later on in initialize_mpi
        ! npes will be set to npes_world later on in initialize_mpi
      endif
 
      call this%cli_options%get(val=this%npes_model, switch='--npes_model', error=status); _VERIFY(status)
      call this%cli_options%get_varying(val=this%npes_input_server, switch='--npes_input_server', error=status); _VERIFY(status)
      call this%cli_options%get_varying(val=this%npes_output_server, switch='--npes_output_server', error=status); _VERIFY(status)
      call this%cli_options%get_varying(val=this%nodes_input_server, switch='--nodes_input_server', error=status); _VERIFY(status)
      call this%cli_options%get_varying(val=this%nodes_output_server, switch='--nodes_output_server', error=status); _VERIFY(status)

      this%n_iserver_group = max(size(this%npes_input_server),size(this%nodes_input_server))
      this%n_oserver_group = max(size(this%npes_output_server),size(this%nodes_output_server))

      call this%cli_options%get(val=buffer, switch='--esmf_logtype', error=status); _VERIFY(status)
      call this%set_esmf_logging_mode(trim(buffer), rc=status); _VERIFY(status)

      ! Ensemble specific options
      call this%cli_options%get(val=buffer, switch='--prefix', error=status); _VERIFY(status)
      this%ensemble_subdir_prefix = trim(buffer)
      call this%cli_options%get(val=this%n_members, switch='--n_members', error=status); _VERIFY(status)
      
      call this%cli_options%get(val=buffer, switch='--cap_rc', error=status); _VERIFY(status)
      this%cap_rc_file = trim(buffer)

      ! Logging options
      call this%cli_options%get(val=buffer, switch='--logging_config', error=status); _VERIFY(status)
      this%logging_config = trim(buffer)

    end subroutine parse_command_line_arguments

    subroutine set_esmf_logging_mode(this, flag_name, unusable, rc)
      class (MAPL_FlapCapOptions), intent(inout) :: this
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

end module MAPL_FlapCapOptionsMod
