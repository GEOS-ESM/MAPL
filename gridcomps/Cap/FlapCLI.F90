#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module MAPL_FlapCLIMod
   use MPI
   use ESMF
   use FLAP
   use mapl_KeywordEnforcerMod
   use mapl_ExceptionHandling
   use mapl_CapOptionsMod, only:  MAPL_CapOptions !Rename is for backward compatibility. Remove renaming for 3.0
   implicit none
   private

   public :: MAPL_FlapCLI
   public :: MAPL_CapOptions !Needed for backward compatibility. Remove for 3.0

   type :: MAPL_FlapCLI
     type(command_line_interface) :: cli_options
   contains
      procedure, nopass :: add_command_line_options
      procedure :: fill_cap_options
   end type MAPL_FlapCLI

   interface MAPL_FlapCLI
      module procedure new_CapOptions_from_flap
      module procedure new_CapOptions_from_flap_back_comp
   end interface MAPL_FlapCLI

   interface MAPL_CapOptions !Needed for backward compatibility. Remove for 3.0
      module procedure old_CapOptions_from_flap
   end interface MAPL_CapOptions


contains

   function new_CapOptions_from_flap(unusable, description, authors, dummy, rc) result (cap_options)
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type (MAPL_CapOptions) :: cap_options
      character(*), intent(in) :: description
      character(*), intent(in) :: authors
      character(*), intent(in) :: dummy !Needed for backward compatibility. Remove after 3.0
      integer, optional, intent(out) :: rc
      integer :: status

      type(MAPL_FlapCLI) :: flap_cli

      call flap_cli%cli_options%init( &
        description = trim(description), &
        authors     = trim(authors))

      call flap_cli%add_command_line_options(flap_cli%cli_options, rc=status)
      _VERIFY(status)   

      call flap_cli%cli_options%parse(error=status); _VERIFY(status)

      call flap_cli%fill_cap_options(cap_options, rc=status)
      _VERIFY(status)
      
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function new_CapOptions_from_flap

   function new_CapOptions_from_flap_back_comp(unusable, description, authors, rc) result (flapcap)
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type (MAPL_FlapCLI) :: flapcap
      character(*), intent(in) :: description
      character(*), intent(in) :: authors
      integer, optional, intent(out) :: rc
      integer :: status


      call flapcap%cli_options%init( &
        description = trim(description), &
        authors     = trim(authors))

      call flapcap%add_command_line_options(flapcap%cli_options, rc=status)
      _VERIFY(status)

      call flapcap%cli_options%parse(error=status); _VERIFY(status)
      _VERIFY(status)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function new_CapOptions_from_flap_back_comp

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

      call options%add(switch='--use_sub_comm', &
           help='# The model by default is using MPI_COMM_WORLD : .true. or .false.', &
           required=.false., &
           def='.false.',     &
           act='store_true',      &
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

      call options%add(switch='--oserver_type', &
           help='Output Server Type', &
           required=.false., &
           def='single', &
           act='store', &
           error=status)
      _VERIFY(status)

      call options%add(switch='--npes_backend_pernode', &
           help='# MPI processes used by the backend output', &
           required=.false., &
           def='0', &
           act='store', &
           error=status)
      _VERIFY(status)

      call options%add(switch='--compress_nodes', &
           help='MPI processes continue on the nodes even MPI communicator is divided', &
           required=.false., &
           def='.false.', &
           act='store_true', &
           error=status)
      _VERIFY(status)

      call options%add(switch='--fast_oclient', &
           help='Copying data before isend. Client would wait until it is re-used', &
           required=.false., &
           def='.false.', &
           act='store_true', &
           error=status)
      _VERIFY(status)

     call options%add(switch='--one_node_output', &
           help='Specify if each output server has only one nodes', &
           required=.false., &
           def='.false.', &
           act='store_true', &
           error=status)

      call options%add(switch='--with_io_profiler', &
           help='Turning on io_profler', &
           required=.false., &
           def='.false.', &
           act='store_true', &
           error=status)
      _VERIFY(status)

      call options%add(switch='--with_esmf_moab', &
           help='Enables use of MOAB library for ESMF meshes', &
           required=.false., &
           def='.false.', &
           act='store_true', &
           error=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)

   end subroutine add_command_line_options

   subroutine fill_cap_options(flapCLI, cap_options, unusable, rc)
      class(MAPL_FlapCLI), intent(inout) :: flapCLI
      type(MAPL_CapOptions), intent(out) :: cap_options
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      integer :: status
      character(80) :: buffer
      logical :: one_node_output, compress_nodes, use_sub_comm

      integer, allocatable :: nodes_output_server(:)

      call flapCLI%cli_options%get(val=buffer, switch='--egress_file', error=status); _VERIFY(status)
      cap_options%egress_file = trim(buffer)

      call flapCLI%cli_options%get(val=use_sub_comm, switch='--use_sub_comm', error=status); _VERIFY(status)
      cap_options%use_comm_world = .not. use_sub_comm

      if ( .not. cap_options%use_comm_world) then
         call flapCLI%cli_options%get(val=buffer, switch='--comm_model', error=status); _VERIFY(status)
         _ASSERT(trim(buffer) /= '*', "Should provide comm for model")
         call flapCLI%cli_options%get(val=cap_options%comm, switch='--comm_model', error=status); _VERIFY(status)
      else
        ! comm will be set to MPI_COMM_WORLD later on in initialize_mpi
        ! npes will be set to npes_world later on in initialize_mpi
      endif

      call flapCLI%cli_options%get(val=cap_options%npes_model, switch='--npes_model', error=status); _VERIFY(status)
      call flapCLI%cli_options%get(val=compress_nodes, switch='--compress_nodes', error=status); _VERIFY(status)
      cap_options%isolate_nodes = .not. compress_nodes
      call flapCLI%cli_options%get(val=cap_options%fast_oclient, switch='--fast_oclient', error=status); _VERIFY(status)
      call flapCLI%cli_options%get(val=cap_options%with_io_profiler, switch='--with_io_profiler', error=status); _VERIFY(status)
      call flapCLI%cli_options%get(val=cap_options%with_esmf_moab, switch='--with_esmf_moab', error=status); _VERIFY(status)
      call flapCLI%cli_options%get_varying(val=cap_options%npes_input_server, switch='--npes_input_server', error=status); _VERIFY(status)
      call flapCLI%cli_options%get_varying(val=cap_options%npes_output_server, switch='--npes_output_server', error=status); _VERIFY(status)
      call flapCLI%cli_options%get_varying(val=cap_options%nodes_input_server, switch='--nodes_input_server', error=status); _VERIFY(status)
      call flapCLI%cli_options%get_varying(val=nodes_output_server, switch='--nodes_output_server', error=status); _VERIFY(status)
      call flapCLI%cli_options%get(val=one_node_output, switch='--one_node_output', error=status); _VERIFY(status)
      if (one_node_output) then
         allocate(cap_options%nodes_output_server(sum(nodes_output_server)), source =1)
      else
         cap_options%nodes_output_server = nodes_output_server
      endif

      cap_options%n_iserver_group = max(size(cap_options%npes_input_server),size(cap_options%nodes_input_server))
      cap_options%n_oserver_group = max(size(cap_options%npes_output_server),size(cap_options%nodes_output_server))

      call flapCLI%cli_options%get(val=buffer, switch='--esmf_logtype', error=status); _VERIFY(status)
      ! set_esmf_logging_mode
      select case (trim(buffer))
      case ('none')
         cap_options%esmf_logging_mode = ESMF_LOGKIND_NONE
      case ('single')
         cap_options%esmf_logging_mode = ESMF_LOGKIND_SINGLE
      case ('multi')
         cap_options%esmf_logging_mode = ESMF_LOGKIND_MULTI
      case ('multi_on_error')
         cap_options%esmf_logging_mode = ESMF_LOGKIND_MULTI_ON_ERROR
      case default
         _FAIL("Unsupported ESMF logging option: "//trim(buffer))
      end select

      ! Ensemble specific options
      call flapCLI%cli_options%get(val=buffer, switch='--prefix', error=status); _VERIFY(status)
      cap_options%ensemble_subdir_prefix = trim(buffer)
      call flapCLI%cli_options%get(val=cap_options%n_members, switch='--n_members', error=status); _VERIFY(status)

      call flapCLI%cli_options%get(val=buffer, switch='--cap_rc', error=status); _VERIFY(status)
      cap_options%cap_rc_file = trim(buffer)

      ! Logging options
      call flapCLI%cli_options%get(val=buffer, switch='--logging_config', error=status); _VERIFY(status)
      cap_options%logging_config = trim(buffer)
      ! ouput server type options
      call flapCLI%cli_options%get(val=buffer, switch='--oserver_type', error=status); _VERIFY(status)
      cap_options%oserver_type = trim(buffer)
      call flapCLI%cli_options%get(val=cap_options%npes_backend_pernode, switch='--npes_backend_pernode', error=status); _VERIFY(status)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine fill_cap_options

   !Function for backward compatibility. Remove for 3.0
   function old_CapOptions_from_Flap( flapCLI, unusable, rc) result (cap_options)
      type (MAPL_CapOptions) :: cap_options
      type (MAPL_FlapCLI), intent(inout) :: flapCLI
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      integer :: status
      character(80) :: buffer
      logical :: one_node_output, compress_nodes, use_sub_comm

      integer, allocatable :: nodes_output_server(:)

      call flapCLI%cli_options%get(val=buffer, switch='--egress_file', error=status); _VERIFY(status)
      cap_options%egress_file = trim(buffer)

      call flapCLI%cli_options%get(val=use_sub_comm, switch='--use_sub_comm', error=status); _VERIFY(status)
      cap_options%use_comm_world = .not. use_sub_comm

      if ( .not. cap_options%use_comm_world) then
         call flapCLI%cli_options%get(val=buffer, switch='--comm_model', error=status); _VERIFY(status)
         _ASSERT(trim(buffer) /= '*', "Should provide comm for model")
         call flapCLI%cli_options%get(val=cap_options%comm, switch='--comm_model', error=status); _VERIFY(status)
      else
        ! comm will be set to MPI_COMM_WORLD later on in initialize_mpi
        ! npes will be set to npes_world later on in initialize_mpi
      endif

      call flapCLI%cli_options%get(val=cap_options%npes_model, switch='--npes_model', error=status); _VERIFY(status)
      call flapCLI%cli_options%get(val=compress_nodes, switch='--compress_nodes', error=status); _VERIFY(status)
      cap_options%isolate_nodes = .not. compress_nodes
      call flapCLI%cli_options%get(val=cap_options%fast_oclient, switch='--fast_oclient', error=status); _VERIFY(status)
      call flapCLI%cli_options%get(val=cap_options%with_io_profiler, switch='--with_io_profiler', error=status); _VERIFY(status)
      call flapCLI%cli_options%get(val=cap_options%with_esmf_moab, switch='--with_esmf_moab', error=status); _VERIFY(status)
      call flapCLI%cli_options%get_varying(val=cap_options%npes_input_server, switch='--npes_input_server', error=status); _VERIFY(status)
      call flapCLI%cli_options%get_varying(val=cap_options%npes_output_server, switch='--npes_output_server', error=status); _VERIFY(status)
      call flapCLI%cli_options%get_varying(val=cap_options%nodes_input_server, switch='--nodes_input_server', error=status); _VERIFY(status)
      call flapCLI%cli_options%get_varying(val=nodes_output_server, switch='--nodes_output_server', error=status); _VERIFY(status)
      call flapCLI%cli_options%get(val=one_node_output, switch='--one_node_output', error=status); _VERIFY(status)
      if (one_node_output) then
         allocate(cap_options%nodes_output_server(sum(nodes_output_server)), source =1)
      else
         cap_options%nodes_output_server = nodes_output_server
      endif

      cap_options%n_iserver_group = max(size(cap_options%npes_input_server),size(cap_options%nodes_input_server))
      cap_options%n_oserver_group = max(size(cap_options%npes_output_server),size(cap_options%nodes_output_server))

      call flapCLI%cli_options%get(val=buffer, switch='--esmf_logtype', error=status); _VERIFY(status)
      ! set_esmf_logging_mode
      select case (trim(buffer))
      case ('none')
         cap_options%esmf_logging_mode = ESMF_LOGKIND_NONE
      case ('single')
         cap_options%esmf_logging_mode = ESMF_LOGKIND_SINGLE
      case ('multi')
         cap_options%esmf_logging_mode = ESMF_LOGKIND_MULTI
      case ('multi_on_error')
         cap_options%esmf_logging_mode = ESMF_LOGKIND_MULTI_ON_ERROR
      case default
         _FAIL("Unsupported ESMF logging option: "//trim(buffer))
      end select

      ! Ensemble specific options
      call flapCLI%cli_options%get(val=buffer, switch='--prefix', error=status); _VERIFY(status)
      cap_options%ensemble_subdir_prefix = trim(buffer)
      call flapCLI%cli_options%get(val=cap_options%n_members, switch='--n_members', error=status); _VERIFY(status)

      call flapCLI%cli_options%get(val=buffer, switch='--cap_rc', error=status); _VERIFY(status)
      cap_options%cap_rc_file = trim(buffer)

      ! Logging options
      call flapCLI%cli_options%get(val=buffer, switch='--logging_config', error=status); _VERIFY(status)
      cap_options%logging_config = trim(buffer)
      ! ouput server type options
      call flapCLI%cli_options%get(val=buffer, switch='--oserver_type', error=status); _VERIFY(status)
      cap_options%oserver_type = trim(buffer)
      call flapCLI%cli_options%get(val=cap_options%npes_backend_pernode, switch='--npes_backend_pernode', error=status); _VERIFY(status)

      _RETURN(_SUCCESS)
   end function old_CapOptions_from_Flap

end module MAPL_FlapCLIMod
