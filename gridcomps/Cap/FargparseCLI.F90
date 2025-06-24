#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module MAPL_FargparseCLIMod
   use MPI
   use ESMF
   use fArgParse
   use gFTL2_IntegerVector
   use mapl_KeywordEnforcerMod
   use mapl_ExceptionHandling
   use mapl_CapOptionsMod, only:  MAPL_CapOptions_ => MAPL_CapOptions !Rename is for backward compatibility. Remove renaming for 3.0
   implicit none
   private

   public :: MAPL_FargparseCLI
   public :: MAPL_CapOptions !Needed for backward compatibility. Remove for 3.0

   type :: MAPL_FargparseCLI
     type(ArgParser) :: parser
     type(StringUnlimitedMap) :: options
   contains
      procedure, nopass :: add_command_line_options
      procedure :: fill_cap_options
   end type MAPL_FargparseCLI

   interface MAPL_FargparseCLI
      module procedure new_CapOptions_from_fargparse
      module procedure new_CapOptions_from_fargparse_back_comp
   end interface MAPL_FargparseCLI

   interface MAPL_CapOptions !Needed for backward compatibility. Remove for 3.0
      module procedure old_CapOptions_from_fargparse
   end interface MAPL_CapOptions

   integer, parameter :: NO_VALUE_PASSED_IN = -999

   abstract interface
      subroutine I_extraoptions(parser, rc)
         import ArgParser
         type(ArgParser), intent(inout) :: parser
         integer, optional, intent(out) :: rc
      end subroutine
   end interface
contains

   function new_CapOptions_from_fargparse(unusable, dummy, extra, rc) result (cap_options)
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type (MAPL_CapOptions_) :: cap_options
      character(*), intent(in) :: dummy !Needed for backward compatibility. Remove after 3.0
      procedure(I_extraoptions), optional :: extra
      integer, optional, intent(out) :: rc
      integer :: status

      type(MAPL_FargparseCLI) :: fargparse_cli

      fargparse_cli%parser = ArgParser()

      call fargparse_cli%add_command_line_options(fargparse_cli%parser, __RC)

      if (present(extra)) then
         call extra(fargparse_cli%parser, __RC)
      end if

      fargparse_cli%options = fargparse_cli%parser%parse_args()

      call fargparse_cli%fill_cap_options(cap_options, __RC)

      __RETURN(__SUCCESS)
      __UNUSED_DUMMY(unusable)
      __UNUSED_DUMMY(dummy)
   end function new_CapOptions_from_fargparse

   function new_CapOptions_from_fargparse_back_comp(unusable, extra, rc) result (fargparsecap)
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type (MAPL_FargparseCLI) :: fargparsecap
      procedure(I_extraoptions), optional :: extra
      integer, optional, intent(out) :: rc
      integer :: status

      call fargparsecap%parser%initialize('executable')


      call fargparsecap%add_command_line_options(fargparsecap%parser, __RC)

      if (present(extra)) then
         call extra(fargparsecap%parser, __RC)
      end if

      fargparsecap%options = fargparsecap%parser%parse_args()

      __RETURN(__SUCCESS)
      __UNUSED_DUMMY(unusable)
   end function new_CapOptions_from_fargparse_back_comp

   ! Static method
   subroutine add_command_line_options(parser, unusable, rc)
      type (ArgParser), intent(inout) :: parser
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(IntegerVector) :: intvec

      call parser%add_argument('--root_dso', &
           help='name of root dso to use',   &
           type='string', &
           default='none', &
           action='store')

      call parser%add_argument('--esmf_logtype', &
           help='ESMF Logging type (allowed: none, single, multi, multi_on_error)', &
           !choices='none,single,multi,multi_on_error', &
           type='string', &
           default='none', &
           action='store')

      call parser%add_argument('--egress_file', &
           help='Egress file name', &
           type='string', &
           default='EGRESS', &
           action='store')

      call parser%add_argument('--cap_rc', &
           help='CAP resource file name',  &
           type='string', &
           default='CAP.rc', &
           action='store')

      call parser%add_argument('--npes_model', &
           help='Number of MPI processes used by model CapGridComp', &
           type='integer', &
           action='store', &
           default=-1)

      call parser%add_argument('--n_members', &
           help='Number of MPI processes used by model CapGridComp1', &
           type='integer', &
           action='store', &
           default=1)

      call parser%add_argument('--use_sub_comm', &
           help='The model by default is using MPI_COMM_WORLD : .true. or .false.', &
           action='store_true')

      call parser%add_argument('--comm_model', &
           help='The model will use the communicator passed in', &
           type='string', &
           action='store', &
           default='*')

      call parser%add_argument('--prefix', &
           help='prefix for ensemble subdirectories', &
           type='string', &
           action='store', &
           default='mem')

      ! We create an IntegerVector with a bad value to test if the user
      ! passed in anything

      call intvec%push_back(NO_VALUE_PASSED_IN)

      call parser%add_argument('--npes_input_server', &
           help='Number of MPI processes used by input server', &
           type='integer', &
           n_arguments ='+', &
           default = intvec, &
           action='store')

      call parser%add_argument('--npes_output_server', &
           help='Number of MPI processes used by output server', &
           type='integer', &
           n_arguments ='+', &
           default = intvec, &
           action='store')

      call parser%add_argument('--nodes_input_server', &
           help='Number of nodes used by input server', &
           type='integer', &
           n_arguments ='+', &
           default = intvec, &
           action='store')

      call parser%add_argument('--nodes_output_server', &
           help='Number of nodes used by output server', &
           type='integer',                    &
           n_arguments ='+', &
           default = intvec, &
           action='store')

      call parser%add_argument('--logging_config', &
           help='Configuration file for logging', &
           type='string',                    &
           default='', &
           action='store')

      call parser%add_argument('--oserver_type', &
           help='Output Server Type', &
           type='string',                    &
           default='single', &
           action='store')

      call parser%add_argument('--npes_backend_pernode', &
           help='Number of MPI processes used by the backend output', &
           type='integer',                    &
           default=0, &
           action='store')

      call parser%add_argument('--compress_nodes', &
           help='MPI processes continue on the nodes even MPI communicator is divided', &
           action='store_true')

      call parser%add_argument('--fast_oclient', &
           help='Copying data before isend. Client would wait until it is re-used', &
           action='store_true')

     call parser%add_argument('--one_node_output', &
           help='Specify if each output server has only one nodes', &
           action='store_true')

      call parser%add_argument('--with_io_profiler', &
           help='Turning on io_profler', &
           action='store_true')

      call parser%add_argument('--with_esmf_moab', &
           help='Enables use of MOAB library for ESMF meshes', &
           action='store_true')

      __RETURN(__SUCCESS)
      __UNUSED_DUMMY(unusable)

   end subroutine add_command_line_options

   subroutine fill_cap_options(fargparseCLI, cap_options, unusable, rc)
      class(MAPL_FargparseCLI), intent(inout) :: fargparseCLI
      type(MAPL_CapOptions_), intent(out) :: cap_options
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      integer :: status
      character(:), allocatable :: buffer
      logical :: one_node_output, compress_nodes, use_sub_comm

      integer, allocatable :: nodes_output_server(:)
      class(*), pointer :: option, option_npes, option_nodes
      type (IntegerVector) :: tmp_int_vector, tmp_npes_vector, tmp_nodes_vector

      option => fargparseCLI%options%at('root_dso')
      if (associated(option)) then
         call cast(option, cap_options%root_dso, __RC)
      end if

      option => fargparseCLI%options%at('egress_file')
      if (associated(option)) then
         call cast(option, cap_options%egress_file, __RC)
      end if

      option => fargparseCLI%options%at('use_sub_comm')
      if (associated(option)) then
         call cast(option, use_sub_comm, __RC)
         cap_options%use_comm_world = .not. use_sub_comm
      end if

      if ( .not. cap_options%use_comm_world) then
         option => fargparseCLI%options%at('comm_model')
         if (associated(option)) then
            call cast(option, buffer, __RC)
            __ASSERT(trim(buffer) /= '*', "Should provide comm for model")
            call cast(option, cap_options%comm, __RC)
         end if
      else
        ! comm will be set to MPI_COMM_WORLD later on in initialize_mpi
        ! npes will be set to npes_world later on in initialize_mpi
      endif

      option => fargparseCLI%options%at('npes_model')
      if (associated(option)) then
         call cast(option, cap_options%npes_model, __RC)
      end if

      option => fargparseCLI%options%at('compress_nodes')
      if (associated(option)) then
         call cast(option, compress_nodes, __RC)
         cap_options%isolate_nodes = .not. compress_nodes
      end if

      option => fargparseCLI%options%at('fast_oclient')
      if (associated(option)) then
         call cast(option, cap_options%fast_oclient, __RC)
      end if

      option => fargparseCLI%options%at('with_io_profiler')
      if (associated(option)) then
         call cast(option, cap_options%with_io_profiler, __RC)
      end if

      option => fargparseCLI%options%at('with_esmf_moab')
      if (associated(option)) then
         call cast(option, cap_options%with_esmf_moab, __RC)
      end if

      ! We only allow one of npes_input_server or nodes_input_server
      option_npes => fargparseCLI%options%at('npes_input_server')
      call cast(option_npes, tmp_npes_vector, __RC)
      option_nodes => fargparseCLI%options%at('nodes_input_server')
      call cast(option_nodes, tmp_nodes_vector, __RC)
      __ASSERT(.not.(tmp_npes_vector%of(1) /= NO_VALUE_PASSED_IN .and. tmp_nodes_vector%of(1) /= NO_VALUE_PASSED_IN), 'Cannot specify both --npes_input_server and --nodes_input_server')

      ! npes_input_server is a gFTL IntegerVector that we need to convert to an integer array
      option => fargparseCLI%options%at('npes_input_server')
      call cast(option, tmp_int_vector, __RC)
      if (tmp_int_vector%of(1) /= NO_VALUE_PASSED_IN) then
         cap_options%npes_input_server = tmp_int_vector%data()
      else
         cap_options%npes_input_server = [0]
      end if

      ! nodes_input_server is a gFTL IntegerVector that we need to convert to an integer array
      option => fargparseCLI%options%at('nodes_input_server')
      call cast(option, tmp_int_vector, __RC)
      if (tmp_int_vector%of(1) /= NO_VALUE_PASSED_IN) then
         cap_options%nodes_input_server = tmp_int_vector%data()
      else
         cap_options%nodes_input_server = [0]
      end if

      ! We only allow one of npes_output_server or nodes_output_server
      option_npes => fargparseCLI%options%at('npes_output_server')
      call cast(option_npes, tmp_npes_vector, __RC)
      option_nodes => fargparseCLI%options%at('nodes_output_server')
      call cast(option_nodes, tmp_nodes_vector, __RC)
      __ASSERT(.not.(tmp_npes_vector%of(1) /= NO_VALUE_PASSED_IN .and. tmp_nodes_vector%of(1) /= NO_VALUE_PASSED_IN), 'Cannot specify both --npes_output_server and --nodes_output_server')

      ! npes_output_server is a gFTL IntegerVector that we need to convert to an integer array
      option => fargparseCLI%options%at('npes_output_server')
      call cast(option, tmp_int_vector, __RC)
      if (tmp_int_vector%of(1) /= NO_VALUE_PASSED_IN) then
         cap_options%npes_output_server = tmp_int_vector%data()
      else
         cap_options%npes_output_server = [0]
      end if

      ! nodes_output_server is a gFTL IntegerVector that we need to convert to an integer array
      option => fargparseCLI%options%at('nodes_output_server')
      call cast(option, tmp_int_vector, __RC)
      if (tmp_int_vector%of(1) /= NO_VALUE_PASSED_IN) then
         nodes_output_server = tmp_int_vector%data()
      else
         nodes_output_server = [0]
      end if

      option => fargparseCLI%options%at('one_node_output')
      if (associated(option)) then
         call cast(option, one_node_output, __RC)
      else
         one_node_output = .false.
      end if
      if (one_node_output) then
         allocate(cap_options%nodes_output_server(sum(nodes_output_server)), source =1)
      else
         cap_options%nodes_output_server = nodes_output_server
      endif

      cap_options%n_iserver_group = max(size(cap_options%npes_input_server),size(cap_options%nodes_input_server))
      cap_options%n_oserver_group = max(size(cap_options%npes_output_server),size(cap_options%nodes_output_server))

      option => fargparseCLI%options%at('esmf_logtype')
      if (associated(option)) then
         call cast(option, buffer, __RC)
      end if
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
         __FAIL("Unsupported ESMF logging option: "//trim(buffer))
      end select

      ! Ensemble specific options
      option => fargparseCLI%options%at('prefix')
      if (associated(option)) then
         call cast(option, cap_options%ensemble_subdir_prefix, __RC)
      end if

      option => fargparseCLI%options%at('n_members')
      if (associated(option)) then
         call cast(option, cap_options%n_members, __RC)
      end if

      option => fargparseCLI%options%at('cap_rc')
      if (associated(option)) then
         call cast(option, cap_options%cap_rc_file, __RC)
      end if

      ! Logging options
      option => fargparseCLI%options%at('logging_config')
      if (associated(option)) then
         call cast(option, cap_options%logging_config, __RC)
      end if

      option => fargparseCLI%options%at('oserver_type')
      if (associated(option)) then
         call cast(option, cap_options%oserver_type, __RC)
      end if

      option => fargparseCLI%options%at('npes_backend_pernode')
      if (associated(option)) then
         call cast(option, cap_options%npes_backend_pernode, __RC)
      end if

      __RETURN(__SUCCESS)
      __UNUSED_DUMMY(unusable)
   end subroutine fill_cap_options

   !Function for backward compatibility. Remove for 3.0
   function old_CapOptions_from_Fargparse( fargparseCLI, unusable, rc) result (cap_options)
      type (MAPL_CapOptions_) :: cap_options
      type (MAPL_FargparseCLI), intent(inout) :: fargparseCLI
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      integer :: status
      character(:), allocatable :: buffer
      logical :: one_node_output, compress_nodes, use_sub_comm

      integer, allocatable :: nodes_output_server(:)
      class(*), pointer :: option, option_npes, option_nodes
      type (IntegerVector) :: tmp_int_vector, tmp_npes_vector, tmp_nodes_vector

      option => fargparseCLI%options%at('root_dso')
      if (associated(option)) then
         call cast(option, cap_options%root_dso, __RC)
      end if

      option => fargparseCLI%options%at('egress_file')
      if (associated(option)) then
         call cast(option, cap_options%egress_file, __RC)
      end if

      option => fargparseCLI%options%at('use_sub_comm')
      if (associated(option)) then
         call cast(option, use_sub_comm, __RC)
         cap_options%use_comm_world = .not. use_sub_comm
      end if

      if ( .not. cap_options%use_comm_world) then
         option => fargparseCLI%options%at('comm_model')
         if (associated(option)) then
            call cast(option, buffer, __RC)
            __ASSERT(trim(buffer) /= '*', "Should provide comm for model")
            call cast(option, cap_options%comm, __RC)
         end if
      else
        ! comm will be set to MPI_COMM_WORLD later on in initialize_mpi
        ! npes will be set to npes_world later on in initialize_mpi
      endif

      option => fargparseCLI%options%at('npes_model')
      if (associated(option)) then
         call cast(option, cap_options%npes_model, __RC)
      end if

      option => fargparseCLI%options%at('compress_nodes')
      if (associated(option)) then
         call cast(option, compress_nodes, __RC)
         cap_options%isolate_nodes = .not. compress_nodes
      end if

      option => fargparseCLI%options%at('fast_oclient')
      if (associated(option)) then
         call cast(option, cap_options%fast_oclient, __RC)
      end if

      option => fargparseCLI%options%at('with_io_profiler')
      if (associated(option)) then
         call cast(option, cap_options%with_io_profiler, __RC)
      end if

      option => fargparseCLI%options%at('with_esmf_moab')
      if (associated(option)) then
         call cast(option, cap_options%with_esmf_moab, __RC)
      end if

      ! We only allow one of npes_input_server or nodes_input_server
      option_npes => fargparseCLI%options%at('npes_input_server')
      call cast(option_npes, tmp_npes_vector, __RC)
      option_nodes => fargparseCLI%options%at('nodes_input_server')
      call cast(option_nodes, tmp_nodes_vector, __RC)
      __ASSERT(.not.(tmp_npes_vector%of(1) /= NO_VALUE_PASSED_IN .and. tmp_nodes_vector%of(1) /= NO_VALUE_PASSED_IN), 'Cannot specify both --npes_input_server and --nodes_input_server')

      ! npes_input_server is a gFTL IntegerVector that we need to convert to an integer array
      option => fargparseCLI%options%at('npes_input_server')
      call cast(option, tmp_int_vector, __RC)
      if (tmp_int_vector%of(1) /= NO_VALUE_PASSED_IN) then
         cap_options%npes_input_server = tmp_int_vector%data()
      else
         cap_options%npes_input_server = [0]
      end if

      ! nodes_input_server is a gFTL IntegerVector that we need to convert to an integer array
      option => fargparseCLI%options%at('nodes_input_server')
      call cast(option, tmp_int_vector, __RC)
      if (tmp_int_vector%of(1) /= NO_VALUE_PASSED_IN) then
         cap_options%nodes_input_server = tmp_int_vector%data()
      else
         cap_options%nodes_input_server = [0]
      end if

      ! We only allow one of npes_output_server or nodes_output_server
      option_npes => fargparseCLI%options%at('npes_output_server')
      call cast(option_npes, tmp_npes_vector, __RC)
      option_nodes => fargparseCLI%options%at('nodes_output_server')
      call cast(option_nodes, tmp_nodes_vector, __RC)
      __ASSERT(.not.(tmp_npes_vector%of(1) /= NO_VALUE_PASSED_IN .and. tmp_nodes_vector%of(1) /= NO_VALUE_PASSED_IN), 'Cannot specify both --npes_output_server and --nodes_output_server')

      ! npes_output_server is a gFTL IntegerVector that we need to convert to an integer array
      option => fargparseCLI%options%at('npes_output_server')
      call cast(option, tmp_int_vector, __RC)
      if (tmp_int_vector%of(1) /= NO_VALUE_PASSED_IN) then
         cap_options%npes_output_server = tmp_int_vector%data()
      else
         cap_options%npes_output_server = [0]
      end if

      ! nodes_output_server is a gFTL IntegerVector that we need to convert to an integer array
      option => fargparseCLI%options%at('nodes_output_server')
      call cast(option, tmp_int_vector, __RC)
      if (tmp_int_vector%of(1) /= NO_VALUE_PASSED_IN) then
         nodes_output_server = tmp_int_vector%data()
      else
         nodes_output_server = [0]
      end if

      option => fargparseCLI%options%at('one_node_output')
      if (associated(option)) then
         call cast(option, one_node_output, __RC)
      else
         one_node_output = .false.
      end if
      if (one_node_output) then
         allocate(cap_options%nodes_output_server(sum(nodes_output_server)), source =1)
      else
         cap_options%nodes_output_server = nodes_output_server
      endif

      cap_options%n_iserver_group = max(size(cap_options%npes_input_server),size(cap_options%nodes_input_server))
      cap_options%n_oserver_group = max(size(cap_options%npes_output_server),size(cap_options%nodes_output_server))

      option => fargparseCLI%options%at('esmf_logtype')
      if (associated(option)) then
         call cast(option, buffer, __RC)
      end if
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
         __FAIL("Unsupported ESMF logging option: "//trim(buffer))
      end select

      ! Ensemble specific options
      option => fargparseCLI%options%at('prefix')
      if (associated(option)) then
         call cast(option, cap_options%ensemble_subdir_prefix, __RC)
      end if

      option => fargparseCLI%options%at('n_members')
      if (associated(option)) then
         call cast(option, cap_options%n_members, __RC)
      end if

      option => fargparseCLI%options%at('cap_rc')
      if (associated(option)) then
         call cast(option, cap_options%cap_rc_file, __RC)
      end if

      ! Logging options
      option => fargparseCLI%options%at('logging_config')
      if (associated(option)) then
         call cast(option, cap_options%logging_config, __RC)
      end if

      option => fargparseCLI%options%at('oserver_type')
      if (associated(option)) then
         call cast(option, cap_options%oserver_type, __RC)
      end if

      option => fargparseCLI%options%at('npes_backend_pernode')
      if (associated(option)) then
         call cast(option, cap_options%npes_backend_pernode, __RC)
      end if

      __RETURN(__SUCCESS)
      __UNUSED_DUMMY(unusable)
   end function old_CapOptions_from_Fargparse

end module MAPL_FargparseCLIMod
