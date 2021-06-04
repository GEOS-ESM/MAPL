#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module MAPL_CapOptionsMod
   use ESMF
#ifdef USE_FLAP
   use MAPL_FlapCLIMod
#endif
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   implicit none
   private

   public :: MAPL_CapOptions

   type :: MAPL_CapOptions

      integer :: comm
      logical :: use_comm_world = .true.
      character(:), allocatable :: egress_file
      character(:), allocatable :: cap_rc_file
      type (ESMF_LogKind_Flag) :: esmf_logging_mode = ESMF_LOGKIND_NONE
      integer :: npes_model = -1
      ! only one of the next two options can have nonzero values
      integer, allocatable :: npes_input_server(:)
      integer, allocatable :: nodes_input_server(:)
      ! only one of the next two options can have nonzero values
      integer, allocatable :: npes_output_server(:)
      integer, allocatable :: nodes_output_server(:)
      ! whether or not the nodes are padding with idle when mod(model total npes , each node npes) /=0
      logical              :: isolate_nodes = .true.
      ! whether or not copy the data before isend to the oserver
      ! it is faster but demands more memory if it is true 
      logical              :: fast_oclient  = .false.
      ! whether or not turn on the io profiler
      logical              :: with_io_profiler = .false.
      ! whether or not to use MOAB in ESMF
      logical              :: with_esmf_moab = .false.
      ! server groups
      integer :: n_iserver_group = 1
      integer :: n_oserver_group = 1
      ! ensemble options
      integer :: n_members = 1
      character(:), allocatable :: ensemble_subdir_prefix
      ! logging options
      character(:), allocatable :: logging_config
      character(:), allocatable :: oserver_type
      integer :: npes_backend_pernode = 0

   end type MAPL_CapOptions

   interface MAPL_CapOptions
      module procedure new_CapOptions
#ifdef USE_FLAP
      module procedure new_CapOptions_from_flap
#endif
   end interface

contains

   function new_CapOptions(unusable, cap_rc_file, egress_file, ensemble_subdir_prefix, esmf_logging_mode, rc) result (cap_options)
      type (MAPL_CapOptions) :: cap_options
      class (KeywordEnforcer), optional, intent(in) :: unusable
      character(*), optional, intent(in) :: cap_rc_file
      character(*), optional, intent(in) :: egress_file
      character(*), optional, intent(in) :: ensemble_subdir_prefix 
      type(ESMF_LogKind_Flag), optional, intent(in) :: esmf_logging_mode

      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(unusable)

      cap_options%cap_rc_file = 'CAP.rc'
      cap_options%egress_file = 'EGRESS'
      cap_options%oserver_type= 'single'
      cap_options%ensemble_subdir_prefix = 'mem'

      cap_options%npes_input_server  =[0]
      cap_options%nodes_input_server =[0]
      cap_options%npes_output_server =[0]
      cap_options%nodes_output_server=[0]

      if (present(cap_rc_file)) cap_options%cap_rc_file = cap_rc_file
      if (present(egress_file)) cap_options%egress_file = egress_file
      if (present(ensemble_subdir_prefix)) cap_options%ensemble_subdir_prefix = ensemble_subdir_prefix
      if (present(esmf_logging_mode)) cap_options%esmf_logging_mode = esmf_logging_mode

      _RETURN(_SUCCESS)

   end function

#ifdef USE_FLAP

   function new_CapOptions_from_flap( flapCLI, unusable, rc) result (cap_options)
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
   end function

#endif

end module MAPL_CapOptionsMod

