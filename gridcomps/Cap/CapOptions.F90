#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module mapl_CapOptionsMod
   use ESMF
   use mapl_KeywordEnforcerMod
   use mapl_ExceptionHandling
   implicit none
   private

   public :: MAPL_CapOptions

   type :: MAPL_CapOptions

      integer :: comm
      logical :: use_comm_world = .true.
      character(:), allocatable :: egress_file
      character(:), allocatable :: cap_rc_file
      character(:), allocatable :: root_dso
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
      module procedure new_CapOptions_copy ! for backward compatibility ! delete for 3.0
   end interface MAPL_CapOptions

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

   end function new_CapOptions

   function new_CapOptions_copy(options) result(copy)
      type(MAPL_CapOptions) :: copy
      type(MAPL_CapOptions), intent(in) :: options
      copy = options
   end function new_CapOptions_copy

end module MAPL_CapOptionsMod

