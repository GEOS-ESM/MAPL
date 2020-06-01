#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module MAPL_CapOptionsMod
   use ESMF
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
      ! server groups
      integer :: n_iserver_group = 1
      integer :: n_oserver_group = 1
      ! ensemble options
      integer :: n_members = 1
      character(:), allocatable :: ensemble_subdir_prefix
      ! logging options
      character(:), allocatable :: logging_config

   end type MAPL_CapOptions

   interface MAPL_CapOptions
      module procedure new_CapOptions
   end interface

contains

   function new_CapOptions(unusable, cap_rc_file, egress_file, ensemble_subdir_prefix, rc) result (cap_options)
      type (MAPL_CapOptions) :: cap_options
      class (KeywordEnforcer), optional, intent(in) :: unusable
      character(*), optional, intent(in) :: cap_rc_file
      character(*), optional, intent(in) :: egress_file
      character(*), optional, intent(in) :: ensemble_subdir_prefix 

      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(unusable)

      cap_options%cap_rc_file = 'CAP.rc'
      cap_options%egress_file = 'EGRESS'
      cap_options%ensemble_subdir_prefix = 'mem'

      cap_options%npes_input_server  =[0]
      cap_options%nodes_input_server =[0]
      cap_options%npes_output_server =[0]
      cap_options%nodes_output_server=[0]

      if (present(cap_rc_file)) cap_options%cap_rc_file = cap_rc_file
      if (present(egress_file)) cap_options%egress_file = egress_file
      if (present(ensemble_subdir_prefix)) cap_options%ensemble_subdir_prefix = ensemble_subdir_prefix

      _RETURN(_SUCCESS)

   end function

end module MAPL_CapOptionsMod

