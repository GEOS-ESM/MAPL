#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module MAPL_CapOptionsMod
   use ESMF
   use MAPL_KeywordEnforcerMod
   use MAPL_ErrorHandlingMod
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
      ! only one of the next two options can be nonzero
      integer :: npes_input_server  = 0
      integer :: nodes_input_server = 0
      ! only one of the next two options can be nonzero
      integer :: npes_output_server = 0
      integer :: nodes_output_server= 0
      ! ensemble options
      integer :: n_members = 1
      character(:), allocatable :: ensemble_subdir_prefix

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
      integer :: status

      _UNUSED_DUMMY(unusable)

      cap_options%cap_rc_file = 'CAP.rc'
      cap_options%egress_file = 'EGRESS'
      cap_options%ensemble_subdir_prefix = 'mem'

      if (present(cap_rc_file)) cap_options%cap_rc_file = cap_rc_file
      if (present(egress_file)) cap_options%egress_file = egress_file
      if (present(ensemble_subdir_prefix)) cap_options%ensemble_subdir_prefix = ensemble_subdir_prefix

      _RETURN(_SUCCESS)

   end function

end module MAPL_CapOptionsMod

