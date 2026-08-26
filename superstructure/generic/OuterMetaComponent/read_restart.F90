#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) read_restart_smod

   use mapl_ErrorHandling_mod
   use mapl_GenericPhases_mod, only: GENERIC_INTERNAL_READ_RESTART

   implicit none(type,external)

contains

   ! Dedicated ESMF_METHOD_READRESTART dispatch for the internal
   ! (in-memory) checkpoint phase.  Existing netCDF restart reads
   ! continue to run under ESMF_METHOD_INITIALIZE phase
   ! GENERIC_INIT_READ_RESTART via initialize_read_restart, which this
   ! procedure does not touch or replace.
   module recursive subroutine read_restart(this, importState, exportState, clock, unusable, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      ! optional arguments
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: current_phase

      call ESMF_GridCompGet(this%get_gridcomp(), currentPhase=current_phase, _RC)

      select case (current_phase)
      case (GENERIC_INTERNAL_READ_RESTART)
         ! Internal (in-memory) checkpoint read.  Intentionally empty
         ! for this proposal.
      case default
         _FAIL('Unknown internal read restart phase.')
      end select

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
      _UNUSED_DUMMY(unusable)
   end subroutine read_restart

end submodule read_restart_smod
