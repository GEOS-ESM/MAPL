#include "MAPL.h"

submodule (mapl3g_OuterMetaComponent) run_custom_smod
   use mapl_ErrorHandling
   use esmf, only: operator(==)
   implicit none

contains

   module subroutine run_custom(this, method_flag, phase_name, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(ESMF_METHOD_FLAG), intent(in) :: method_flag
      character(*), intent(in) :: phase_name
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: phase_idx
      type(StringVector), pointer :: phases
      logical :: found

      phases => this%get_phases(method_flag)
      phase_idx = get_phase_index(phases, phase_name, found=found)
      _RETURN_UNLESS(found)
      if (method_flag == ESMF_METHOD_INITIALIZE) then
         call this%user_gc_driver%initialize(phase_idx=phase_idx, _RC)
      else if (method_flag == ESMF_METHOD_RUN) then
         call this%user_gc_driver%run(phase_idx=phase_idx, _RC)
      else if (method_flag == ESMF_METHOD_FINALIZE) then
         call this%user_gc_driver%finalize(phase_idx=phase_idx, _RC)
      else
         _FAIL('Unknown ESMF method flag.')
      end if

      _RETURN(_SUCCESS)
   end subroutine run_custom

end submodule run_custom_smod
