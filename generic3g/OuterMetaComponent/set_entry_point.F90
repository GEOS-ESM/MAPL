#include "MAPL.h"

submodule (mapl3g_OuterMetaComponent) set_entry_point_smod
   use mapl_ErrorHandling
   implicit none

contains

   module subroutine set_entry_point(this, method_flag, userProcedure, unusable, phase_name, rc)
      class(OuterMetaComponent), intent(inout) :: this
      type(ESMF_Method_Flag), intent(in) :: method_flag
      procedure(I_Run) :: userProcedure
      class(KE), optional, intent(in) :: unusable
      character(len=*), optional, intent(in) :: phase_name
      integer, optional, intent(out) ::rc

      integer :: status
      character(:), allocatable :: phase_name_
      type(ESMF_GridComp) :: user_gridcomp
      logical :: found

      if (present(phase_name)) then
         phase_name_ = phase_name
      else
         phase_name_ = get_default_phase_name(method_flag)
      end if
      call add_phase(this%user_phases_map, method_flag=method_flag, phase_name=phase_name_, _RC)

      associate (phase_idx => get_phase_index(this%user_phases_map%of(method_flag), phase_name=phase_name_, found=found))
        _ASSERT(found, "run phase: <"//phase_name_//"> not found.")
        user_gridcomp = this%user_gc_driver%get_gridcomp()
        call ESMF_GridCompSetEntryPoint(user_gridcomp, method_flag, userProcedure, phase=phase_idx, _RC)
      end associate

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine set_entry_point

end submodule set_entry_point_smod
