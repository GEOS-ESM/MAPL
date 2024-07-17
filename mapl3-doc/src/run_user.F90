#include "MAPL_Generic.h"

submodule (mapl3g_OuterMetaComponent) run_user_smod
   implicit none

contains

   module recursive subroutine run_user(this, phase_name, unusable, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      ! optional arguments
      character(len=*), optional, intent(in) :: phase_name
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status, userRC, i
      integer :: phase_idx
      type(StateExtension), pointer :: extension
      type(StringVector), pointer :: run_phases
      logical :: found
      integer :: phase

      type(ActualPtComponentDriverMap), pointer :: export_Couplers
      type(ComponentDriverVector), pointer :: import_Couplers
      type(ActualPtComponentDriverMapIterator) :: iter
      type(ComponentDriverVectorIterator) :: import_iter
      class(ComponentDriver), pointer :: drvr

      run_phases => this%get_phases(ESMF_METHOD_RUN)
      phase = get_phase_index(run_phases, phase_name, found=found)
      _ASSERT(found, 'phase <'//phase_name//'> not found for gridcomp <'//this%get_name()//'>')

      import_couplers => this%registry%get_import_couplers()
      associate (e => import_couplers%ftn_end())
        import_iter = import_couplers%ftn_begin()
        do while (import_iter /= e)
           call import_iter%next()
           drvr => import_iter%of()
           call drvr%run(phase_idx=GENERIC_COUPLER_UPDATE, _RC)
        end do
      end associate

      call this%user_gc_driver%run(phase_idx=phase, _RC)

      export_couplers => this%registry%get_export_couplers()
      associate (e => export_couplers%ftn_end())
        iter = export_couplers%ftn_begin()
        do while (iter /= e)
           call iter%next()
           drvr => iter%second()
           call drvr%run(phase_idx=GENERIC_COUPLER_INVALIDATE, _RC)
        end do
      end associate

      _RETURN(ESMF_SUCCESS)
   end subroutine run_user

end submodule run_user_smod
