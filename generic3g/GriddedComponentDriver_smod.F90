#include "MAPL_ErrLog.h"

submodule(mapl3g_GriddedComponentDriver) GriddedComponentDriver_run_smod
   use :: mapl_ErrorHandling
   use :: mapl3g_OuterMetaComponent
   use :: mapl3g_MethodPhasesMapUtils
   use mapl3g_CouplerMetaComponent, only: GENERIC_COUPLER_INVALIDATE, GENERIC_COUPLER_UPDATE
   implicit none

contains

   module recursive subroutine run(this, unusable, phase_idx, rc)
      class(GriddedComponentDriver), intent(inout) :: this
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(in) :: phase_idx
      integer, optional, intent(out) :: rc

      integer :: status, userRC

      call this%run_import_couplers(_RC)
      associate ( &
           importState => this%states%importState, &
           exportState => this%states%exportState)

        call ESMF_GridCompRun(this%gridcomp, &
             importState=importState, &
             exportState=exportState, &
             clock=this%clock, &
             phase=phase_idx, userRC=userRC, _RC)
        _VERIFY(userRC)
      end associate
      call this%run_export_couplers(phase_idx=phase_idx, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine run

   recursive module subroutine initialize(this, unusable, phase_idx, rc)
      class(GriddedComponentDriver), intent(inout) :: this
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(in) :: phase_idx
      integer, optional, intent(out) :: rc

      integer :: status, userRC

      associate ( &
           importState => this%states%importState, &
           exportState => this%states%exportState)

        call ESMF_GridCompInitialize(this%gridcomp, &
             importState=importState, exportState=exportState, clock=this%clock, &
             phase=phase_idx, userRC=userRC, _RC)
        _VERIFY(userRC)

      end associate

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine initialize

   module recursive subroutine finalize(this, unusable, phase_idx, rc)
      use MAPL_Shared, only: 
      class(GriddedComponentDriver), intent(inout) :: this
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(in) :: phase_idx
      integer, optional, intent(out) :: rc

      integer :: status, userRC

      associate ( &
           importState => this%states%importState, &
           exportState => this%states%exportState)

        call ESMF_GridCompFinalize(this%gridcomp, &
             importState=importState, exportState=exportState, clock=this%clock, &
             phase=phase_idx, userRC=userRC, _RC)
        _VERIFY(userRC)
      end associate

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine finalize


   module function get_clock(this) result(clock)
      type(ESMF_Clock) :: clock
      class(GriddedComponentDriver), intent(in) :: this

      clock = this%clock
   end function get_clock

   module subroutine set_clock(this, clock)
      class(GriddedComponentDriver), intent(inout) :: this
      type(ESMF_Clock), intent(in) :: clock

      this%clock = clock
   end subroutine set_clock


   module function get_states(this) result(states)
      type(MultiState) :: states
      class(GriddedComponentDriver), intent(in) :: this

      states = this%states
   end function get_states

   recursive module subroutine run_import_couplers(this, rc)
      class(GriddedComponentDriver), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentDriverVectorIterator) :: iter
      class(ComponentDriver), pointer :: driver

      associate (e => this%import_couplers%ftn_end() )
        iter = this%import_couplers%ftn_begin()
        do while (iter /= e)
           call iter%next()
           driver => iter%of()
           call driver%run(phase_idx=GENERIC_COUPLER_UPDATE, _RC)
        end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine run_import_couplers

   recursive module subroutine run_export_couplers(this, unusable, phase_idx, rc)
      class(GriddedComponentDriver), intent(inout) :: this
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(in) :: phase_idx
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentDriverVectorIterator) :: iter
      class(ComponentDriver), pointer :: driver

      associate (e => this%export_couplers%ftn_end() )
        iter = this%export_couplers%ftn_begin()
        do while (iter /= e)
           call iter%next()
           driver => iter%of()
           call driver%run(phase_idx=GENERIC_COUPLER_INVALIDATE, _RC)
        end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine run_export_couplers

end submodule GriddedComponentDriver_run_smod
