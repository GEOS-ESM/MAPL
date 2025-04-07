#include "MAPL_Generic.h"

submodule (mapl3g_OuterMetaComponent) initialize_read_restart_smod

   use mapl3g_GenericPhases
   use mapl_ErrorHandling
   use mapl3g_MultiState
   use mapl3g_RestartHandler, only: RestartHandler

   implicit none

contains

   module recursive subroutine initialize_read_restart(this, unusable, rc)
      class(OuterMetaComponent), intent(inout) :: this
      ! optional arguments
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      character(*), parameter :: PHASE_NAME = 'GENERIC::INIT_READ_RESTART'
      type(GriddedComponentDriver), pointer :: driver
      character(len=:), allocatable :: name
      type(MultiState) :: states
      type(ESMF_State) :: import_state, internal_state
      type(RestartHandler) :: restart_handler
      integer :: status

      driver => this%get_user_gc_driver()
      name = driver%get_name()
      if (this%has_geom()) then
         states = driver%get_states()
         restart_handler = RestartHandler(name, this%get_geom(), driver%get_clock(), _RC)
         call states%get_state(import_state, "import", _RC)
         call restart_handler%read("import", import_state, _RC)
         call states%get_state(internal_state, "internal", _RC)
         call restart_handler%read("internal", internal_state, _RC)
      end if

      call this%run_custom(ESMF_METHOD_INITIALIZE, PHASE_NAME, _RC)
      call recurse(this, phase_idx=GENERIC_INIT_READ_RESTART, _RC)

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine initialize_read_restart

end submodule initialize_read_restart_smod
