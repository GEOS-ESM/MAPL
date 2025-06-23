#include "MAPL_Generic.h"

module mapl3g_GriddedComponentDriver
   use mapl3g_MultiState
   use mapl3g_ComponentDriver
   use mapl3g_ComponentDriverVector
   use mapl_ErrorHandlingMod
   use mapl_KeywordEnforcer, only: KE => KeywordEnforcer
   use :: esmf
   implicit none
   private

   public :: GriddedComponentDriver

   type, extends(ComponentDriver) :: GriddedComponentDriver
      private
      type(ESMF_GridComp) :: gridcomp
      type(MultiState) :: states
      type(ESMF_Clock) :: clock
      type(ComponentDriverVector) :: export_couplers
      type(ComponentDriverVector) :: import_couplers
   contains
      procedure :: initialize
      procedure :: run
      procedure :: finalize
      procedure :: write_restart
      procedure :: clock_advance

      ! Accessors
      procedure :: get_clock
      procedure :: set_clock
      procedure :: get_states
      procedure :: get_gridcomp
      procedure :: get_name

      ! Couplers
      procedure :: run_export_couplers
      procedure :: run_import_couplers
      procedure :: add_export_coupler
      procedure :: add_import_coupler
   end type GriddedComponentDriver

   interface GriddedComponentDriver
      module procedure new_GriddedComponentDriver_all
   end interface GriddedComponentDriver

   interface

      module recursive subroutine initialize(this, unusable, phase_idx, rc)
         class(GriddedComponentDriver), intent(inout) :: this
         class(KE), optional, intent(in) :: unusable
         integer, optional, intent(in) :: phase_idx
         integer, optional, intent(out) :: rc
      end subroutine initialize

      ! run_self() is implemented in submodule to avoid circular dependency
      ! on OuterMetaComponent.
      module recursive subroutine run(this, unusable, phase_idx, rc)
         class(GriddedComponentDriver), intent(inout) :: this
         class(KE), optional, intent(in) :: unusable
         integer, optional, intent(in) :: phase_idx
         integer, optional, intent(out) :: rc
      end subroutine

      module recursive subroutine finalize(this, unusable, phase_idx, rc)
         class(GriddedComponentDriver), intent(inout) :: this
         class(KE), optional, intent(in) :: unusable
         integer, optional, intent(in) :: phase_idx
         integer, optional, intent(out) :: rc
      end subroutine finalize

      module recursive subroutine write_restart(this, unusable, phase_idx, rc)
         class(GriddedComponentDriver), intent(inout) :: this
         class(KE), optional, intent(in) :: unusable
         integer, optional, intent(in) :: phase_idx
         integer, optional, intent(out) :: rc
      end subroutine write_restart

      module function get_states(this) result(states)
         type(MultiState) :: states
         class(GriddedComponentDriver), intent(in) :: this
      end function get_states

      module function get_clock(this) result(clock)
         use esmf, only: ESMF_Clock
         type(ESMF_Clock) :: clock
         class(GriddedComponentDriver), intent(in) :: this
      end function get_clock

      module subroutine set_clock(this, clock)
         use esmf, only: ESMF_Clock
         class(GriddedComponentDriver), intent(inout) :: this
         type(ESMF_Clock), intent(in) :: clock
      end subroutine set_clock

      recursive module subroutine run_export_couplers(this, unusable, phase_idx, rc)
         class(GriddedComponentDriver), intent(inout) :: this
         class(KE), optional, intent(in) :: unusable
         integer, optional, intent(in) :: phase_idx
         integer, optional, intent(out) :: rc
      end subroutine run_export_couplers

      recursive module subroutine run_import_couplers(this, rc)
         class(GriddedComponentDriver), intent(inout) :: this
         integer, optional, intent(out) :: rc
      end subroutine run_import_couplers

      module subroutine clock_advance(this, rc)
         class(GriddedComponentDriver), intent(inout) :: this
         integer, optional, intent(out) :: rc
      end subroutine clock_advance

      module function get_gridcomp(this) result(gridcomp)
         use esmf, only: ESMF_GridComp
         type(ESMF_GridComp) :: gridcomp 
         class(GriddedComponentDriver), intent(in) :: this
      end function get_gridcomp

      module function get_name(this, rc) result(name)
         character(:), allocatable :: name
         class(GriddedComponentDriver), intent(in) :: this
         integer, optional, intent(out) :: rc
      end function get_name

      module subroutine add_export_coupler(this, driver)
         class(GriddedComponentDriver), intent(inout) :: this
         type(GriddedComponentDriver), intent(in) :: driver
      end subroutine add_export_coupler

      module subroutine add_import_coupler(this, driver)
         class(GriddedComponentDriver), intent(inout) :: this
         type(GriddedComponentDriver), intent(in) :: driver
      end subroutine add_import_coupler

   end interface

contains


   function new_GriddedComponentDriver_all(gridcomp, states, clock) result(driver)
      type(GriddedComponentDriver) :: driver
      type(ESMF_GridComp), intent(in) :: gridcomp
      type(MultiState), optional, intent(in) :: states
      type(ESMF_Clock), optional, intent(in) :: clock

      driver%gridcomp = gridcomp

      if (present(states)) then
         driver%states = states
      else
         driver%states = MultiState()
      end if

      if (present(clock)) then
         driver%clock = clock
      end if
         
   end function new_GriddedComponentDriver_all


end module mapl3g_GriddedComponentDriver
