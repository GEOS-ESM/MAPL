#include "MAPL_Generic.h"

module mapl3g_GriddedComponentDriver
   use mapl3g_MultiState
   use mapl3g_ComponentDriver
   use mapl3g_ComponentDriverVector
   use mapl_ErrorHandlingMod
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
      procedure :: run
      procedure :: initialize
      procedure :: finalize

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
      module procedure new_GriddedComponentDriver
   end interface GriddedComponentDriver

   interface

      module recursive subroutine initialize(this, unusable, phase_idx, rc)
         use :: MaplShared, only: KeywordEnforcer
         class(GriddedComponentDriver), intent(inout) :: this
         class(KeywordEnforcer), optional, intent(in) :: unusable
         integer, optional, intent(in) :: phase_idx
         integer, optional, intent(out) :: rc
      end subroutine initialize

      ! run_self() is implemented in submodule to avoid circular dependency
      ! on OuterMetaComponent.
      module recursive subroutine run(this, unusable, phase_idx, rc)
         use :: MaplShared, only: KeywordEnforcer
         class(GriddedComponentDriver), intent(inout) :: this
         class(KeywordEnforcer), optional, intent(in) :: unusable
         integer, optional, intent(in) :: phase_idx
         integer, optional, intent(out) :: rc
      end subroutine

      module recursive subroutine finalize(this, unusable, phase_idx, rc)
         use :: MaplShared, only: KeywordEnforcer
         class(GriddedComponentDriver), intent(inout) :: this
         class(KeywordEnforcer), optional, intent(in) :: unusable
         integer, optional, intent(in) :: phase_idx
         integer, optional, intent(out) :: rc
      end subroutine finalize


      module function get_states(this) result(states)
         use mapl3g_MultiState
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
         use :: MaplShared, only: KeywordEnforcer
         class(GriddedComponentDriver), intent(inout) :: this
         class(KeywordEnforcer), optional, intent(in) :: unusable
         integer, optional, intent(in) :: phase_idx
         integer, optional, intent(out) :: rc
      end subroutine run_export_couplers

      recursive module subroutine run_import_couplers(this, rc)
         class(GriddedComponentDriver), intent(inout) :: this
         integer, optional, intent(out) :: rc
      end subroutine run_import_couplers

   end interface

contains

   function new_GriddedComponentDriver(gridcomp, clock, states) result(child)
      type(GriddedComponentDriver) :: child
      type(ESMF_GridComp), intent(in) :: gridcomp
      type(ESMF_Clock), optional, intent(in) :: clock
      type(MultiState), optional, intent(in) :: states

      child%gridcomp = gridcomp
      ! Allow for lazy initialization of clock
      if (present(clock)) child%clock = clock

      if (present(states)) then
         child%states = states
         return
      end if
      child%states = MultiState()

   end function new_GriddedComponentDriver


   function get_gridcomp(this) result(gridcomp)
      use esmf, only: ESMF_GridComp
      type(ESMF_GridComp) :: gridcomp
      class(GriddedComponentDriver), intent(in) :: this
      gridcomp = this%gridcomp
   end function get_gridcomp

   function get_name(this, rc) result(name)
      character(:), allocatable :: name
      class(GriddedComponentDriver), intent(in) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: buffer

      call ESMF_GridCompGet(this%gridcomp, name=buffer, _RC)
      name = trim(buffer)

      _RETURN(ESMF_SUCCESS)
   end function get_name

   subroutine add_export_coupler(this, driver)
      class(GriddedComponentDriver), intent(inout) :: this
      type(GriddedComponentDriver), intent(in) :: driver
      call this%export_couplers%push_back(driver)
   end subroutine add_export_coupler

   subroutine add_import_coupler(this, driver)
      class(GriddedComponentDriver), intent(inout) :: this
      type(GriddedComponentDriver), intent(in) :: driver
      call this%import_couplers%push_back(driver)
   end subroutine add_import_coupler


end module mapl3g_GriddedComponentDriver
