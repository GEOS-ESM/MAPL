#include "MAPL_Generic.h"

module mapl3g_ComponentDriver
   use mapl3g_MultiState
   use mapl_ErrorHandlingMod
   use :: esmf
   implicit none
   private

   public :: ComponentDriver

   type :: ComponentDriver
      private
      type(ESMF_GridComp) :: gridcomp
      type(MultiState) :: states
      type(ESMF_Clock) :: clock
   contains
      procedure :: run
      procedure :: initialize
      procedure :: finalize
      procedure :: advance

      ! Accessors
      procedure :: get_clock
      procedure :: set_clock
      procedure :: get_states
      procedure :: get_gridcomp
      procedure :: get_name

   end type ComponentDriver

   interface ComponentDriver
      module procedure new_ComponentDriver
   end interface ComponentDriver

   interface

      module recursive subroutine initialize(this, unusable, phase_idx, rc)
         use :: MaplShared, only: KeywordEnforcer
         class(ComponentDriver), intent(inout) :: this
         class(KeywordEnforcer), optional, intent(in) :: unusable
         integer, optional, intent(in) :: phase_idx
         integer, optional, intent(out) :: rc
      end subroutine initialize

      ! run_self() is implemented in submodule to avoid circular dependency
      ! on OuterMetaComponent.
      module recursive subroutine run(this, unusable, phase_idx, rc)
         use :: MaplShared, only: KeywordEnforcer
         class(ComponentDriver), intent(inout) :: this
         class(KeywordEnforcer), optional, intent(in) :: unusable
         integer, optional, intent(in) :: phase_idx
         integer, optional, intent(out) :: rc
      end subroutine

      module recursive subroutine finalize(this, unusable, phase_idx, rc)
         use :: MaplShared, only: KeywordEnforcer
         class(ComponentDriver), intent(inout) :: this
         class(KeywordEnforcer), optional, intent(in) :: unusable
         integer, optional, intent(in) :: phase_idx
         integer, optional, intent(out) :: rc
      end subroutine finalize

      module subroutine advance(this, rc)
         class(ComponentDriver), intent(inout) :: this
         integer, optional, intent(out) :: rc
      end subroutine advance


      module function get_states(this) result(states)
         use mapl3g_MultiState
         type(MultiState) :: states
         class(ComponentDriver), intent(in) :: this
      end function get_states

      module function get_clock(this) result(clock)
         use esmf, only: ESMF_Clock
         type(ESMF_Clock) :: clock
         class(ComponentDriver), intent(in) :: this
      end function get_clock

      module subroutine set_clock(this, clock)
         use esmf, only: ESMF_Clock
         class(ComponentDriver), intent(inout) :: this
         type(ESMF_Clock), intent(in) :: clock
      end subroutine set_clock

   end interface

contains

   function new_ComponentDriver(gridcomp, clock, states) result(child)
      type(ComponentDriver) :: child
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

   end function new_ComponentDriver


   function get_gridcomp(this) result(gridcomp)
      use esmf, only: ESMF_GridComp
      type(ESMF_GridComp) :: gridcomp
      class(ComponentDriver), intent(in) :: this
      gridcomp = this%gridcomp
   end function get_gridcomp

   function get_name(this, rc) result(name)
      character(:), allocatable :: name
      class(ComponentDriver), intent(in) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: buffer

      call ESMF_GridCompGet(this%gridcomp, name=buffer, _RC)
      name = trim(buffer)

      _RETURN(ESMF_SUCCESS)
   end function get_name

end module mapl3g_ComponentDriver
