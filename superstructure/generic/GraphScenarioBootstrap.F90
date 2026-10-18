#include "MAPL.h"

!------------------------------------------------------------------------------
! GraphScenarioBootstrap: reusable, non-pFUnit-coupled extraction of
! Test_Scenarios.pf's own setup() bootstrap sequence (docs/graph/spec/
! 20-implementation-roadmap.md sec 20.4.21, Phase 21, sub-step 21-1).
!
! Goal (sized for this one extraction, per the roadmap's own exit
! criterion): take a top-level YAML config file path in, drive it
! through the real ESMF_GridCompCreate -> ESMF_GridCompSetServices ->
! GENERIC_INIT_PHASE_SEQUENCE sequence Test_Scenarios.pf's own setup()
! already performs, and hand back a real, fully-advertised-and-
! connected OuterMetaComponent tree (reached via the returned
! ESMF_GridComp and get_outer_meta()) - exactly the input the Phase 21
! driver (21-2) needs to walk and export.
!
! Deliberately NOT reproduced here: setup()'s own ESMF_HConfig-driven
! numsteps/run-phase loop, its tearDown(), or anything about pFUnit's
! own test-case machinery. This module stops at "tree is built and
! initialized" - running GENERIC_RUN_USER (if a caller wants the graph
! to additionally reflect a post-run-phase pull-hook state) is each
! caller's own choice, not this subroutine's job.
!
! Mirrors setup()'s own choices exactly where they matter for a
! faithful re-use:
!   - ESMF_GridCompCreate(name='ROOT', ..., contextFlag=ESMF_CONTEXT_PARENT_VM)
!   - mapl_GenericSetservices as the top-level SetServices routine
!   - a one-hour clock, 2000-04-03T21:00:00 -> T22:00:00 (arbitrary;
!     no scenario fixture's own expectations depend on the exact
!     value, only that a valid clock exists for GENERIC_INIT_SET_CLOCK)
!   - every entry of GENERIC_INIT_PHASE_SEQUENCE run in order
!
! NOT reproduced from setup(): the NUOPC_FieldDictionarySetup() call
! against './scenarios/FieldDictionary.yml' - that path is specific to
! superstructure/generic/tests/'s own WORKING_DIRECTORY convention.
! Callers running against a different working directory (e.g. the
! Phase 21 driver, apps/graph_diagnostic/) must call
! NUOPC_FieldDictionarySetup themselves first (or rely on
! MAPL_Initialize's own default field-dictionary loading) - this
! module does not hardcode a path that would only be valid from one
! specific caller's own working directory.
!------------------------------------------------------------------------------
module mapl_GraphScenarioBootstrap_mod
   use mapl_OuterMetaComponent_mod, only: OuterMetaComponent, get_outer_meta
   use mapl_GenericPhases_mod, only: GENERIC_INIT_PHASE_SEQUENCE
   use mapl_Generic_api, only: mapl_GenericSetservices
   use mapl_KeywordEnforcer_mod, only: KE => KeywordEnforcer
   use mapl_ErrorHandling_mod
   use esmf
   implicit none(type, external)
   private

   public :: build_scenario_tree

contains

   ! Builds and fully initializes (through the end of
   ! GENERIC_INIT_PHASE_SEQUENCE) a real OuterMetaComponent tree rooted
   ! at a top-level YAML config file, mirroring
   ! superstructure/generic/tests/Test_Scenarios.pf's own setup().
   !
   ! root_name: the ESMF_GridComp name assigned to the root component
   ! (cosmetic only - 'ROOT', matching setup()'s own hardcoded value,
   ! unless a caller needs a different name for diagnostic labeling).
   !
   ! outer_gc/outer_meta: the real root ESMF_GridComp and its
   ! OuterMetaComponent (reached via get_outer_meta(outer_gc)) - a
   ! caller walks the tree from outer_meta using
   ! get_num_children/get_child_name/get_child_outer_meta, exactly as
   ! GraphBuilder_Freeze_smod.F90's own freeze_subtree_ does.
   !
   ! clock/importState/exportState are returned because they remain
   ! live ESMF handles the caller owns (e.g. to run a further
   ! GENERIC_RUN_USER phase, as Test_GraphModeDemonstration.pf's own
   ! fixtures do) - this subroutine does not destroy them.
   subroutine build_scenario_tree(config_file, outer_gc, outer_meta, unusable, &
        root_name, clock, importState, exportState, rc)
      character(*), intent(in) :: config_file
      type(ESMF_GridComp), intent(out) :: outer_gc
      type(OuterMetaComponent), pointer, intent(out) :: outer_meta
      class(KE), optional, intent(in) :: unusable
      character(*), optional, intent(in) :: root_name
      type(ESMF_Clock), optional, intent(out) :: clock
      type(ESMF_State), optional, intent(out) :: importState
      type(ESMF_State), optional, intent(out) :: exportState
      integer, optional, intent(out) :: rc

      integer :: status, user_status
      integer :: i
      character(:), allocatable :: name_
      type(ESMF_HConfig) :: config
      type(ESMF_Config) :: config_cf
      type(ESMF_State) :: importState_, exportState_
      type(ESMF_Clock) :: clock_
      type(ESMF_Time) :: t0, t1
      type(ESMF_TimeInterval) :: dt

      name_ = 'ROOT'
      if (present(root_name)) name_ = root_name

      config = ESMF_HConfigCreate(filename=config_file, _RC)

      call ESMF_TimeSet(t0, timeString="2000-04-03T21:00:00", _RC)
      call ESMF_TimeSet(t1, timeString="2000-04-03T22:00:00", _RC)
      call ESMF_TimeIntervalSet(dt, h=1)
      clock_ = ESMF_ClockCreate(timestep=dt, startTime=t0, stoptime=t1, refTime=t0, _RC)

      config_cf = ESMF_ConfigCreate(hconfig=config, _RC)
      outer_gc = ESMF_GridCompCreate(name=name_, config=config_cf, contextFlag=ESMF_CONTEXT_PARENT_VM, _RC)
      call ESMF_GridCompSetServices(outer_gc, mapl_GenericSetservices, userRC=user_status, _RC)
      _VERIFY(user_status)

      importState_ = ESMF_StateCreate(_RC)
      exportState_ = ESMF_StateCreate(_RC)

      do i = 1, size(GENERIC_INIT_PHASE_SEQUENCE)
         associate (phase => GENERIC_INIT_PHASE_SEQUENCE(i))
            call ESMF_GridCompInitialize(outer_gc, &
                 importState=importState_, exportState=exportState_, clock=clock_, &
                 phase=phase, userRC=user_status, _RC)
            _VERIFY(user_status)
         end associate
      end do

      outer_meta => get_outer_meta(outer_gc, _RC)

      if (present(clock)) clock = clock_
      if (present(importState)) importState = importState_
      if (present(exportState)) exportState = exportState_

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine build_scenario_tree

end module mapl_GraphScenarioBootstrap_mod
