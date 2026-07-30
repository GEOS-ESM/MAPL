#include "MAPL.h"

! Each generic initialize phase can be supplemented by the user
! gridcomp if necessary.   User phases are MAPL phases appended by
! "_PRE" or "_POST".
!
! Generic initialize phases:
!     MAPL_PROPAGATE_GRID
!     MAPL_ADVERTISE
!     MAPL_REALIZE

module mapl_GenericGridComp_mod
   use :: mapl_OuterMetaComponent_mod, only: OuterMetaComponent
   use :: mapl_OuterMetaComponent_mod, only: get_outer_meta
   use :: mapl_OuterMetaComponent_mod, only: attach_outer_meta
   use :: mapl_GenericPhases_mod
   use :: mapl_GriddedComponentDriver_mod
   use esmf
   use :: mapl_KeywordEnforcer_mod, only: KeywordEnforcer
   use :: mapl_ErrorHandling_mod
   implicit none(type,external)
   private

   ! Procedures
   public :: GenericSetServices

contains

   recursive subroutine GenericSetServices(gridcomp, rc)
      type(ESMF_GridComp) :: gridcomp
      integer, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta
      type(ESMF_HConfig) :: hconfig
      character(len=ESMF_MAXSTR) :: comp_name
      type(ESMF_GridComp) :: user_gridcomp
      type(GriddedComponentDriver) :: user_gc_driver

      outer_meta => get_outer_meta(gridcomp, rc=status)
      if (status /= ESMF_SUCCESS .or. .not. associated(outer_meta)) then
         call set_is_generic(gridcomp, .true., _RC)
         call attach_outer_meta(gridcomp, _RC)
         outer_meta => get_outer_meta(gridcomp, _RC)

         call ESMF_GridCompGet(gridcomp, name=comp_name, config=hconfig, _RC)

         user_gridcomp = ESMF_GridCompCreate(name=comp_name, _RC)
         call set_is_generic(user_gridcomp, .false., _RC)
         user_gc_driver = GriddedComponentDriver(user_gridcomp)

         outer_meta%self_gridcomp = gridcomp
         outer_meta%user_gc_driver = user_gc_driver
         outer_meta%hconfig = hconfig
         call outer_meta%init_meta(_RC)
      end if

      call outer_meta%setServices(_RC)
      call set_entry_points(gridcomp, _RC)

      _RETURN(ESMF_SUCCESS)

   contains

      subroutine set_entry_points(gridcomp, rc)
         type(ESMF_GridComp), intent(inout) :: gridcomp
         integer, intent(out) :: rc
         integer :: status
         integer :: phase_idx

         integer, parameter :: NUM_GENERIC_RUN_PHASES = 1

         ! Mandatory generic initialize phases
         call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, initialize, phase=GENERIC_INIT_SET_CLOCK, _RC)
         call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, initialize, phase=GENERIC_INIT_GEOM_A, _RC)
         call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, initialize, phase=GENERIC_INIT_GEOM_B, _RC)
         call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, initialize, phase=GENERIC_INIT_ADVERTISE, _RC)
         call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, initialize, phase=GENERIC_INIT_MODIFY_ADVERTISED, _RC)
         call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, initialize, phase=GENERIC_INIT_REALIZE, _RC)
         call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, initialize, phase=GENERIC_INIT_READ_RESTART, _RC)
         call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, initialize, phase=GENERIC_INIT_USER, _RC)

         ! Run phases, including mandatory
         call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, run, phase=GENERIC_RUN_CLOCK_ADVANCE, _RC)

         associate (phases => outer_meta%get_phases(ESMF_METHOD_RUN))
           do phase_idx = 1, phases%size()
              call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, run, phase=phase_idx, _RC)
           end do
         end associate

         call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_FINALIZE,     finalize,      _RC)
         call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_WRITERESTART, write_restart, _RC)

         _RETURN(ESMF_SUCCESS)
      end subroutine set_entry_points

   end subroutine GenericSetServices

   ! Generic initialize phases are always executed.  User component can specify
   ! additional pre-action for each phase.
   recursive subroutine initialize(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gridcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

      integer :: status
      integer :: phase
      type(OuterMetaComponent), pointer :: outer_meta

      outer_meta => get_outer_meta(gridcomp, _RC)
      call ESMF_GridCompGet(gridcomp, currentPhase=phase, _RC)
      select case (phase)
      case (GENERIC_INIT_SET_CLOCK)
         call outer_meta%initialize_set_clock(clock, _RC)
      case (GENERIC_INIT_GEOM_A)
         call outer_meta%initialize_geom_a(_RC)
      case (GENERIC_INIT_GEOM_B)
         call outer_meta%initialize_geom_b(_RC)
      case (GENERIC_INIT_ADVERTISE)
         call outer_meta%initialize_advertise(_RC)
      case (GENERIC_INIT_MODIFY_ADVERTISED)
         call outer_meta%initialize_modify_advertised(importState, exportState, clock, _RC)
      case (GENERIC_INIT_REALIZE)
         call outer_meta%initialize_realize(importState, exportState, clock, _RC)
      case (GENERIC_INIT_READ_RESTART)
         call outer_meta%initialize_read_restart(_RC)
      case (GENERIC_INIT_USER)
         call outer_meta%initialize_user(_RC)
      case default
         _FAIL('Unknown generic phase ')
      end select

      _RETURN(ESMF_SUCCESS)
   end subroutine initialize

   ! The only run phases are those specified by the user component.
   recursive subroutine run(gridcomp, importState, exportState, clock, rc)
      use gFTL2_StringVector
      type(ESMF_GridComp) :: gridcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

      integer :: status
      integer :: phase_idx
      character(:), pointer :: phase_name
      type(OuterMetaComponent), pointer :: outer_meta
      type(StringVector), pointer :: phases

      outer_meta => get_outer_meta(gridcomp, _RC)
      call ESMF_GridCompGet(gridcomp, currentPhase=phase_idx, _RC)
      select case (phase_idx)
      case (GENERIC_RUN_CLOCK_ADVANCE)
         call outer_meta%run_clock_advance(clock, _RC)
      case default ! user-defined run phase
         phases => outer_meta%get_phases(ESMF_METHOD_RUN)
         phase_name => phases%of(phase_idx)
         call outer_meta%run_user(clock, phase_name=phase_name, _RC)
      end select

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
   end subroutine run


   recursive subroutine finalize(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gridcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta

      outer_meta => get_outer_meta(gridcomp, _RC)
      call outer_meta%finalize(importState, exportState, clock, _RC)

      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _RETURN(ESMF_SUCCESS)
   end subroutine finalize


   recursive subroutine write_restart(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gridcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta

      outer_meta => get_outer_meta(gridcomp, _RC)
      call outer_meta%write_restart(importState, exportState, clock, _RC)

      _RETURN(ESMF_SUCCESS)
   end subroutine write_restart

   ! Parent components name their children, but such names should
   ! apply to the (inner) user grid comp.  The MAPL wrapper gridcomp,
   ! has a different name derived from that name.
   ! "A" -->   "[A]"
   function outer_name(inner_name)
      character(:), allocatable :: outer_name
      character(*), intent(in) :: inner_name

      outer_name = "[" // inner_name // "]"
   end function outer_name

   subroutine set_is_generic(gridcomp, flag, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      logical, optional, intent(in) :: flag
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: flag_
      type(ESMF_Info) :: info

      flag_ = .true.
      if (present(flag)) flag_ = flag

      call ESMF_InfoGetFromHost(gridcomp, info, _RC)
      call ESMF_InfoSet(info, key='MAPL/GRIDCOMP_IS_GENERIC', value=flag_, _RC)

      _RETURN(_SUCCESS)
   end subroutine set_is_generic
end module mapl_GenericGridComp_mod
