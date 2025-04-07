#include "MAPL_ErrLog.h"

! Each generic initialize phase can be supplemented by the user
! gridcomp if necessary.   User phases are MAPL phases appended by
! "_PRE" or "_POST".
!
! Generic initialize phases:
!     MAPL_PROPAGATE_GRID
!     MAPL_ADVERTISE
!     MAPL_REALIZE

module mapl3g_GenericGridComp
   use :: mapl3g_OuterMetaComponent, only: OuterMetaComponent
   use :: mapl3g_OuterMetaComponent, only: get_outer_meta
   use :: mapl3g_OuterMetaComponent, only: attach_outer_meta
   use :: mapl3g_GenericPhases
   use :: mapl3g_GriddedComponentDriver
   use esmf
   use :: mapl_KeywordEnforcer, only: KeywordEnforcer
   use :: mapl_ErrorHandling
   implicit none
   private

   ! Procedures
   public :: setServices
   public :: MAPL_GridCompCreate

   interface MAPL_GridCompCreate
      module procedure create_grid_comp_primary
   end interface MAPL_GridCompCreate

contains

   recursive subroutine setServices(gridcomp, rc)
      type(ESMF_GridComp) :: gridcomp
      integer, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta

      outer_meta => get_outer_meta(gridcomp, _RC)
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
         call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, initialize, phase=GENERIC_INIT_ADVERTISE, _RC)
         call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, initialize, phase=GENERIC_INIT_MODIFY_ADVERTISED, _RC)
         call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, initialize, phase=GENERIC_INIT_MODIFY_ADVERTISED2, _RC)
         call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, initialize, phase=GENERIC_INIT_REALIZE, _RC)
!#         call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, initialize, phase=GENERIC_INIT_RESTORE, _RC)
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

   end subroutine setServices




   recursive type(ESMF_GridComp) function create_grid_comp_primary( &
        name, set_services, config, unusable, petlist, rc) result(gridcomp)
      use :: mapl3g_UserSetServices, only: AbstractUserSetServices

      character(*), intent(in) :: name
      class(AbstractUserSetServices), intent(in) :: set_services
      type(ESMF_HConfig), intent(in) :: config
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: petlist(:)
      integer, optional, intent(out) :: rc

      type(ESMF_GridComp) :: user_gridcomp
      type(OuterMetaComponent), pointer :: outer_meta
      type(GriddedComponentDriver) :: user_gc_driver
      type(ESMF_Context_Flag) :: contextFlag
      integer :: status

      contextFlag = ESMF_CONTEXT_PARENT_VM
      if(present(petlist)) contextFlag = ESMF_CONTEXT_OWN_VM
      gridcomp = ESMF_GridCompCreate(name=outer_name(name), &
           petlist=petlist, contextFlag=contextFlag, _RC)
      call set_is_generic(gridcomp, _RC)

      user_gridcomp = ESMF_GridCompCreate(name=name, petlist=petlist, contextFlag=contextFlag, _RC)
      call set_is_generic(user_gridcomp, .false., _RC)

      call attach_outer_meta(gridcomp, _RC)
      outer_meta => get_outer_meta(gridcomp, _RC)

      ! We copy the outer gridcomp here.  If the user gridcomp runs at a different (slower!) timestep, that
      ! must be processed later as the information gets stored in the ComponentSpec.

      user_gc_driver = GriddedComponentDriver(user_gridcomp)
#ifndef __GFORTRAN__
      outer_meta = OuterMetaComponent(gridcomp, user_gc_driver, set_services, config)
#else
      ! GFortran 12 & 13 cannot directly assign to outer_meta.  But
      ! the assignment works for an object without the POINTER
      ! attribute.  An internal procedure is a workaround, but
      ! ... ridiculous.
      call ridiculous(outer_meta, OuterMetaComponent(gridcomp, user_gc_driver, set_services, config))
#endif
      call outer_meta%init_meta(_RC)

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)
#ifdef __GFORTRAN__
   contains

      subroutine ridiculous(a, b)
         type(OuterMetaComponent), intent(out) :: a
         type(OuterMetaComponent), intent(in) :: b
         a = b
      end subroutine ridiculous
#endif
   end function create_grid_comp_primary


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
      case (GENERIC_INIT_ADVERTISE)
         call outer_meta%initialize_advertise(_RC)
      case (GENERIC_INIT_MODIFY_ADVERTISED)
         call outer_meta%initialize_modify_advertised(importState, exportState, clock, _RC)
      case (GENERIC_INIT_MODIFY_ADVERTISED2)
         call outer_meta%initialize_modify_advertised2(importState, exportState, clock, _RC)
      case (GENERIC_INIT_REALIZE)
         call outer_meta%initialize_realize(_RC)
!#      case (GENERIC_INIT_RESTORE)
!#         call outer_meta%initialize_realize(_RC)
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
end module mapl3g_GenericGridComp
