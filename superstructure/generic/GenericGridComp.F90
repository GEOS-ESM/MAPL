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
   public :: GridCompCreate

   interface GridCompCreate
      procedure create_grid_comp_primary
   end interface GridCompCreate

contains

   recursive subroutine GenericSetServices(gridcomp, rc)
       type(ESMF_GridComp) :: gridcomp
       integer, intent(out) :: rc

       integer :: status
       type(OuterMetaComponent), pointer :: outer_meta

       call get_or_create_outer_meta(gridcomp, outer_meta, _RC)
       call outer_meta%setServices(_RC)
       call set_entry_points(gridcomp, _RC)

       _RETURN(ESMF_SUCCESS)

    contains

      subroutine get_or_create_outer_meta(gridcomp, outer_meta, rc)
         type(ESMF_GridComp), intent(inout) :: gridcomp
         type(OuterMetaComponent), pointer, intent(out) :: outer_meta
         integer, intent(out) :: rc

         type :: PrivateWrapper
            type(OuterMetaComponent), pointer :: ptr
         end type PrivateWrapper

         type(ESMF_Config) :: config
         type(ESMF_HConfig) :: hconfig
         type(PrivateWrapper) :: wrapper
         type(ESMF_GridComp) :: user_gridcomp
         type(GriddedComponentDriver) :: user_gc_driver
         integer :: status
         character(ESMF_MAXSTR) :: name

         call ESMF_InternalStateGet(gridcomp, internalState=wrapper, &
              label='MAPL::OuterMetaComponent::private', rc=status)
         if (status == ESMF_SUCCESS) then
            outer_meta => wrapper%ptr
            _RETURN(_SUCCESS)
         end if

         call ESMF_GridCompGet(gridcomp, config=config, _RC)
         call ESMF_GridCompGet(gridcomp, name=name, _RC)
         call ESMF_ConfigGet(config, hconfig=hconfig, _RC)

         user_gridcomp = ESMF_GridCompCreate(name=trim(name), _RC)
         call set_is_generic(user_gridcomp, .false., _RC)
         user_gc_driver = GriddedComponentDriver(user_gridcomp)
         call set_is_generic(gridcomp, _RC)
         call attach_outer_meta(gridcomp, _RC)
         outer_meta => get_outer_meta(gridcomp, _RC)
#ifndef __GFORTRAN__
         outer_meta = OuterMetaComponent(gridcomp, user_gc_driver, hconfig=hconfig)
#else
         call ridiculous(outer_meta, OuterMetaComponent(gridcomp, user_gc_driver, hconfig=hconfig))
#endif
         call outer_meta%init_meta(_RC)

         _RETURN(_SUCCESS)
      end subroutine get_or_create_outer_meta

#ifdef __GFORTRAN__

      subroutine ridiculous(a, b)
         type(OuterMetaComponent), intent(out) :: a
         type(OuterMetaComponent), intent(in) :: b
         a = b
      end subroutine ridiculous
#endif

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
         call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, initialize, phase=GENERIC_INIT_REALIZE_PROVIDED, _RC)
         call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, initialize, phase=GENERIC_INIT_ACCEPT_TRANSFER, _RC)
         call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, initialize, phase=GENERIC_INIT_REALIZE_ACCEPTED, _RC)
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

         ! Internal (in-memory) checkpoint phases.  These are registered
         ! directly against ESMF_METHOD_READRESTART/WRITERESTART and are
         ! intentionally NOT part of GENERIC_INIT_PHASE_SEQUENCE.
         call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_READRESTART,  read_restart,  phase=GENERIC_INTERNAL_READ_RESTART, _RC)
         call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_WRITERESTART, write_restart, phase=GENERIC_INTERNAL_WRITE_RESTART, _RC)

         _RETURN(ESMF_SUCCESS)
      end subroutine set_entry_points

   end subroutine GenericSetServices




   recursive type(ESMF_GridComp) function create_grid_comp_primary( &
        name, set_services, config, unusable, petlist, rc) result(gridcomp)
      use :: mapl_UserSetServices_mod, only: UserSetServices

      character(*), intent(in) :: name
      class(UserSetServices), optional, intent(in) :: set_services
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
      if (present(set_services)) then
         outer_meta = OuterMetaComponent(gridcomp, user_gc_driver, set_services, config)
      else
         outer_meta = OuterMetaComponent(gridcomp, user_gc_driver, hconfig=config)
      end if
#else
      ! GFortran 12 & 13 cannot directly assign to outer_meta.  But
      ! the assignment works for an object without the POINTER
      ! attribute.  An internal procedure is a workaround, but
      ! ... ridiculous.
      if (present(set_services)) then
         call ridiculous(outer_meta, OuterMetaComponent(gridcomp, user_gc_driver, set_services, config))
      else
         call ridiculous(outer_meta, OuterMetaComponent(gridcomp, user_gc_driver, hconfig=config))
      end if
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
      case (GENERIC_INIT_GEOM_A)
         call outer_meta%initialize_geom_a(_RC)
      case (GENERIC_INIT_GEOM_B)
         call outer_meta%initialize_geom_b(_RC)
      case (GENERIC_INIT_ADVERTISE)
         call outer_meta%initialize_advertise(_RC)
       case (GENERIC_INIT_MODIFY_ADVERTISED)
          call outer_meta%initialize_modify_advertised(importState, exportState, clock, _RC)
       case (GENERIC_INIT_REALIZE_PROVIDED)
          call outer_meta%initialize_realize_provided(importState, exportState, clock, _RC)
       case (GENERIC_INIT_ACCEPT_TRANSFER)
          call outer_meta%initialize_accept_transfer(importState, exportState, clock, _RC)
       case (GENERIC_INIT_REALIZE_ACCEPTED)
          call outer_meta%initialize_realize_accepted(importState, exportState, clock, _RC)
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

   ! Dedicated ESMF_METHOD_READRESTART entry point.  Unlike write_restart,
   ! no prior outer-level READRESTART entry point existed: existing netCDF
   ! restart reads run under ESMF_METHOD_INITIALIZE phase
   ! GENERIC_INIT_READ_RESTART via initialize_read_restart, which this
   ! procedure does not touch or replace.
   recursive subroutine read_restart(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gridcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta

      outer_meta => get_outer_meta(gridcomp, _RC)
      call outer_meta%read_restart(importState, exportState, clock, _RC)

      _RETURN(ESMF_SUCCESS)
   end subroutine read_restart

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
