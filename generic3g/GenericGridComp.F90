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
   use :: mapl3g_GenericConfig
   use esmf
   use :: mapl_KeywordEnforcer, only: KeywordEnforcer
   use :: mapl_ErrorHandling
   implicit none
   private

   ! Procedures
   public :: setServices
   public :: create_grid_comp

   
   ! Named constants
   public :: GENERIC_INIT_GRID
   public :: GENERIC_INIT_ADVERTISE
   public :: GENERIC_INIT_REALIZE
   public :: GENERIC_INIT_USER

   enum, bind(c)
      !!!! IMPORTANT: USER phase must be "1" !!!!
      enumerator :: GENERIC_INIT_USER = 1
      enumerator :: GENERIC_INIT_GRID
      enumerator :: GENERIC_INIT_ADVERTISE
      enumerator :: GENERIC_INIT_REALIZE
   end enum


   interface create_grid_comp
      module procedure create_grid_comp_primary
   end interface create_grid_comp

contains

   recursive subroutine setServices(gridcomp, rc)
      type(ESMF_GridComp) :: gridcomp
      integer, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta

      outer_meta => get_outer_meta(gridcomp, _RC)
      call outer_meta%setservices(_RC)
      call set_entry_points(gridcomp, _RC)

      _RETURN(ESMF_SUCCESS)

   contains

      subroutine set_entry_points(gridcomp, rc)
         type(ESMF_GridComp), intent(inout) :: gridcomp
         integer, intent(out) :: rc
         integer :: status
         integer :: phase

         associate (phases => outer_meta%get_phases(ESMF_METHOD_RUN))
           do phase = 1, phases%size()
              call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, run, phase=phase, _RC)
           end do
         end associate

         ! Mandatory generic initialize phases
         call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, initialize, phase=GENERIC_INIT_GRID, _RC)
         call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, initialize, phase=GENERIC_INIT_ADVERTISE, _RC)
         call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, initialize, phase=GENERIC_INIT_REALIZE, _RC)
!!$         call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, initialize, phase=GENERIC_INIT_RESTORE, _RC)
         call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, initialize, phase=GENERIC_INIT_USER, _RC)

         call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_FINALIZE,     finalize,      _RC)
!!$         call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_READRESTART,  read_restart,  _RC)
!!$         call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_WRITERESTART, write_restart, _RC)

         _RETURN(ESMF_SUCCESS)
      end subroutine set_entry_points

   end subroutine setServices



   
   type(ESMF_GridComp) function create_grid_comp_primary( &
        name, set_services, config, unusable, petlist, rc) result(gridcomp)
      use :: mapl3g_UserSetServices, only: AbstractUserSetServices

      character(*), intent(in) :: name
      class(AbstractUserSetServices), intent(in) :: set_services
      type(GenericConfig), intent(in) :: config
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: petlist(:)
      integer, optional, intent(out) :: rc
      
      type(OuterMetaComponent), pointer :: outer_meta
      type(OuterMetaComponent) :: outer_meta_tmp
      integer :: status

      gridcomp = ESMF_GridCompCreate(name=name, petlist=petlist,  _RC)
      call attach_outer_meta(gridcomp, _RC)
      outer_meta => get_outer_meta(gridcomp, _RC)

#ifdef __GFORTRAN__
      ! GFortran 12. cannot directly assign to outer_meta.  But the
      ! assignment works for an object without the POINTER attribute.
      ! An internal procedure is a workaround, but ... ridiculous.
      call ridiculous(outer_meta, OuterMetaComponent(gridcomp, set_services, config))
#else
      outer_meta = OuterMetaComponent(gridcomp, set_services, config)
#endif

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



   ! Create ESMF GridComp, attach an internal state for meta, and a config.
   type(ESMF_GridComp) function make_basic_gridcomp(name, unusable, petlist, rc) result(gridcomp)
      character(len=*), intent(in) :: name
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: petlist(:)
      integer, optional, intent(out) :: rc

      integer :: status

      gridcomp = ESMF_GridCompCreate(name=name, petlist=petlist,  _RC)
      call attach_outer_meta(gridcomp, _RC)
      
      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function make_basic_gridcomp


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
      case (GENERIC_INIT_GRID)
         call outer_meta%initialize_grid(importState, exportState, clock, _RC)
      case (GENERIC_INIT_ADVERTISE)
         call outer_meta%initialize_advertise(importState, exportState, clock, _RC)
      case (GENERIC_INIT_REALIZE)
         call outer_meta%initialize_realize(importState, exportState, clock, _RC)
!!$      case (GENERIC_INIT_RESTORE)
!!$         call outer_meta%initialize_realize(importState, exportState, clock, _RC)
      case (GENERIC_INIT_USER)
         call outer_meta%initialize_user(importState, exportState, clock, _RC)
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
      integer :: phase
      character(:), pointer :: phase_name
      type(OuterMetaComponent), pointer :: outer_meta
      type(StringVector), pointer :: phases
      
      outer_meta => get_outer_meta(gridcomp, _RC)
      call ESMF_GridCompGet(gridcomp, currentPhase=phase, _RC)

      phases => outer_meta%get_phases(ESMF_METHOD_RUN)
      phase_name => phases%of(phase)
      call outer_meta%run(importState, exportState, clock, phase_name=phase_name, _RC)

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

end module mapl3g_GenericGridComp
