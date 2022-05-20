#include "MAPL_ErrLog.h"

module mapl3g_GenericGridComp
   use :: mapl3g_OuterMetaComponent, only: OuterMetaComponent
   use :: mapl3g_OuterMetaComponent, only: get_outer_meta
   use :: mapl3g_OuterMetaComponent, only: attach_outer_meta
   use esmf
   use :: mapl_KeywordEnforcer, only: KeywordEnforcer
   use :: mapl_ErrorHandling
   implicit none
   private

   public :: setServices
   public :: create_grid_comp


   interface create_grid_comp
      module procedure create_grid_comp_traditional
      module procedure create_grid_comp_advanced
   end interface create_grid_comp

   public :: initialize

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

         call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE,   initialize,    _RC)
         call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_FINALIZE,     finalize,      _RC)
!!$         call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_READRESTART,  read_restart,  _RC)
!!$         call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_WRITERESTART, write_restart, _RC)

         _RETURN(ESMF_SUCCESS)
      end subroutine set_entry_points

   end subroutine setServices
   

   type(ESMF_GridComp) function create_grid_comp_traditional( &
        name, userRoutine, unusable, config, petlist, rc) result(gridcomp)
      use :: mapl3g_UserSetServices, only: user_setservices
      use :: mapl3g_ESMF_Interfaces, only: I_SetServices
      
      character(len=*), intent(in) :: name
      procedure(I_SetServices) :: userRoutine
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_config), optional, intent(inout) :: config
      integer, optional, intent(in) :: petlist(:)
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta

      gridcomp = make_basic_gridcomp(name=name, petlist=petlist, _RC)
      outer_meta => get_outer_meta(gridcomp, _RC)
      if (present(config)) call outer_meta%set_esmf_config(config)
      call outer_meta%set_user_setservices(user_setservices(userRoutine))

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function create_grid_comp_traditional


   type(ESMF_GridComp) function create_grid_comp_advanced( &
        name, config, unusable, petlist, rc) result(gridcomp)
      use :: mapl3g_UserSetServices, only: user_setservices
      use :: yafyaml, only: YAML_Node

      character(len=*), intent(in) :: name
      class(YAML_Node), intent(inout) :: config
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: petlist(:)
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta
!!$      class(YAML_Node), pointer :: dso_yaml
!!$      character(:), allocatable :: sharedObj, userRoutine
      
      gridcomp = make_basic_gridcomp(name=name, petlist=petlist, _RC)
      outer_meta => get_outer_meta(gridcomp, _RC)
      call outer_meta%set_config(config)

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function create_grid_comp_advanced

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


   recursive subroutine initialize(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gridcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta
      
      outer_meta => get_outer_meta(gridcomp, _RC)
      call outer_meta%initialize(importState, exportState, clock, _RC)

      _RETURN(ESMF_SUCCESS)
   end subroutine initialize


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
