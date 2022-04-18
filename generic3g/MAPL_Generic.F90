#include "MAPL_ErrLog.h"

!---------------------------------------------------------------------
!
! This module contains procedures that are intended to be called from
! within user-level gridded components.  These are primarily thin
! wrappers that access the internal private state of the gridcomp and
! then invoke methods on that type.
! 
!---------------------------------------------------------------------

module mapl3g_Generic
   use :: mapl3g_InnerMetaComponent, only: InnerMetaComponent
   use :: mapl3g_InnerMetaComponent, only: get_inner_meta
   use :: mapl3g_OuterMetaComponent, only: OuterMetaComponent
   use :: mapl3g_OuterMetaComponent, only: get_outer_meta
   use :: mapl3g_ESMF_Interfaces, only: I_Run
   use :: esmf, only: ESMF_GridComp
   use :: esmf, only: ESMF_Clock
   use :: esmf, only: ESMF_SUCCESS
   use :: esmf, only: ESMF_Method_Flag
   use mapl_ErrorHandling
   use mapl_KeywordEnforcer
   implicit none
   private

   public :: MAPL_GridCompSetEntryPoint
!!$   public :: MAPL_GetInternalState
   public :: MAPL_add_child
   public :: MAPL_run_child
!!$   public :: MAPL_run_children

!!$   public :: MAPL_AddImportSpec
!!$   public :: MAPL_AddExportSpec
!!$   public :: MAPL_AddInternalSpec
!!$
!!$   public :: MAPL_GetResource
   
   ! Accessors
!!$   public :: MAPL_GetConfig
!!$   public :: MAPL_GetOrbit
!!$   public :: MAPL_GetCoordinates
!!$   public :: MAPL_GetLayout

!!$   interface MAPL_GetInternalState
!!$      module procedure :: get_internal_state
!!$   end interface MAPL_GetInternalState

   interface MAPL_add_child
      module procedure :: add_child_by_name
   end interface MAPL_add_child
      
   interface MAPL_run_child
      module procedure :: run_child_by_name
   end interface MAPL_run_child

!!$   interface MAPL_run_children
!!$      module procedure :: run_children
!!$   end interface MAPL_run_children
!!$
!!$   interface MAPL_AddImportSpec
!!$      module procedure :: add_import_spec
!!$   end interface MAPL_AddImportSpec
!!$
!!$   interface MAPL_AddExportSpec
!!$      module procedure :: add_import_spec
!!$   end interface MAPL_AddExportSpec
!!$
!!$   interface MAPL_Get
!!$      module procedure :: get
!!$   end interface MAPL_Get


   interface MAPL_GridCompSetEntryPoint
      module procedure gridcomp_set_entry_point
   end interface MAPL_GridCompSetEntryPoint

contains

   subroutine add_child_by_name(gridcomp, child_name, config, rc)
      use yaFyaml
      type(ESMF_GridComp), intent(inout) :: gridcomp
      character(len=*), intent(in) :: child_name
      class(YAML_Node), intent(inout) :: config
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta

      _HERE,'add_child_by_name'
      outer_meta => get_outer_meta_from_inner_gc(gridcomp, _RC)
      call outer_meta%add_child(child_name, config, _RC)
      
      _RETURN(ESMF_SUCCESS)
   end subroutine add_child_by_name


   subroutine run_child_by_name(gridcomp, child_name, clock, unusable, phase_name, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      character(len=*), intent(in) :: child_name
      type(ESMF_Clock), intent(inout) :: clock
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), optional, intent(in) :: phase_name
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta

      _HERE,'run_child_by_name'
      outer_meta => get_outer_meta(gridcomp, _RC)
      call outer_meta%run_child(child_name, clock, phase_name=phase_name, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine run_child_by_name


   subroutine run_children_(gridcomp, clock, unusable, phase_name, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(ESMF_Clock), intent(inout) :: clock
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), intent(in) :: phase_name
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta

      _HERE,'run_children'
      outer_meta => get_outer_meta_from_inner_gc(gridcomp, _RC)
      call outer_meta%run_children(clock, phase_name=phase_name, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine run_children_


   ! Helper functions to access intenal/private state.
   type(ESMF_GridComp) function get_outer_gridcomp(gridcomp, rc) result(outer_gc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status
      type(InnerMetaComponent), pointer :: inner_meta

      _HERE,'get_outer_gridcomp'
      inner_meta => get_inner_meta(gridcomp, _RC)
      outer_gc = inner_meta%get_outer_gridcomp()
      _RETURN(_SUCCESS)
   end function get_outer_gridcomp


   ! User-level gridded components do not store a reference to the
   ! outer meta component directly, but must instead get it indirectly
   ! through the reference to the outer gridcomp.
   function get_outer_meta_from_inner_gc(gridcomp, rc) result(outer_meta)
      type(OuterMetaComponent), pointer :: outer_meta
      type(ESMF_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_GridComp) :: outer_gc

      _HERE,'get_outer_meta_from_inner_gc'
      outer_gc = get_outer_gridcomp(gridcomp, _RC)
      outer_meta => get_outer_meta(outer_gc, _RC)

      _RETURN(_SUCCESS)
   end function get_outer_meta_from_inner_gc


   subroutine gridcomp_set_entry_point(gridcomp, method_flag, userProcedure, unusable, phase_name, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(ESMF_Method_Flag), intent(in) :: method_flag
      procedure(I_Run) :: userProcedure
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), optional, intent(in) :: phase_name
      integer, optional, intent(out) ::rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta

      _HERE,'gridcomp_set_entry_point'
      outer_meta => get_outer_meta_from_inner_gc(gridcomp, _RC)
      call outer_meta%set_entry_point(method_flag, userProcedure, phase_name=phase_name, _RC)

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine gridcomp_set_entry_point


!!$   subroutine add_import_spec(gridcomp, ...)
!!$   end subroutine add_import_spec
end module mapl3g_Generic
