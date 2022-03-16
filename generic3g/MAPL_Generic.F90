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
   use :: mapl3g_InnerMetaComponent, only:
   use :: mapl3g_OuterMetaComponent, only: OuterMetaComponent
   use :: esmf, only: ESMF_GridComp
   implicit none
   private

   public :: MAPL_GridCompSetEntryPoint
   public :: MAPL_GetInternalState
   public :: MAPL_add_child
   public :: MAPL_run_child
   public :: MAPL_run_children

   public :: MAPL_AddImportSpec
   public :: MAPL_AddExportSpec
   public :: MAPL_AddInternalSpec

   public :: MAPL_GetResource
   
   ! Accessors
   public :: MAPL_GetConfig
   public :: MAPL_GetOrbit
   public :: MAPL_GetCoordinates
   public :: MAPL_GetLayout
   public :: MAPL_

   interface MAPL_GetInternalState
      module procedure :: get_internal_state
   end interface MAPL_GetInternalState

   interface MAPL_add_child
      module procedure :: add_child_by_name
   end interface MAPL_add_child
      
   interface MAPL_run_child
      module procedure :: run_child_by_name
   end interface MAPL_run_child

   interface MAPL_run_children
      module procedure :: run_children
   end interface MAPL_run_children

   interface MAPL_AddImportSpec
      module procedure :: add_import_spec
   end interface MAPL_AddImportSpec

   interface MAPL_AddExportSpec
      module procedure :: add_import_spec
   end interface MAPL_AddExportSpec

   interface MAPL_Get
      module procedure :: get
   end interface MAPL_Get


contains

   subroutine add_child_by_name(gridcomp, child_name, config, rc)
      class(ESMF_GridComp), intent(inout) :: gridcomp
      character(len=*), intent(in) :: child_name
      type(Configuration), intent(in) :: config
      integer, optional, intent(out) :: rc

      integer :: status

      outer_meta => get_outer_meta(gridcomp, _RC)
      call outer_meta%add_child(child_name, config, _RC)
      
      _RETURN(ESMF_SUCCESS)
   end subroutine add_child_by_name


   subroutine run_child_by_name(gridcomp, child_name, clock, unusable, phase_name, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      character(len=*), intent(in) :: child_name
      type(ESMF_Clock), intent(inout) :: clock
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), intent(in) :: phase_name
      integer, optional, intent(out) :: rc

      integer :: status

      outer_meta => get_outer_meta(this%gridcomp, _RC)
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

      integer :: status,

      outer_meta => get_outer_meta(this%gridcomp, _RC)
      call outer_meta%run_children(clock, phase_name=phase_name, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine run_children_


   ! Helper functions to access intenal/private state.
   type(ESMF_GridComp) function get_outer_gridcomp(gridcomp, rc) result(outer_gc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status
      
      inner_meta => get_inner_meta(gridcomp, _RC)
      outer_gc = inner_meta%get_outer_gridcomp()

      _RETURN(_SUCCESS)
   end function get_outer_gridcomp


   ! User-level gridded components do not store a reference to the
   ! outer meta component directly, but must instead get it indirectly
   ! through the reference to the outer gridcomp.
   function get_outer_meta(gridcomp, rc) result(outer_meta)
      type(OuterMetaComponent), pointer :: outer_meta
      type(ESMF_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status

      outer_gc = get_outer_gridcomp(gridcomp, _RC)
      outer_meta => get_outer_meta(outer_gc, _RC)

      _RETURN(_SUCCESS)
   end function get_outer_gridcomp

   
end module mapl3g_Generic
