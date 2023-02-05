#include "MAPL_ErrLog.h"

!---------------------------------------------------------------------
!
! This module contains procedures that are intended to be called from
! within user-level gridded components.  These are primarily thin
! wrappers that access the internal private state of the gridcomp and
! then invoke methods on that type.
!
! The names of these procedures are meant to be backward compatible
! with earlier MAPL.  However, not all interfaces will be provided.
! E.g., MAPL2 usually provided gridcomp and meta overloads for many
! procedures.  Now the "meta" interfaces are OO methods in either
! inner or outer MetaComponent.
! 
!---------------------------------------------------------------------

module mapl3g_Generic
   use :: mapl3g_InnerMetaComponent, only: InnerMetaComponent
   use :: mapl3g_InnerMetaComponent, only: get_inner_meta
   use :: mapl3g_OuterMetaComponent, only: OuterMetaComponent
   use :: mapl3g_OuterMetaComponent, only: get_outer_meta
   use :: mapl3g_ComponentSpec, only: ComponentSpec
   use :: mapl3g_VariableSpec, only: VariableSpec
   use :: mapl3g_Validation, only: is_valid_name
   use :: mapl3g_ESMF_Interfaces, only: I_Run
   use :: mapl3g_AbstractStateItemSpec
   use :: esmf, only: ESMF_GridComp
   use :: esmf, only: ESMF_GeomBase, ESMF_GeomBaseCreate
   use :: esmf, only: ESMF_Grid, ESMF_Mesh, ESMF_Xgrid, ESMF_LocStream
   use :: esmf, only: ESMF_STAGGERLOC_INVALID
   use :: esmf, only: ESMF_Clock
   use :: esmf, only: ESMF_SUCCESS
   use :: esmf, only: ESMF_Method_Flag
   use :: esmf, only: ESMF_STATEINTENT_IMPORT, ESMF_STATEINTENT_EXPORT
   use mapl_ErrorHandling
   use mapl_KeywordEnforcer
   implicit none
   private

   
   public :: MAPL_GridCompSetEntryPoint
   public :: MAPL_add_child
   public :: MAPL_run_child
   public :: MAPL_run_children

!!$   public :: MAPL_GetInternalState

   public :: MAPL_AddImportSpec
   public :: MAPL_AddExportSpec
   public :: MAPL_AddInternalSpec
!!$
!!$   public :: MAPL_GetResource
   
   ! Accessors
!!$   public :: MAPL_GetConfig
!!$   public :: MAPL_GetOrbit
!!$   public :: MAPL_GetCoordinates
!!$   public :: MAPL_GetLayout

   public :: MAPL_SetGeomBase

   interface MAPL_SetGeom
      module procedure MAPL_SetGeomBase
      module procedure MAPL_SetGeomGrid
      module procedure MAPL_SetGeomMesh
      module procedure MAPL_SetGeomXgrid
      module procedure MAPL_SetGeomLocStream
   end interface MAPL_SetGeom


!!$   interface MAPL_GetInternalState
!!$      module procedure :: get_internal_state
!!$   end interface MAPL_GetInternalState


   ! Interfaces
   
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
!!$      module procedure :: add_import_field_spec
   end interface MAPL_AddImportSpec

   interface MAPL_AddExportSpec
      module procedure :: add_export_spec
   end interface MAPL_AddExportSpec

   interface MAPL_AddInternalSpec
      module procedure :: add_internal_spec
   end interface MAPL_AddInternalSpec

!!$   interface MAPL_Get
!!$      module procedure :: get
!!$   end interface MAPL_Get


   interface MAPL_GridCompSetEntryPoint
      module procedure gridcomp_set_entry_point
   end interface MAPL_GridCompSetEntryPoint

contains

   subroutine add_child_by_name(gridcomp, child_name, setservices, config, rc)
      use mapl3g_UserSetServices
      use mapl3g_GenericConfig
      type(ESMF_GridComp), intent(inout) :: gridcomp
      character(len=*), intent(in) :: child_name
      class(AbstractUserSetServices), intent(in) :: setservices
      type(GenericConfig), intent(inout) :: config
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta

      _ASSERT(is_valid_name(child_name), 'Child name <' // child_name //'> does not conform to GEOS standards.')
      outer_meta => get_outer_meta_from_inner_gc(gridcomp, _RC)
      call outer_meta%add_child(child_name, setservices, config, _RC)
      
      _RETURN(ESMF_SUCCESS)
   end subroutine add_child_by_name


   ! In this procedure, gridcomp is actually an _outer_ gridcomp.   The intent is that
   ! an inner gridcomp will call this on its child which is a wrapped user comp.
   
   subroutine run_child_by_name(gridcomp, child_name, clock, unusable, phase_name, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      character(len=*), intent(in) :: child_name
      type(ESMF_Clock), intent(inout) :: clock
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), optional, intent(in) :: phase_name
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta

      outer_meta => get_outer_meta(gridcomp, _RC)
      call outer_meta%run_child(child_name, clock, phase_name=phase_name, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine run_child_by_name


   subroutine run_children(gridcomp, clock, unusable, phase_name, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(ESMF_Clock), intent(inout) :: clock
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), intent(in) :: phase_name
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta

      outer_meta => get_outer_meta_from_inner_gc(gridcomp, _RC)
      call outer_meta%run_children(clock, phase_name=phase_name, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine run_children


   ! Helper functions to access intenal/private state.
   type(ESMF_GridComp) function get_outer_gridcomp(gridcomp, rc) result(outer_gc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status
      type(InnerMetaComponent), pointer :: inner_meta

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
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta

      outer_meta => get_outer_meta_from_inner_gc(gridcomp, _RC)

      call outer_meta%set_entry_point(method_flag, userProcedure, phase_name=phase_name, _RC)

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine gridcomp_set_entry_point


   subroutine add_import_spec(gridcomp, unusable, short_name, standard_name, units, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), intent(in) :: short_name
      character(len=*), intent(in) :: standard_name
      character(len=*), optional, intent(in) :: units
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta
      type(ComponentSpec), pointer :: component_spec

      outer_meta => get_outer_meta_from_inner_gc(gridcomp, _RC)
      component_spec => outer_meta%get_component_spec()
      call component_spec%var_specs%push_back(VariableSpec(ESMF_STATEINTENT_IMPORT, short_name=short_name, standard_name=standard_name))

      _RETURN(ESMF_SUCCESS)
   end subroutine add_import_spec

!!$   subroutine add_import_field_spec(gridcomp, short_name, standard_name, typekind, grid, unusable, extra_dims, rc)
!!$      type(ESMF_GridComp), intent(inout) :: gridcomp
!!$      character(len=*), intent(in) :: short_name
!!$      class(AbstractStateItemSpec), intent(in) :: spec
!!$      class(KeywordEnforcer), optional, intent(in) :: unusable
!!$      type(ExtraDimsSpec), intent(in) :: extra_dims
!!$      integer, optional, intent(out) :: rc
!!$
!!$      integer :: status
!!$      type(OuterMetaComponent), pointer :: outer_meta
!!$
!!$      field_dictionary => get_field_dictionary()
!!$      _ASSERT(field_dictionary%count(standard_name) == 1, 'No such standard name: '//standard_name)
!!$      units = field_dictionary%get_units(standard_name)
!!$      long_name = field_dictionary%get_long_name(standard_name)
!!$      
!!$      call MAPL_add_import_spec(gridcomp, &
!!$           FieldSpec(extra_dims, typekind, grid, units, long_name), &
!!$           _RC)
!!$      
!!$      _RETURN(ESMF_SUCCESS)
!!$   end subroutine add_import_field_spec

   subroutine add_export_spec(gridcomp, unusable, short_name, standard_name, units, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), intent(in) :: short_name
      character(len=*), intent(in) :: standard_name
      character(len=*), optional, intent(in) :: units
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta
      type(ComponentSpec), pointer :: component_spec

      outer_meta => get_outer_meta_from_inner_gc(gridcomp, _RC)
      component_spec => outer_meta%get_component_spec()
      call component_spec%var_specs%push_back(VariableSpec(ESMF_STATEINTENT_EXPORT, short_name=short_name, standard_name=standard_name))

      _RETURN(ESMF_SUCCESS)
   end subroutine add_export_spec

   subroutine add_internal_spec(gridcomp, unusable, short_name, standard_name, units, rc)
      use mapl3g_VirtualConnectionPt, only: ESMF_STATEINTENT_INTERNAL
      type(ESMF_GridComp), intent(inout) :: gridcomp
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), intent(in) :: short_name
      character(len=*), intent(in) :: standard_name
      character(len=*), optional, intent(in) :: units
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta
      type(ComponentSpec), pointer :: component_spec

      outer_meta => get_outer_meta_from_inner_gc(gridcomp, _RC)
      component_spec => outer_meta%get_component_spec()
      call component_spec%var_specs%push_back(VariableSpec(ESMF_STATEINTENT_INTERNAL, short_name=short_name, standard_name=standard_name))

      _RETURN(ESMF_SUCCESS)
   end subroutine add_internal_spec



   subroutine MAPL_SetGeomBase(gridcomp, geom_base, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(ESMF_GeomBase), intent(in) :: geom_base
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta

      outer_meta => get_outer_meta(gridcomp, _RC)
      call outer_meta%set_geom_base(geom_base)

      _RETURN(_SUCCESS)
   end subroutine MAPL_SetGeomBase

   subroutine MAPL_SetGeomGrid(gridcomp, grid, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(ESMF_Grid), intent(in) :: grid
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta
      type(ESMF_GeomBase) :: geom_base

      outer_meta => get_outer_meta(gridcomp, _RC)

      geom_base = ESMF_GeomBaseCreate(grid, ESMF_STAGGERLOC_INVALID, _RC)
      call outer_meta%set_geom_base(geom_base)

      _RETURN(_SUCCESS)
   end subroutine MAPL_SetGeomGrid

   subroutine MAPL_SetGeomMesh(gridcomp, mesh, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(ESMF_Mesh), intent(in) :: mesh
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta
      type(ESMF_GeomBase) :: geom_base

      outer_meta => get_outer_meta(gridcomp, _RC)

      geom_base = ESMF_GeomBaseCreate(mesh, _RC)
      call outer_meta%set_geom_base(geom_base)

      _RETURN(_SUCCESS)
   end subroutine MAPL_SetGeomMesh

   subroutine MAPL_SetGeomXGrid(gridcomp, xgrid, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(ESMF_XGrid), intent(in) :: xgrid
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta
      type(ESMF_GeomBase) :: geom_base

      outer_meta => get_outer_meta(gridcomp, _RC)

      geom_base = ESMF_GeomBaseCreate(xgrid, _RC)
      call outer_meta%set_geom_base(geom_base)

      _RETURN(_SUCCESS)
   end subroutine MAPL_SetGeomXGrid

   subroutine MAPL_SetGeomLocStream(gridcomp, locstream, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(ESMF_LocStream), intent(in) :: locstream
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta
      type(ESMF_GeomBase) :: geom_base

      outer_meta => get_outer_meta(gridcomp, _RC)

      geom_base = ESMF_GeomBaseCreate(locstream, _RC)
      call outer_meta%set_geom_base(geom_base)

      _RETURN(_SUCCESS)
   end subroutine MAPL_SetGeomLocStream

end module mapl3g_Generic
