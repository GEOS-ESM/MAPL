#include "MAPL_Generic.h"

!---------------------------------------------------------------------
!
! This module contains procedures that are intended to be called from
! within user-level gridded components.  These are primarily thin
! wrappers that access the internal private state of the gridcomp and
! then invoke methods on that type.

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
   use :: mapl3g_ChildSpec, only: ChildSpec
   use :: mapl3g_ComponentSpec, only: ComponentSpec
   use :: mapl3g_VariableSpec, only: VariableSpec, make_VariableSpec
   use :: mapl3g_GriddedComponentDriver, only: GriddedComponentDriver
   use :: mapl3g_UngriddedDims, only: UngriddedDims
   use :: mapl3g_Validation, only: is_valid_name
   use :: mapl3g_ESMF_Interfaces, only: I_Run
   use :: mapl3g_StateItemSpec
   use :: mapl3g_VerticalGrid
   use mapl3g_StateRegistry, only: StateRegistry
   use mapl_InternalConstantsMod
   use :: esmf, only: ESMF_Info
   use :: esmf, only: ESMF_InfoGetFromHost
   use :: esmf, only: ESMF_InfoGet
   use :: esmf, only: ESMF_InfoIsSet
   use :: esmf, only: ESMF_GridComp
   use :: esmf, only: ESMF_GridCompGet
   use :: esmf, only: ESMF_Geom, ESMF_GeomCreate
   use :: esmf, only: ESMF_Grid, ESMF_Mesh, ESMF_Xgrid, ESMF_LocStream
   use :: esmf, only: ESMF_STAGGERLOC_INVALID
   use :: esmf, only: ESMF_Clock
   use :: esmf, only: ESMF_Config
   use :: esmf, only: ESMF_ConfigGet
   use :: esmf, only: ESMF_HConfig
   use :: esmf, only: ESMF_HConfigIsDefined
   use :: esmf, only: ESMF_SUCCESS
   use :: esmf, only: ESMF_Method_Flag
   use :: esmf, only: ESMF_STAGGERLOC_INVALID
   use :: esmf, only: ESMF_StateIntent_Flag
   use :: esmf, only: ESMF_STATEINTENT_IMPORT, ESMF_STATEINTENT_EXPORT, ESMF_STATEINTENT_INTERNAL
   use :: esmf, only: ESMF_TypeKind_Flag, ESMF_TYPEKIND_R4
   use :: esmf, only: ESMF_KIND_I4, ESMF_KIND_I8, ESMF_KIND_R4, ESMF_KIND_R8
   use :: esmf, only: ESMF_StateItem_Flag, ESMF_STATEITEM_FIELD, ESMF_STATEITEM_FIELDBUNDLE
   use :: esmf, only: ESMF_STATEITEM_STATE, ESMF_STATEITEM_UNKNOWN
   use :: esmf, only: ESMF_KIND_R8, ESMF_KIND_R4, ESMF_NOKIND
   use :: esmf, only: ESMF_TYPEKIND_R8, ESMF_TYPEKIND_R4, ESMF_NOKIND
   use :: esmf, only: ESMF_Time, ESMF_TimeInterval
   use mapl3g_hconfig_get
   use :: pflogger, only: logger_t => logger
   use mapl_ErrorHandling
   use mapl_KeywordEnforcer
   implicit none
   private

   public :: MAPL_GridCompGetOuterMeta
   public :: MAPL_GridCompIsGeneric
   public :: MAPL_GridCompIsUser

   public :: get_outer_meta_from_inner_gc

   public :: MAPL_GridCompGet
   public :: MAPL_GridCompSet
   public :: MAPL_GridCompSetEntryPoint

   public :: MAPL_GridCompAddChild
   public :: MAPL_RunChild
   public :: MAPL_RunChildren

!!$   public :: MAPL_GetInternalState

   public :: MAPL_AddSpec
   public :: MAPL_AddImportSpec
   public :: MAPL_AddExportSpec
   public :: MAPL_AddInternalSpec
   public :: MAPL_SetGeometry
!!$
    public :: MAPL_ResourceGet

   ! Accessors
!!$   public :: MAPL_GetOrbit
!!$   public :: MAPL_GetCoordinates
!!$   public :: MAPL_GetLayout

   public :: MAPL_GridCompSetGeom
   public :: MAPL_GridCompSetVerticalGrid

   ! Connections
!#   public :: MAPL_AddConnection
   public :: MAPL_ConnectAll


   ! Interfaces

   interface MAPL_GridCompGetOuterMeta
      procedure :: gridcomp_get_outer_meta
   end interface MAPL_GridCompGetOuterMeta

   interface MAPL_GridCompSetGeom
      procedure MAPL_GridCompSetGeom
      procedure MAPL_GridCompSetGeomGrid
      procedure MAPL_GridCompSetGeomMesh
      procedure MAPL_GridCompSetGeomXgrid
      procedure MAPL_GridCompSetGeomLocStream
   end interface MAPL_GridCompSetGeom

   interface MAPL_GridCompGet
      procedure :: gridcomp_get
   end interface MAPL_GridCompGet

   interface MAPL_GridCompSet
      procedure :: gridcomp_set
   end interface MAPL_GridCompSet

!!$   interface MAPL_GetInternalState
!!$      procedure :: get_internal_state
!!$   end interface MAPL_GetInternalState



   interface MAPL_GridCompAddChild
      procedure :: add_child_config
      procedure :: add_child_by_spec
   end interface MAPL_GridCompAddChild

   interface MAPL_RunChild
      procedure :: run_child_by_name
   end interface MAPL_RunChild

   interface MAPL_RunChildren
      procedure :: run_children
   end interface MAPL_RunChildren

   interface MAPL_AddSpec
      procedure :: add_spec_basic
      procedure :: add_spec_explicit
   end interface MAPL_AddSpec

   interface MAPL_AddImportSpec
      procedure :: add_import_spec_legacy
   end interface MAPL_AddImportSpec

   interface MAPL_AddExportSpec
      procedure :: add_export_spec
   end interface MAPL_AddExportSpec

   interface MAPL_AddInternalSpec
      procedure :: add_internal_spec
   end interface MAPL_AddInternalSpec

   interface MAPL_SetGeometry
      procedure :: set_geometry
   end interface MAPL_SetGeometry

   interface MAPL_GridCompSetEntryPoint
      procedure gridcomp_set_entry_point
   end interface MAPL_GridCompSetEntryPoint

   interface MAPL_ConnectAll
      procedure :: gridcomp_connect_all
   end interface MAPL_ConnectAll

   interface MAPL_ResourceGet
      procedure :: resource_get_i4_gc
      procedure :: resource_get_i8_gc
      procedure :: resource_get_r4_gc
      procedure :: resource_get_r8_gc
      procedure :: resource_get_logical_gc
      procedure :: resource_get_i4seq_gc
      procedure :: resource_get_i8seq_gc
      procedure :: resource_get_r4seq_gc
      procedure :: resource_get_r8seq_gc
      procedure :: resource_get_logical_seq_gc
      procedure :: resource_get_string_gc
   end interface MAPL_ResourceGet

   interface MAPL_GridCompIsGeneric
      procedure :: gridcomp_is_generic
   end interface MAPL_GridCompIsGeneric

   interface MAPL_GridCompIsUser
      procedure :: gridcomp_is_user
   end interface MAPL_GridCompIsUser


contains

   recursive subroutine gridcomp_get_outer_meta(gridcomp, outer_meta, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(OuterMetaComponent), pointer, intent(out) :: outer_meta
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: is_generic
      type(ESMF_GridComp) :: outer_gc

      is_generic = MAPL_GridCompIsGeneric(gridcomp, _RC)

      if (is_generic) then
         outer_meta => get_outer_meta(gridcomp, _RC)
         _RETURN(_SUCCESS)
      end if

      ! is user gridcomp
      outer_gc = get_outer_gridcomp(gridcomp, _RC)
      call MAPL_GridCompGetOuterMeta(outer_gc, outer_meta, _RC)

      _RETURN(_SUCCESS)
   end subroutine

   subroutine gridcomp_get(gridcomp, unusable, &
        hconfig, &
        outer_meta, &
        logger, &
        registry, &
        geom, &
        vertical_grid, &
        rc)

      type(ESMF_GridComp), intent(inout) :: gridcomp
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_Hconfig), optional, intent(out) :: hconfig
      type(OuterMetaComponent), pointer, optional, intent(out) :: outer_meta
      class(Logger_t), optional, pointer, intent(out) :: logger
      type(StateRegistry), optional, pointer, intent(out) :: registry
      type(ESMF_Geom), optional, intent(out) :: geom
      class(VerticalGrid), allocatable, optional, intent(out) :: vertical_grid
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta_, outer_meta_from_inner_gc

      call MAPL_GridCompGetOuterMeta(gridcomp, outer_meta_, _RC)
      
      if (present(hconfig)) hconfig = outer_meta_%get_hconfig()
      if (present(outer_meta)) outer_meta => outer_meta_
      if (present(logger)) logger => outer_meta_%get_lgr()
      if (present(registry)) registry => outer_meta_%get_registry()
      if (present(geom)) geom = outer_meta_%get_geom()
      if (present(vertical_grid)) then
         outer_meta_from_inner_gc => get_outer_meta_from_inner_gc(gridcomp, _RC)
         vertical_grid = outer_meta_from_inner_gc%get_vertical_grid()
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine gridcomp_get

   subroutine gridcomp_set(gridcomp, unusable, activate_all_exports, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(in) :: activate_all_exports
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta

      call MAPL_GridCompGetOuterMeta(gridcomp, outer_meta, _RC)
      call outer_meta%set(activate_all_exports=activate_all_exports)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine gridcomp_set

   subroutine add_child_config(gridcomp, child_name, setservices, hconfig, unusable, timeStep, refTime_offset, rc)
      use mapl3g_UserSetServices
      type(ESMF_GridComp), intent(inout) :: gridcomp
      character(len=*), intent(in) :: child_name
      class(AbstractUserSetServices), intent(in) :: setservices
      type(ESMF_HConfig), intent(in) :: hconfig
      class(KeywordEnforcer), optional, intent(out) :: unusable
      type(ESMF_TimeInterval), optional, intent(in) :: timeStep
      type(ESMF_TimeInterval), optional, intent(in) :: refTime_offset
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta
      type(ChildSpec) :: child_spec

      _ASSERT(is_valid_name(child_name), 'Child name <' // child_name //'> does not conform to GEOS standards.')

      child_spec = ChildSpec(setServices, hconfig=hconfig, timeStep=timeStep, refTime_offset=refTime_offset)
      call add_child_by_spec(gridcomp, child_name, child_spec, _RC)

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine add_child_config

   subroutine add_child_by_spec(gridcomp, child_name, child_spec, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      character(len=*), intent(in) :: child_name
      type(ChildSpec), intent(in) :: child_spec
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta

      call MAPL_GridCompGetOuterMeta(gridcomp, outer_meta, _RC)
      call outer_meta%add_child(child_name, child_spec, _RC)

      _RETURN(ESMF_SUCCESS)
   end subroutine add_child_by_spec


   ! In this procedure, gridcomp is actually an _outer_ gridcomp.   The intent is that
   ! an inner gridcomp will call this on its child which is a wrapped user comp.
   recursive subroutine run_child_by_name(gridcomp, child_name, unusable, phase_name, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      character(len=*), intent(in) :: child_name
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), optional, intent(in) :: phase_name
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta

      call MAPL_GridCompGetOuterMeta(gridcomp, outer_meta, _RC)
      call outer_meta%run_child(child_name, phase_name=phase_name, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine run_child_by_name


   recursive subroutine run_children(gridcomp, unusable, phase_name, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), intent(in) :: phase_name
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta

      call MAPL_GridCompGetOuterMeta(gridcomp, outer_meta, _RC)
      call outer_meta%run_children(phase_name=phase_name, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine run_children


   ! Helper functions to access intenal/private state.
   type(ESMF_GridComp) function get_outer_gridcomp(gridcomp, rc) result(outer_gc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status
      type(InnerMetaComponent), pointer :: inner_meta
      logical :: is_user_gridcomp

      is_user_gridcomp =  MAPL_GridCompIsUser(gridcomp, _RC)
      _ASSERT(is_user_gridcomp, 'gridcomp argument must be a user gridcomp')
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

      call MAPL_GridCompGetOuterMeta(gridcomp, outer_meta, _RC)
      call outer_meta%set_entry_point(method_flag, userProcedure, phase_name=phase_name, _RC)

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine gridcomp_set_entry_point


   subroutine add_spec_basic(gridcomp, variable_spec, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(VariableSpec), intent(in) :: variable_spec
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta
      type(ComponentSpec), pointer :: component_spec

      call MAPL_GridCompGetOuterMeta(gridcomp, outer_meta, _RC)
      component_spec => outer_meta%get_component_spec()
      call component_spec%var_specs%push_back(variable_spec)

      _RETURN(_SUCCESS)
   end subroutine add_spec_basic

   subroutine add_spec_explicit(gridcomp, state_intent, unusable, short_name, standard_name, typekind, ungridded_dims, units, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(ESMF_Stateintent_Flag), intent(in) :: state_intent
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(*), intent(in) :: short_name
      character(*), intent(in) :: standard_name
      type(ESMF_TypeKind_Flag), optional, intent(in) :: typekind
      type(UngriddedDims), intent(in) :: ungridded_dims
      character(*), optional, intent(in) :: units
      integer, optional, intent(out) :: rc

      integer :: status
      type(VariableSpec) :: var_spec

      var_spec = make_VariableSpec( &
           state_intent=state_intent, &
           short_name=short_name, &
           standard_name=standard_name, &
           typekind=typekind, &
           ungridded_dims=ungridded_dims, &
           units=units)
      call MAPL_AddSpec(gridcomp, var_spec, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine add_spec_explicit


   subroutine add_import_spec_legacy(gc, short_name, long_name,               &
        units,  dims, vlocation,                 &
        datatype,num_subtiles, refresh_interval, &
        averaging_interval, halowidth, precision, default,  &
        restart, ungridded_dims, field_type,     &
        staggering, rotation, rc)
      type (ESMF_GridComp)            , intent(inout)   :: gc
      character (len=*)               , intent(in)      :: short_name
      character (len=*)  , optional   , intent(in)      :: long_name
      character (len=*)  , optional   , intent(in)      :: units
      integer            , optional   , intent(in)      :: dims
      integer            , optional   , intent(in)      :: datatype
      integer            , optional   , intent(in)      :: num_subtiles
      integer            , optional   , intent(in)      :: vlocation
      integer            , optional   , intent(in)      :: refresh_interval
      integer            , optional   , intent(in)      :: averaging_interval
      integer            , optional   , intent(in)      :: halowidth
      integer            , optional   , intent(in)      :: precision
      real               , optional   , intent(in)      :: default
      integer            , optional   , intent(in)      :: restart
      integer            , optional   , intent(in)      :: ungridded_dims(:)
      integer            , optional   , intent(in)      :: field_type
      integer            , optional   , intent(in)      :: staggering
      integer            , optional   , intent(in)      :: rotation
      integer            , optional   , intent(out)     :: rc

      integer :: status
      type(VariableSpec) :: var_spec
      type(ESMF_TypeKind_Flag), allocatable :: typekind

      ! Leave unallocated if precision is not PRESENT.  Default (R4)
      ! is actually set inside VariableSpec constructor.
      if (present(precision)) then
         typekind = to_typekind(precision)
      end if

      var_spec = make_VariableSpec( &
           state_intent=ESMF_STATEINTENT_IMPORT, &
           short_name=short_name, &
           typekind=typekind, &
           itemtype=to_itemtype(datatype), &
           units=units &
!#           ungridded_dims=to_ungridded_dims(dims, vlocation, ungridded_dims, ungridded_coords), &
           )

      call MAPL_AddSpec(gc, var_spec, _RC)

      _RETURN(ESMF_SUCCESS)
   end subroutine add_import_spec_legacy

   function to_typekind(precision) result(tk)
      type(ESMF_TypeKind_Flag) :: tk
      integer, optional, intent(in) :: precision

      tk = ESMF_TYPEKIND_R4 ! GEOS default
      if (.not. present(precision)) return

      select case (precision)
      case (ESMF_KIND_R4)
         tk = ESMF_TYPEKIND_R4
      case (ESMF_KIND_R8)
         tk = ESMF_TYPEKIND_R8
      case default
         tk = ESMF_NOKIND
      end select

   end function to_typekind

   function to_ungridded_dims(dims, vlocation, legacy_ungridded_dims, ungridded_coords) result(ungridded_dims)
      type(UngriddedDims) :: ungridded_dims
      integer, optional, intent(in) :: dims
      integer, optional, intent(in) :: vlocation
      integer, optional, intent(in) :: legacy_ungridded_dims(:)
      real, optional, intent(in) :: ungridded_coords(:)
      character(len=11) :: dim_name

      if (any(dims == [MAPL_DimsVertOnly, MAPL_DimsHorzVert])) then
!!$         call extra_dims%add_dim_spec(UngriddedDim('lev', ...))
!!$         call ungridded_dims%add_dim_spec(DefferredDimSpec('lev', ...))
      end if

!!$      do i = 1, size(legacy_ungridded_dims)
!!$         write(dim_name,'("ungridded_", i1)') i
!!$         call ungridded_dims%add_dim_spec(dim_name, 'unknown', ungridded_dims(i))
!!$      end do

   end function to_ungridded_dims

   function to_itemtype(datatype) result(itemtype)
      type(ESMF_StateItem_Flag) :: itemtype
      integer, optional, intent(in) :: datatype

      itemtype = ESMF_STATEITEM_FIELD ! GEOS default
      if (.not. present(datatype)) return

      select case (datatype)
      case (MAPL_FieldItem)
         itemtype = ESMF_STATEITEM_FIELD
      case (MAPL_BundleItem)
         itemtype = ESMF_STATEITEM_FIELDBUNDLE
      case (MAPL_StateItem)
         itemtype = ESMF_STATEITEM_STATE
      case default
         itemtype = ESMF_STATEITEM_UNKNOWN
      end select
   end function to_itemtype


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
      type(VariableSpec) :: var_spec

      call MAPL_GridCompGetOuterMeta(gridcomp, outer_meta, _RC)
      component_spec => outer_meta%get_component_spec()
      var_spec = make_VariableSpec(ESMF_STATEINTENT_EXPORT, short_name=short_name, &
           standard_name=standard_name, _RC)
      call component_spec%var_specs%push_back(var_spec)

      _RETURN(ESMF_SUCCESS)
   end subroutine add_export_spec

   subroutine add_internal_spec(gridcomp, unusable, short_name, standard_name, units, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), intent(in) :: short_name
      character(len=*), intent(in) :: standard_name
      character(len=*), optional, intent(in) :: units
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta
      type(ComponentSpec), pointer :: component_spec
      type(VariableSpec) :: var_spec

      call MAPL_GridCompGetOuterMeta(gridcomp, outer_meta, _RC)
      component_spec => outer_meta%get_component_spec()
      var_spec = make_VariableSpec( &
           ESMF_STATEINTENT_INTERNAL, &
           short_name=short_name, &
           standard_name=standard_name, &
           units=units, _RC)
      call component_spec%var_specs%push_back(var_spec)

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine add_internal_spec

   subroutine MAPL_GridCompSetVerticalGrid(gridcomp, vertical_grid, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      class(VerticalGrid), intent(in) :: vertical_grid
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta

      call MAPL_GridCompGetOuterMeta(gridcomp, outer_meta, _RC)
      call outer_meta%set_vertical_grid(vertical_grid)

      _RETURN(_SUCCESS)
   end subroutine MAPL_GridCompSetVerticalGrid

   subroutine MAPL_GridCompSetGeom(gridcomp, geom, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(ESMF_Geom), intent(in) :: geom
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta

      call MAPL_GridCompGetOuterMeta(gridcomp, outer_meta, _RC)
      call outer_meta%set_geom(geom)

      _RETURN(_SUCCESS)
   end subroutine MAPL_GridCompSetGeom

   subroutine MAPL_GridCompSetGeomGrid(gridcomp, grid, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(ESMF_Grid), intent(in) :: grid
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Geom) :: geom

      geom = ESMF_GeomCreate(grid, ESMF_STAGGERLOC_INVALID, _RC)
      call MAPL_GridCompSetGeom(gridcomp, geom, _RC)


      _RETURN(_SUCCESS)
   end subroutine MAPL_GridCompSetGeomGrid

   subroutine MAPL_GridCompSetGeomMesh(gridcomp, mesh, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(ESMF_Mesh), intent(in) :: mesh
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Geom) :: geom

      geom = ESMF_GeomCreate(mesh, _RC)
      call MAPL_GridCompSetGeom(gridcomp, geom, _RC)

      _RETURN(_SUCCESS)
   end subroutine MAPL_GridCompSetGeomMesh

   subroutine MAPL_GridCompSetGeomXGrid(gridcomp, xgrid, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(ESMF_XGrid), intent(in) :: xgrid
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Geom) :: geom

      geom = ESMF_GeomCreate(xgrid, _RC)
      call MAPL_GridCompSetGeom(gridcomp, geom, _RC)

      _RETURN(_SUCCESS)
   end subroutine MAPL_GridCompSetGeomXGrid

   subroutine MAPL_GridCompSetGeomLocStream(gridcomp, locstream, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(ESMF_LocStream), intent(in) :: locstream
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Geom) :: geom

      geom = ESMF_GeomCreate(locstream, _RC)
      call MAPL_GridCompSetGeom(gridcomp, geom, _RC)


      _RETURN(_SUCCESS)
   end subroutine MAPL_GridCompSetGeomLocStream

   subroutine gridcomp_connect_all(gridcomp, src_comp, dst_comp, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      character(*), intent(in) :: src_comp
      character(*), intent(in) :: dst_comp
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta

      call MAPL_GridCompGetOuterMeta(gridcomp, outer_meta, _RC)
      call outer_meta%connect_all(src_comp, dst_comp, _RC)

      _RETURN(_SUCCESS)
   end subroutine gridcomp_connect_all

   subroutine gridcomp_get_hconfig(gridcomp, hconfig, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(ESMF_HConfig), intent(out) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Config) :: config
      
      call ESMF_GridCompGet(gridcomp, config=config, _RC)
      call ESMF_ConfigGet(config, hconfig=hconfig, _RC)

      _RETURN(_SUCCESS)
   end subroutine gridcomp_get_hconfig

   subroutine resource_get_i4_gc(gc, keystring, value, unusable, default, value_set, rc)
      integer(kind=ESMF_KIND_I4), intent(inout) :: value
      integer(kind=ESMF_KIND_I4), optional, intent(in) :: default
      type(ESMF_GridComp), intent(inout) :: gc
      character(len=*), intent(in) :: keystring
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: value_set
      integer, optional, intent(out) :: rc
      class(Logger_t), pointer :: logger
      type(ESMF_HConfig) :: hconfig
      type(HConfigParams) :: params
      integer :: status

      call MAPL_GridCompGet(gc, hconfig=hconfig, logger=logger, _RC)
      params = HConfigParams(hconfig, keystring, value_set, logger)
      call MAPL_HConfigGet(params, value, default, _RC) 
      if(present(value_set)) value_set = params%value_set

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine resource_get_i4_gc

   subroutine resource_get_i8_gc(gc, keystring, value, unusable, default, value_set, rc)
      integer(kind=ESMF_KIND_I8), intent(inout) :: value
      integer(kind=ESMF_KIND_I8), optional, intent(in) :: default
      type(ESMF_GridComp), intent(inout) :: gc
      character(len=*), intent(in) :: keystring
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: value_set
      integer, optional, intent(out) :: rc
      class(Logger_t), pointer :: logger
      type(ESMF_HConfig) :: hconfig
      type(HConfigParams) :: params
      integer :: status

      call MAPL_GridCompGet(gc, hconfig=hconfig, logger=logger, _RC)
      params = HConfigParams(hconfig, keystring, value_set, logger)
      call MAPL_HConfigGet(params, value, default, _RC) 
      if(present(value_set)) value_set = params%value_set

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine resource_get_i8_gc
   
   subroutine resource_get_r4_gc(gc, keystring, value, unusable, default, value_set, rc)
      real(kind=ESMF_KIND_R4), intent(inout) :: value
      real(kind=ESMF_KIND_R4), optional, intent(in) :: default
      type(ESMF_GridComp), intent(inout) :: gc
      character(len=*), intent(in) :: keystring
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: value_set
      integer, optional, intent(out) :: rc
      class(Logger_t), pointer :: logger
      type(ESMF_HConfig) :: hconfig
      type(HConfigParams) :: params
      integer :: status

      call MAPL_GridCompGet(gc, hconfig=hconfig, logger=logger, _RC)
      params = HConfigParams(hconfig, keystring, value_set, logger)
      call MAPL_HConfigGet(params, value, default, _RC) 
      if(present(value_set)) value_set = params%value_set

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine resource_get_r4_gc

   subroutine resource_get_r8_gc(gc, keystring, value, unusable, default, value_set, rc)
      real(kind=ESMF_KIND_R8), intent(inout) :: value
      real(kind=ESMF_KIND_R8), optional, intent(in) :: default
      type(ESMF_GridComp), intent(inout) :: gc
      character(len=*), intent(in) :: keystring
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: value_set
      integer, optional, intent(out) :: rc
      class(Logger_t), pointer :: logger
      type(ESMF_HConfig) :: hconfig
      type(HConfigParams) :: params
      integer :: status

      call MAPL_GridCompGet(gc, hconfig=hconfig, logger=logger, _RC)
      params = HConfigParams(hconfig, keystring, value_set, logger)
      call MAPL_HConfigGet(params, value, default, _RC) 
      if(present(value_set)) value_set = params%value_set

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine resource_get_r8_gc

   subroutine resource_get_logical_gc(gc, keystring, value, unusable, default, value_set, rc)
      logical, intent(inout) :: value
      logical, optional, intent(in) :: default
      type(ESMF_GridComp), intent(inout) :: gc
      character(len=*), intent(in) :: keystring
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: value_set
      integer, optional, intent(out) :: rc
      class(Logger_t), pointer :: logger
      type(ESMF_HConfig) :: hconfig
      type(HConfigParams) :: params
      integer :: status

      call MAPL_GridCompGet(gc, hconfig=hconfig, logger=logger, _RC)
      params = HConfigParams(hconfig, keystring, value_set, logger)
      call MAPL_HConfigGet(params, value, default, _RC) 
      if(present(value_set)) value_set = params%value_set

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine resource_get_logical_gc

   subroutine resource_get_string_gc(gc, keystring, value, unusable, default, value_set, rc)
      character(len=:), allocatable, intent(inout) :: value
      character(len=*), optional, intent(in) :: default
      type(ESMF_GridComp), intent(inout) :: gc
      character(len=*), intent(in) :: keystring
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: value_set
      integer, optional, intent(out) :: rc
      class(Logger_t), pointer :: logger
      type(ESMF_HConfig) :: hconfig
      type(HConfigParams) :: params
      integer :: status

      call MAPL_GridCompGet(gc, hconfig=hconfig, logger=logger, _RC)
      params = HConfigParams(hconfig, keystring, value_set, logger=logger)
      call MAPL_HConfigGet(params, value, default, _RC) 
      if(present(value_set)) value_set = params%value_set

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine resource_get_string_gc

   subroutine resource_get_i4seq_gc(gc, keystring, value, unusable, default, value_set, rc)
      integer(kind=ESMF_KIND_I4), dimension(:), allocatable, intent(inout) :: value
      integer(kind=ESMF_KIND_I4), dimension(:), optional, intent(in) :: default
      type(ESMF_GridComp), intent(inout) :: gc
      character(len=*), intent(in) :: keystring
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: value_set
      integer, optional, intent(out) :: rc
      class(Logger_t), pointer :: logger
      type(ESMF_HConfig) :: hconfig
      type(HConfigParams) :: params
      integer :: status

      call MAPL_GridCompGet(gc, hconfig=hconfig, logger=logger, _RC)
      params = HConfigParams(hconfig, keystring, value_set, logger)
      call MAPL_HConfigGet(params, value, default, _RC) 
      if(present(value_set)) value_set = params%value_set

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine resource_get_i4seq_gc

   subroutine resource_get_i8seq_gc(gc, keystring, value, unusable, default, value_set, rc)
      integer(kind=ESMF_KIND_I8), dimension(:), allocatable, intent(inout) :: value
      integer(kind=ESMF_KIND_I8), dimension(:), optional, intent(in) :: default
      type(ESMF_GridComp), intent(inout) :: gc
      character(len=*), intent(in) :: keystring
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: value_set
      integer, optional, intent(out) :: rc
      class(Logger_t), pointer :: logger
      type(ESMF_HConfig) :: hconfig
      type(HConfigParams) :: params
      integer :: status

      call MAPL_GridCompGet(gc, hconfig=hconfig, logger=logger, _RC)
      params = HConfigParams(hconfig, keystring, value_set, logger)
      call MAPL_HConfigGet(params, value, default, _RC) 
      if(present(value_set)) value_set = params%value_set

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine resource_get_i8seq_gc

   subroutine resource_get_r4seq_gc(gc, keystring, value, unusable, default, value_set, rc)
      real(kind=ESMF_KIND_R4), dimension(:), allocatable, intent(inout) :: value
      real(kind=ESMF_KIND_R4), dimension(:), optional, intent(in) :: default
      type(ESMF_GridComp), intent(inout) :: gc
      character(len=*), intent(in) :: keystring
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: value_set
      integer, optional, intent(out) :: rc
      class(Logger_t), pointer :: logger
      type(ESMF_HConfig) :: hconfig
      type(HConfigParams) :: params
      integer :: status

      call MAPL_GridCompGet(gc, hconfig=hconfig, logger=logger, _RC)
      params = HConfigParams(hconfig, keystring, value_set, logger)
      call MAPL_HConfigGet(params, value, default, _RC) 
      if(present(value_set)) value_set = params%value_set

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine resource_get_r4seq_gc

   subroutine resource_get_r8seq_gc(gc, keystring, value, unusable, default, value_set, rc)
      real(kind=ESMF_KIND_R8), dimension(:), allocatable, intent(inout) :: value
      real(kind=ESMF_KIND_R8), dimension(:), optional, intent(in) :: default
      type(ESMF_GridComp), intent(inout) :: gc
      character(len=*), intent(in) :: keystring
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: value_set
      integer, optional, intent(out) :: rc
      class(Logger_t), pointer :: logger
      type(ESMF_HConfig) :: hconfig
      type(HConfigParams) :: params
      integer :: status

      call MAPL_GridCompGet(gc, hconfig=hconfig, logger=logger, _RC)
      params = HConfigParams(hconfig, keystring, value_set, logger)
      call MAPL_HConfigGet(params, value, default, _RC) 
      if(present(value_set)) value_set = params%value_set

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine resource_get_r8seq_gc

   subroutine resource_get_logical_seq_gc(gc, keystring, value, unusable, default, value_set, rc)
      logical, dimension(:), allocatable, intent(inout) :: value
      logical, dimension(:), optional, intent(in) :: default
      type(ESMF_GridComp), intent(inout) :: gc
      character(len=*), intent(in) :: keystring
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: value_set
      integer, optional, intent(out) :: rc
      class(Logger_t), pointer :: logger
      type(ESMF_HConfig) :: hconfig
      type(HConfigParams) :: params
      integer :: status

      call MAPL_GridCompGet(gc, hconfig=hconfig, logger=logger, _RC)
      params = HConfigParams(hconfig, keystring, value_set, logger)
      call MAPL_HConfigGet(params, value, default, _RC) 
      if(present(value_set)) value_set = params%value_set

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine resource_get_logical_seq_gc

   logical function gridcomp_is_generic(gridcomp, rc)
      type(ESMF_GridComp), intent(in) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: info
      logical :: found

      gridcomp_is_generic = .false.
      call ESMF_InfoGetFromHost(gridcomp, info, _RC)
      found = ESMF_InfoIsSet(info, key='MAPL/GRIDCOMP_IS_GENERIC', _RC)
      if (found) then
         call ESMF_InfoGet(info, key='MAPL/GRIDCOMP_IS_GENERIC', value=gridcomp_is_generic, _RC)
      end if

      _RETURN(_SUCCESS)
   end function gridcomp_is_generic

   logical function gridcomp_is_user(gridcomp, rc)
      type(ESMF_GridComp), intent(in) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: info

      gridcomp_is_user = .not. MAPL_GridCompIsGeneric(gridcomp, _RC)

      _RETURN(_SUCCESS)
   end function gridcomp_is_user

   subroutine set_geometry(gridcomp, state_intent, short_name, geom, vertical_grid, rc)
      use mapl3g_VirtualConnectionPt
      use mapl3g_ExtensionFamily
      use mapl3g_StateItemExtension
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(Esmf_StateIntent_Flag), intent(in) :: state_intent
      character(*), intent(in) :: short_name
      type(ESMF_Geom), optional, intent(in) :: geom
      class(VerticalGrid), optional, intent(in) :: vertical_grid
      integer, optional, intent(out) :: rc

      integer :: status
      type(StateRegistry), pointer :: registry
      type(VirtualConnectionPt) :: v_pt
      type(ExtensionFamily), pointer :: family
      type(StateItemExtension), pointer :: primary
      class(StateItemSpec), pointer :: spec
      
      call MAPL_GridCompGet(gridcomp, registry=registry, _RC)
      v_pt = VirtualConnectionPt(state_intent, short_name)

      family => registry%get_extension_family(v_pt, _RC)
      _ASSERT(family%has_primary(), 'Should not set geometry on vars from other components.')
      _ASSERT(family%num_variants() == 1, 'No extensions should happen prior to this call.')

      primary => family%get_primary(_RC)
      _ASSERT(associated(primary), 'null pointer for primary')
      spec => primary%get_spec()
      _ASSERT(associated(spec), 'null pointer for spec')

      call spec%set_geometry(geom=geom, vertical_grid=vertical_grid, _RC)

      _RETURN(_SUCCESS)
   end subroutine set_geometry

end module mapl3g_Generic
