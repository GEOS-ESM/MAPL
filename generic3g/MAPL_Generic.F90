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

   use mapl3g_InnerMetaComponent, only: InnerMetaComponent
   use mapl3g_InnerMetaComponent, only: get_inner_meta
   use mapl3g_OuterMetaComponent, only: OuterMetaComponent
   use mapl3g_OuterMetaComponent, only: get_outer_meta
   use mapl3g_ChildSpec, only: ChildSpec
   use mapl3g_ComponentSpec, only: ComponentSpec
   use mapl3g_VariableSpec, only: VariableSpec, make_VariableSpec
   use mapl3g_Validation, only: is_valid_name
   use mapl3g_ESMF_Interfaces, only: I_Run
   use mapl3g_StateItemSpec
   use mapl3g_VerticalGrid
   use mapl3g_VerticalStaggerLoc, only: VerticalStaggerLoc
   use mapl3g_StateRegistry, only: StateRegistry
   use mapl_InternalConstantsMod
   use mapl3g_HorizontalDimsSpec, only: HorizontalDimsSpec, HORIZONTAL_DIMS_NONE, HORIZONTAL_DIMS_GEOM
   use mapl3g_UngriddedDims, only: UngriddedDims
   use mapl3g_StateItem, only: MAPL_STATEITEM_STATE, MAPL_STATEITEM_FIELDBUNDLE
   use mapl3g_ESMF_Utilities, only: esmf_state_intent_to_string
   use esmf, only: ESMF_Info
   use esmf, only: ESMF_InfoGetFromHost
   use esmf, only: ESMF_InfoGet
   use esmf, only: ESMF_InfoIsSet
   use esmf, only: ESMF_GridComp
   use esmf, only: ESMF_Geom, ESMF_GeomCreate, ESMF_GeomGet
   use esmf, only: ESMF_Grid, ESMF_Mesh, ESMF_Xgrid, ESMF_LocStream
   use esmf, only: ESMF_STAGGERLOC_INVALID
   use esmf, only: ESMF_HConfig
   use esmf, only: ESMF_Method_Flag
   use esmf, only: ESMF_StateIntent_Flag, ESMF_STATEINTENT_INTERNAL
   use esmf, only: ESMF_KIND_I4, ESMF_KIND_I8, ESMF_KIND_R4, ESMF_KIND_R8
   use esmf, only: ESMF_KIND_R8, ESMF_KIND_R4
   use esmf, only: ESMF_Time, ESMF_TimeInterval, ESMF_TimeIntervalGet, ESMF_Clock
   use esmf, only: MAPL_ClockGet => ESMF_ClockGet
   use esmf, only: ESMF_State, ESMF_StateItem_Flag, ESMF_STATEITEM_FIELD
   use esmf, only: operator(==)
   use mapl3g_hconfig_get
   use mapl3g_RestartHandler
   use pflogger, only: logger_t => logger
   use mapl_ErrorHandling
   use mapl_KeywordEnforcer

   implicit none
   private

   ! These should not be needed by users
   public :: MAPL_GridCompGetOuterMeta
   public :: MAPL_GridCompGetRegistry


   ! These should be available to users
   public :: MAPL_GridCompAddVarSpec
   public :: MAPL_GridCompAddSpec
   public :: MAPL_GridCompIsGeneric
   public :: MAPL_GridCompIsUser

   public :: MAPL_GridCompGet
   public :: MAPL_GridCompSet
   public :: MAPL_GridCompSetEntryPoint

   public :: MAPL_GridCompAddChild
   public :: MAPL_GridCompRunChild
   public :: MAPL_GridCompRunChildren

   public :: MAPL_GridCompGetInternalState

   public :: MAPL_GridCompSetGeometry

   public :: MAPL_GridcompGetResource

   public :: MAPL_ClockGet

   ! Accessors
!!$   public :: MAPL_GetOrbit
!!$   public :: MAPL_GetCoordinates
!!$   public :: MAPL_GetLayout

   public :: MAPL_GridCompSetGeom
   public :: MAPL_GridCompSetVerticalGrid

   ! Connections
   public :: MAPL_GridCompAddConnectivity
   public :: MAPL_GridCompReexport
   public :: MAPL_GridCompConnectAll

   ! Spec types
   public :: MAPL_STATEITEM_STATE, MAPL_STATEITEM_FIELDBUNDLE

   ! Restart types
   public :: MAPL_RESTART, MAPL_RESTART_SKIP

   ! Interfaces

   interface MAPL_GridCompGetOuterMeta
      procedure :: gridcomp_get_outer_meta
   end interface MAPL_GridCompGetOuterMeta

   interface MAPL_GridCompGetRegistry
      procedure :: gridcomp_get_registry
   end interface MAPL_GridCompGetRegistry

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

   interface MAPL_GridCompGetInternalState
      procedure :: get_internal_state
   end interface MAPL_GridCompGetInternalState

   interface MAPL_GridCompAddChild
      procedure :: gridcomp_add_child_config
      procedure :: gridcomp_add_child_by_spec
   end interface MAPL_GridCompAddChild

   interface MAPL_GridCompRunChild
      procedure :: gridcomp_run_child_by_name
   end interface MAPL_GridCompRunChild

   interface MAPL_GridCompRunChildren
      procedure :: gridcomp_run_children
   end interface MAPL_GridCompRunChildren

   interface MAPL_GridCompAddVarSpec
      procedure :: gridcomp_add_varspec_basic
   end interface MAPL_GridCompAddVarSpec

   interface MAPL_GridCompAddSpec
      procedure :: gridcomp_add_spec
   end interface MAPL_GridCompAddSpec

   interface MAPL_GridCompSetGeometry
      procedure :: gridcomp_set_geometry
   end interface MAPL_GridCompSetGeometry

   interface MAPL_GridCompSetEntryPoint
      procedure gridcomp_set_entry_point
   end interface MAPL_GridCompSetEntryPoint

   interface MAPL_GridCompGetResource
      procedure :: gridcomp_get_resource_i4
      procedure :: gridcomp_get_resource_i8
      procedure :: gridcomp_get_resource_r4
      procedure :: gridcomp_get_resource_r8
      procedure :: gridcomp_get_resource_logical
      procedure :: gridcomp_get_resource_i4seq
      procedure :: gridcomp_get_resource_i8seq
      procedure :: gridcomp_get_resource_r4seq
      procedure :: gridcomp_get_resource_r8seq
      procedure :: gridcomp_get_resource_logical_seq
      procedure :: gridcomp_get_resource_string
   end interface MAPL_GridCompGetResource

   interface MAPL_GridCompIsGeneric
      procedure :: gridcomp_is_generic
   end interface MAPL_GridCompIsGeneric

   interface MAPL_GridCompIsUser
      procedure :: gridcomp_is_user
   end interface MAPL_GridCompIsUser

   interface MAPL_GridCompAddConnectivity
      procedure :: gridcomp_add_simple_connectivity
   end interface MAPL_GridCompAddConnectivity

   interface MAPL_GridCompReexport
      procedure :: gridcomp_reexport
   end interface MAPL_GridCompReexport

   interface MAPL_GridCompConnectAll
      procedure :: gridcomp_connect_all
   end interface MAPL_GridCompConnectAll

   interface MAPL_ClockGet
      procedure :: clock_get
   end interface

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

   contains
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

   end subroutine

    subroutine  gridcomp_get_registry(gridcomp, registry, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(StateRegistry), pointer :: registry
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta

      call MAPL_GridCompGetOuterMeta(gridcomp, outer_meta, _RC)
      registry => outer_meta%get_registry()

      _RETURN(_SUCCESS)
   end subroutine gridcomp_get_registry

  subroutine gridcomp_get(gridcomp, unusable, &
        hconfig, &
        logger, &
        geom, &
        grid, &
        num_levels, &
        rc)

      type(ESMF_GridComp), intent(inout) :: gridcomp
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_Hconfig), optional, intent(out) :: hconfig
      class(Logger_t), optional, pointer, intent(out) :: logger
      type(ESMF_Geom), optional, intent(out) :: geom
      type(ESMF_Grid), optional, intent(out) :: grid
      integer, optional, intent(out) :: num_levels
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta_
      type(ESMF_Geom), allocatable :: geom_
      class(VerticalGrid), allocatable :: vertical_grid_

      call MAPL_GridCompGetOuterMeta(gridcomp, outer_meta_, _RC)

      if (present(hconfig)) hconfig = outer_meta_%get_hconfig()
      if (present(logger)) logger => outer_meta_%get_lgr()
      if (present(geom)) geom = outer_meta_%get_geom(_RC)
      if (present(grid)) then
         geom_ = outer_meta_%get_geom(_RC)
         call ESMF_GeomGet(geom_, grid=grid, _RC)
      end if
      if (present(num_levels)) then
         vertical_grid_ = outer_meta_%get_vertical_grid()
         num_levels = vertical_grid_%get_num_levels()
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine gridcomp_get

   subroutine gridcomp_set(gridcomp, unusable, activate_all_exports, activate_all_imports, write_exports, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(in) :: activate_all_exports
      logical, optional, intent(in) :: activate_all_imports
      logical, optional, intent(in) :: write_exports
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta

      call MAPL_GridCompGetOuterMeta(gridcomp, outer_meta, _RC)
      call outer_meta%set(activate_all_exports=activate_all_exports)
      call outer_meta%set(activate_all_imports=activate_all_imports)
      call outer_meta%set(write_exports=write_exports)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine gridcomp_set

   subroutine get_internal_state(gridcomp, internal_state, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(ESMF_State), intent(out) :: internal_state
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta

      call MAPL_GridCompGetOuterMeta(gridcomp, outer_meta, _RC)
      internal_state = outer_meta%get_internal_state()

      _RETURN(_SUCCESS)
   end subroutine get_internal_state

   subroutine gridcomp_add_child_config(gridcomp, child_name, setservices, hconfig, unusable, timeStep, refTime_offset, rc)
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
      type(ChildSpec) :: child_spec

      _ASSERT(is_valid_name(child_name), 'Child name <' // child_name //'> does not conform to GEOS standards.')

      child_spec = ChildSpec(setServices, hconfig=hconfig, timeStep=timeStep, offset=refTime_offset)
      call MAPL_GridCompAddChild(gridcomp, child_name, child_spec, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine gridcomp_add_child_config

   subroutine gridcomp_add_child_by_spec(gridcomp, child_name, child_spec, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      character(len=*), intent(in) :: child_name
#if defined(ESMF_HCONFIGSET_HAS_INTENT_INOUT)
      type(ChildSpec), intent(inout) :: child_spec
#else
      type(ChildSpec), intent(in) :: child_spec
#endif
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta

      call MAPL_GridCompGetOuterMeta(gridcomp, outer_meta, _RC)
      call outer_meta%add_child(child_name, child_spec, _RC)

      _RETURN(_SUCCESS)
   end subroutine gridcomp_add_child_by_spec


   ! In this procedure, gridcomp is actually an _outer_ gridcomp.   The intent is that
   ! an inner gridcomp will call this on its child which is a wrapped user comp.
   recursive subroutine gridcomp_run_child_by_name(gridcomp, child_name, unusable, phase_name, rc)
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
   end subroutine gridcomp_run_child_by_name


   recursive subroutine gridcomp_run_children(gridcomp, unusable, phase_name, rc)
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
   end subroutine gridcomp_run_children


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

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine gridcomp_set_entry_point


   subroutine gridcomp_add_varspec_basic(gridcomp, variable_spec, rc)
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
   end subroutine gridcomp_add_varspec_basic

   subroutine gridcomp_add_spec( &
        gridcomp, &
        state_intent, &
        short_name, &
        standard_name, &
        dims, &
        vstagger, &
        ! OPTIONAL
        ungridded_dims, &
        unusable, &
        units, &
        restart, &
        itemType, &
        add_to_export, &
        rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(ESMF_StateIntent_Flag), intent(in) :: state_intent
      character(*), intent(in) :: short_name
      character(*), intent(in) :: standard_name
      character(*), intent(in) :: dims
      type(VerticalStaggerLoc), intent(in) :: vstagger
      ! OPTIONAL
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: ungridded_dims(:)
      character(*), optional, intent(in) :: units
      integer(kind=kind(MAPL_RESTART)), optional, intent(in) :: restart
      type(ESMF_StateItem_Flag), optional, intent(in) :: itemType
      logical, optional, intent(in) :: add_to_export
      integer, optional, intent(out) :: rc

      type(VariableSpec) :: var_spec
      type(HorizontalDimsSpec) :: horizontal_dims_spec
      type(OuterMetaComponent), pointer :: outer_meta
      type(ComponentSpec), pointer :: component_spec
      character(len=:), allocatable :: units_
      type(UngriddedDims), allocatable :: dim_specs_vec
      integer :: status

      _ASSERT((dims=="xyz") .or. (dims=="xy") .or. (dims=="z"), "dims can be one of xyz/xy/z")
      horizontal_dims_spec = HORIZONTAL_DIMS_GEOM
      if (dims == "z") then
         horizontal_dims_spec = HORIZONTAL_DIMS_NONE
      end if
      ! TODO: Using standard_name, look up field dictionary for units_
      ! If input units is present, override using input values
      if (present(units)) units_ = units
      if (present(ungridded_dims)) dim_specs_vec = UngriddedDims(ungridded_dims)
      var_spec = make_VariableSpec( &
           state_intent, &
           short_name, &
           standard_name=standard_name, &
           units=units_, &
           itemType=itemType, &
           vertical_stagger=vstagger, &
           ungridded_dims=dim_specs_vec, &
           horizontal_dims_spec=horizontal_dims_spec, &
           _RC)
      call MAPL_GridCompGetOuterMeta(gridcomp, outer_meta, _RC)
      component_spec => outer_meta%get_component_spec()
      call component_spec%var_specs%push_back(var_spec)

      if (present(add_to_export)) then
         if (add_to_export) then
            _ASSERT((state_intent==ESMF_STATEINTENT_INTERNAL), "cannot reexport a non-internal spec")
            call gridcomp_reexport( &
                 gridcomp=gridcomp, &
                 src_comp="<self>", &
                 src_name=short_name, &
                 src_intent=esmf_state_intent_to_string(state_intent), &
                 _RC)
         end if
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(restart)
   end subroutine gridcomp_add_spec

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

   subroutine gridcomp_get_resource_i4(gc, keystring, value, unusable, default, value_set, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      character(len=*), intent(in) :: keystring
      integer(kind=ESMF_KIND_I4), intent(inout) :: value
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer(kind=ESMF_KIND_I4), optional, intent(in) :: default
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
   end subroutine gridcomp_get_resource_i4

   subroutine gridcomp_get_resource_i8(gc, keystring, value, unusable, default, value_set, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      character(len=*), intent(in) :: keystring
      integer(kind=ESMF_KIND_I8), intent(inout) :: value
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer(kind=ESMF_KIND_I8), optional, intent(in) :: default
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
   end subroutine gridcomp_get_resource_i8

   subroutine gridcomp_get_resource_r4(gc, keystring, value, unusable, default, value_set, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      character(len=*), intent(in) :: keystring
      real(kind=ESMF_KIND_R4), intent(inout) :: value
      class(KeywordEnforcer), optional, intent(in) :: unusable
      real(kind=ESMF_KIND_R4), optional, intent(in) :: default
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
   end subroutine gridcomp_get_resource_r4

   subroutine gridcomp_get_resource_r8(gc, keystring, value, unusable, default, value_set, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      character(len=*), intent(in) :: keystring
      real(kind=ESMF_KIND_R8), intent(inout) :: value
      class(KeywordEnforcer), optional, intent(in) :: unusable
      real(kind=ESMF_KIND_R8), optional, intent(in) :: default
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
   end subroutine gridcomp_get_resource_r8

   subroutine gridcomp_get_resource_logical(gc, keystring, value, unusable, default, value_set, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      character(len=*), intent(in) :: keystring
      logical, intent(inout) :: value
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(in) :: default
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
   end subroutine gridcomp_get_resource_logical

   subroutine gridcomp_get_resource_string(gc, keystring, value, unusable, default, value_set, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      character(len=*), intent(in) :: keystring
      character(len=:), allocatable, intent(inout) :: value
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), optional, intent(in) :: default
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
   end subroutine gridcomp_get_resource_string

   subroutine gridcomp_get_resource_i4seq(gc, keystring, value, unusable, default, value_set, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      character(len=*), intent(in) :: keystring
      integer(kind=ESMF_KIND_I4), dimension(:), allocatable, intent(inout) :: value
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer(kind=ESMF_KIND_I4), dimension(:), optional, intent(in) :: default
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
   end subroutine gridcomp_get_resource_i4seq

   subroutine gridcomp_get_resource_i8seq(gc, keystring, value, unusable, default, value_set, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      character(len=*), intent(in) :: keystring
      integer(kind=ESMF_KIND_I8), dimension(:), allocatable, intent(inout) :: value
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer(kind=ESMF_KIND_I8), dimension(:), optional, intent(in) :: default
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
   end subroutine gridcomp_get_resource_i8seq

   subroutine gridcomp_get_resource_r4seq(gc, keystring, value, unusable, default, value_set, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      character(len=*), intent(in) :: keystring
      real(kind=ESMF_KIND_R4), dimension(:), allocatable, intent(inout) :: value
      class(KeywordEnforcer), optional, intent(in) :: unusable
      real(kind=ESMF_KIND_R4), dimension(:), optional, intent(in) :: default
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
   end subroutine gridcomp_get_resource_r4seq

   subroutine gridcomp_get_resource_r8seq(gc, keystring, value, unusable, default, value_set, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      character(len=*), intent(in) :: keystring
      real(kind=ESMF_KIND_R8), dimension(:), allocatable, intent(inout) :: value
      class(KeywordEnforcer), optional, intent(in) :: unusable
      real(kind=ESMF_KIND_R8), dimension(:), optional, intent(in) :: default
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
   end subroutine gridcomp_get_resource_r8seq

   subroutine gridcomp_get_resource_logical_seq(gc, keystring, value, unusable, default, value_set, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      character(len=*), intent(in) :: keystring
      logical, dimension(:), allocatable, intent(inout) :: value
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, dimension(:), optional, intent(in) :: default
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
   end subroutine gridcomp_get_resource_logical_seq

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

      gridcomp_is_user = .not. MAPL_GridCompIsGeneric(gridcomp, _RC)

      _RETURN(_SUCCESS)
   end function gridcomp_is_user

   subroutine gridcomp_set_geometry(gridcomp, state_intent, short_name, geom, vertical_grid, rc)
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

      call MAPL_GridCompGetRegistry(gridcomp, registry=registry, _RC)
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
   end subroutine gridcomp_set_geometry


   ! Use "<SELF>" to indicate connection to gridcomp.
   ! src_name and dst_name can be comma-delimited strings for multiple connection
   subroutine gridcomp_add_simple_connectivity(gridcomp, unusable, src_comp, src_names, dst_comp, dst_names, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(*), intent(in) :: src_comp
      character(*), intent(in) :: src_names
      character(*), intent(in) :: dst_comp
      character(*), optional, intent(in) :: dst_names ! default is src_names
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta
      type(ComponentSpec), pointer :: component_spec

      call MAPL_GridCompGetOuterMeta(gridcomp, outer_meta, _RC)
      component_spec => outer_meta%get_component_spec()
      call component_spec%add_connectivity(src_comp=src_comp, src_names=src_names, dst_comp=dst_comp, dst_names=dst_names, _RC)
      
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine gridcomp_add_simple_connectivity


   subroutine gridcomp_reexport(gridcomp, unusable, src_comp, src_name, src_intent, new_name, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(*), intent(in) :: src_comp
      character(*), intent(in) :: src_name
      character(*), optional, intent(in) :: src_intent
      character(*), optional, intent(in) :: new_name
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta
      type(ComponentSpec), pointer :: component_spec

      call MAPL_GridCompGetOuterMeta(gridcomp, outer_meta, _RC)
      component_spec => outer_meta%get_component_spec()
      call component_spec%reexport(src_comp=src_comp, src_name=src_name, src_intent=src_intent, &
           new_name=new_name, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine gridcomp_reexport

   subroutine clock_get(clock, dt, rc)
      use esmf, only: ESMF_ClockGet
      type(ESMF_Clock), intent(in) :: clock
      real(ESMF_KIND_R4), intent(out) :: dt ! timestep in seconds
      integer, optional, intent(out) :: rc

      type(ESMF_TimeInterval) :: timestep
      integer :: seconds, status

      call ESMF_ClockGet(clock, timeStep=timestep, _RC)
      call ESMF_TimeIntervalGet(timestep, s=seconds, _RC)
      dt = real(seconds, kind=ESMF_KIND_R4)

      _RETURN(_SUCCESS)
   end subroutine clock_get

end module mapl3g_Generic
