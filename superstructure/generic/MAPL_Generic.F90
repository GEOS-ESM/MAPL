#include "MAPL.h"

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

module mapl_Generic_mod

   use mapl_InnerMetaComponent_mod, only: InnerMetaComponent
   use mapl_InnerMetaComponent_mod, only: get_inner_meta
   use mapl_OuterMetaComponent_mod, only: OuterMetaComponent
   use mapl_OuterMetaComponent_mod, only: get_outer_meta
   use mapl_ChildSpec_mod, only: ChildSpec
   use mapl_ComponentSpec_mod, only: ComponentSpec, CheckpointControls
   use mapl_VariableSpec_mod, only: VariableSpec, make_VariableSpec
   use mapl_Validation_mod, only: is_valid_name
   use mapl_ESMF_Interfaces_mod, only: I_Run
   use mapl_StateItemSpec_mod
   use mapl_VerticalGrid_mod
   use mapl_VerticalStaggerLoc_mod, only: VerticalStaggerLoc
   use mapl_StateRegistry_mod, only: StateRegistry
   use mapl_HorizontalDimsSpec_mod, only: HorizontalDimsSpec, HORIZONTAL_DIMS_NONE, HORIZONTAL_DIMS_GEOM
   use mapl_UngriddedDim_mod, only: UngriddedDim
   use mapl_UngriddedDims_mod, only: UngriddedDims
   use mapl_StateItem_mod, only: mapl_STATEITEM_STATE, mapl_STATEITEM_FIELDBUNDLE
   use mapl_StateItem_mod, only: mapl_STATEITEM_SERVICE, mapl_STATEITEM_VECTOR
   use mapl_ESMF_Utilities_mod, only: esmf_state_intent_to_string
   use mapl_ESMF_Interfaces_mod, only: mapl_UserCompGetInternalState, mapl_UserCompSetInternalState
   use mapl_hconfig_get_mod
   use mapl_RestartModes_mod, only: RestartMode
   use mapl_ComponentSpecParser_mod, only: parse_geometry_spec
   use mapl_InternalConstants_mod
   use mapl_ErrorHandling_mod
   use mapl_KeywordEnforcer_mod
   use mapl_EsmfRegridder_mod, only: EsmfRegridderParam
   use esmf, only: ESMF_Info, ESMF_InfoIsSet, ESMF_InfoGet, ESMF_InfoGetFromHost
   use esmf, only: ESMF_GridComp, ESMF_GridCompGet
   use esmf, only: ESMF_Geom, ESMF_GeomCreate, ESMF_GeomGet
   use esmf, only: ESMF_Grid, ESMF_Mesh, ESMF_Xgrid, ESMF_LocStream
   use esmf, only: ESMF_STAGGERLOC_INVALID
   use esmf, only: ESMF_HConfig, ESMF_HConfigCreate, ESMF_HConfigDestroy
   use esmf, only: ESMF_Method_Flag
   use esmf, only: ESMF_StateIntent_Flag, ESMF_STATEINTENT_INTERNAL
   use esmf, only: ESMF_KIND_I4, ESMF_KIND_I8, ESMF_KIND_R4, ESMF_KIND_R8
   use esmf, only: ESMF_MAXSTR
   use esmf, only: ESMF_Time, ESMF_TimeInterval, ESMF_TimeIntervalGet, ESMF_Clock, ESMF_ClockGet
   use esmf, only: ESMF_State, ESMF_StateItem_Flag, ESMF_TypeKind_Flag
   use esmf, only: operator(==)
   use pflogger, only: logger_t => logger
   use gftl2_StringVector, only: StringVector

   implicit none(type,external)
   private

   ! These should not be needed by users
   public :: GridCompGetOuterMeta
   public :: GridCompGetRegistry

   ! These should be available to users
   public :: GridCompAddVarSpec
   public :: GridCompAddSpec
   public :: GridCompAdvertiseVariable
   public :: GridCompIsGeneric
   public :: GridCompIsUser

   public :: GridCompGet
   public :: GridCompSet
   public :: GridCompSetEntryPoint

   public :: GridCompAddChild
   public :: GridCompGetChildName
   public :: GridCompRunChild
   public :: GridCompRunChildren

   public :: GridCompGetInternalState

   public :: GridCompSetGeometry

   public :: GridcompGetResource

   public :: ClockGet

   ! Accessors
!!$   public :: MAPL_GetOrbit
!!$   public :: MAPL_GetCoordinates
!!$   public :: MAPL_GetLayout

   public :: GridCompSetGeom
   public :: GridCompSetVerticalGrid

   ! Connections
   public :: GridCompAddConnection
   public :: GridCompAddConnectivity  ! Legacy name - temporary backward compatibility
   public :: GridCompReexport
   public :: GridCompConnectAll

   ! Timers
   public :: GridCompTimerStart
   public :: GridCompTimerStop

   ! Checkpoint directory
   public :: GridCompGetCheckpointDir

   ! Spec types
   public :: mapl_STATEITEM_STATE, mapl_STATEITEM_FIELDBUNDLE
   public :: mapl_STATEITEM_SERVICE, mapl_STATEITEM_VECTOR

   public :: mapl_UserCompGetInternalState, mapl_UserCompSetInternalState

   ! Interfaces

   interface GridCompGetOuterMeta
      procedure :: gridcomp_get_outer_meta
   end interface GridCompGetOuterMeta

   interface GridCompGetRegistry
      procedure :: gridcomp_get_registry
   end interface GridCompGetRegistry

   interface GridCompSetGeom
      procedure GridCompSetGeom
      procedure GridCompSetGeomGrid
      procedure GridCompSetGeomMesh
      procedure GridCompSetGeomXgrid
      procedure GridCompSetGeomLocStream
   end interface GridCompSetGeom

   interface GridCompGet
      procedure :: gridcomp_get
   end interface GridCompGet

   interface GridCompSet
      procedure :: gridcomp_set
   end interface GridCompSet

   interface GridCompGetInternalState
      procedure :: get_internal_state
   end interface GridCompGetInternalState

   interface GridCompAddChild
      procedure :: gridcomp_add_child_by_procedure_and_config
      procedure :: gridcomp_add_child_by_procedure_and_config_file
      procedure :: gridcomp_add_child_by_dso_and_config
      procedure :: gridcomp_add_child_by_dso_and_config_file
      procedure :: gridcomp_add_child_by_spec
   end interface GridCompAddChild

   interface GridCompGetChildName
      procedure :: gridcomp_get_child_name_by_index
   end interface GridCompGetChildName

   interface GridCompRunChild
      procedure :: gridcomp_run_child_by_name
   end interface GridCompRunChild

   interface GridCompRunChildren
      procedure :: gridcomp_run_children
   end interface GridCompRunChildren

   interface GridCompAddVarSpec
      procedure :: gridcomp_add_varspec_basic
   end interface GridCompAddVarSpec

   interface GridCompAddSpec
      procedure :: gridcomp_add_spec
   end interface GridCompAddSpec

   interface GridCompAdvertiseVariable
      procedure :: gridcomp_advertise_variable
   end interface GridCompAdvertiseVariable

   interface GridCompSetGeometry
      procedure :: gridcomp_set_geometry
      procedure :: gridcomp_set_geometry_from_hconfig
   end interface GridCompSetGeometry

   interface GridCompSetEntryPoint
      procedure gridcomp_set_entry_point
   end interface GridCompSetEntryPoint

   interface GridCompGetResource
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
   end interface GridCompGetResource

   interface GridCompIsGeneric
      procedure :: gridcomp_is_generic
   end interface GridCompIsGeneric

   interface GridCompIsUser
      procedure :: gridcomp_is_user
   end interface GridCompIsUser

   interface GridCompAddConnection
      procedure :: gridcomp_add_simple_connection
   end interface GridCompAddConnection

   ! Legacy interface - temporary backward compatibility
   interface GridCompAddConnectivity
      procedure :: gridcomp_add_simple_connection
   end interface GridCompAddConnectivity

   interface GridCompReexport
      procedure :: gridcomp_reexport
   end interface GridCompReexport

   interface GridCompConnectAll
      procedure :: gridcomp_connect_all
   end interface GridCompConnectAll

   interface GridCompTimerStart
      procedure :: gridcomp_timer_start
   end interface GridCompTimerStart

   interface GridCompTimerStop
      procedure :: gridcomp_timer_stop
   end interface GridCompTimerStop

   interface GridCompGetCheckpointDir
      procedure :: gridcomp_get_checkpoint_dir
   end interface GridCompGetCheckpointDir

   interface ClockGet
      procedure :: clock_get_dt
   end interface ClockGet

   interface MAPL_MethodAdd
      procedure :: method_add
   end interface MAPL_MethodAdd

contains

   recursive subroutine gridcomp_get_outer_meta(gridcomp, outer_meta, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(OuterMetaComponent), pointer, intent(out) :: outer_meta
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: is_generic
      type(ESMF_GridComp) :: outer_gc

      is_generic = GridCompIsGeneric(gridcomp, _RC)

      if (is_generic) then
         outer_meta => get_outer_meta(gridcomp, _RC)
         _RETURN(_SUCCESS)
      end if

      ! is user gridcomp
      outer_gc = get_outer_gridcomp(gridcomp, _RC)
      call GridCompGetOuterMeta(outer_gc, outer_meta, _RC)

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

      call GridCompGetOuterMeta(gridcomp, outer_meta, _RC)
      registry => outer_meta%get_registry()

      _RETURN(_SUCCESS)
   end subroutine gridcomp_get_registry

  subroutine gridcomp_get(gridcomp, unusable, &
        name, &
        hconfig, &
        logger, &
        geom, &
        grid, &
        num_levels, &
        num_children, &
        rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(:), optional, allocatable :: name
      type(ESMF_Hconfig), optional, intent(out) :: hconfig
      class(Logger_t), optional, pointer, intent(out) :: logger
      type(ESMF_Geom), optional, intent(out) :: geom
      type(ESMF_Grid), optional, intent(out) :: grid
      integer, optional, intent(out) :: num_levels
      integer, optional, intent(out) :: num_children
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta_
      type(ESMF_Geom), allocatable :: geom_
      class(VerticalGrid), pointer :: vertical_grid_
      character(ESMF_MAXSTR) :: buffer

      call GridCompGetOuterMeta(gridcomp, outer_meta_, _RC)

      if (present(hconfig)) hconfig = outer_meta_%get_hconfig()
      if (present(logger)) logger => outer_meta_%get_logger()
      if (present(geom)) geom = outer_meta_%get_geom(_RC)
      if (present(grid)) then
         geom_ = outer_meta_%get_geom(_RC)
         call ESMF_GeomGet(geom_, grid=grid, _RC)
      end if
      if (present(num_levels)) then
         vertical_grid_ => outer_meta_%get_vertical_grid()
         num_levels = 1
         if (associated(vertical_grid_)) then
            num_levels = vertical_grid_%get_num_layers()
         end if
      end if

      if (present(name)) then
         call esmf_GridCompGet(gridcomp, name=buffer, _RC)
         name = trim(buffer)
      end if

      if (present(num_children)) then
         num_children = outer_meta_%get_num_children()
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine gridcomp_get

   subroutine gridcomp_set(gridcomp, unusable, activate_all_exports, activate_all_imports, checkpoint_controls, restart_controls, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(in) :: activate_all_exports
      logical, optional, intent(in) :: activate_all_imports
      type(CheckpointControls), optional, intent(in) :: checkpoint_controls
      type(CheckpointControls), optional, intent(in) :: restart_controls
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta

      call GridCompGetOuterMeta(gridcomp, outer_meta, _RC)
      call outer_meta%set_misc( &
           activate_all_exports=activate_all_exports, &
           activate_all_imports=activate_all_imports, &
           checkpoint_controls=checkpoint_controls, &
           restart_controls=restart_controls)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine gridcomp_set

   subroutine get_internal_state(gridcomp, internal_state, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(ESMF_State), intent(out) :: internal_state
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta

      call GridCompGetOuterMeta(gridcomp, outer_meta, _RC)
      internal_state = outer_meta%get_internal_state()

      _RETURN(_SUCCESS)
   end subroutine get_internal_state

   subroutine gridcomp_add_child_by_procedure_and_config(gridcomp, child_name, ss_proc, hconfig, unusable, timeStep, refTime_offset, rc)
      use mapl_UserSetServices_mod
      type(ESMF_GridComp), intent(inout) :: gridcomp
      character(len=*), intent(in) :: child_name
      procedure() :: ss_proc
      type(ESMF_HConfig), intent(in) :: hconfig
      class(KeywordEnforcer), optional, intent(out) :: unusable
      type(ESMF_TimeInterval), optional, intent(in) :: timeStep
      type(ESMF_TimeInterval), optional, intent(in) :: refTime_offset
      integer, optional, intent(out) :: rc

      integer :: status
      class(AbstractUserSetServices), allocatable :: setservices
      type(ChildSpec) :: child_spec

      _ASSERT(is_valid_name(child_name), 'Child name <' // child_name //'> does not conform to GEOS standards.')
      setservices = user_setservices(ss_proc)

      child_spec = ChildSpec(setServices, hconfig=hconfig, timeStep=timeStep, offset=refTime_offset)
      call GridCompAddChild(gridcomp, child_name, child_spec, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine gridcomp_add_child_by_procedure_and_config

   subroutine gridcomp_add_child_by_procedure_and_config_file(gridcomp, child_name, ss_proc, hconfig_file, unusable, timeStep, refTime_offset, rc)
      use mapl_UserSetServices_mod
      type(ESMF_GridComp), intent(inout) :: gridcomp
      character(len=*), intent(in) :: child_name
      procedure() :: ss_proc
      character(len=*), intent(in) :: hconfig_file
      class(KeywordEnforcer), optional, intent(out) :: unusable
      type(ESMF_TimeInterval), optional, intent(in) :: timeStep
      type(ESMF_TimeInterval), optional, intent(in) :: refTime_offset
      integer, optional, intent(out) :: rc

      type(ESMF_HConfig) :: hconfig
      integer :: status

      hconfig = ESMF_HConfigCreate(filename=hconfig_file, _RC)
      call GridCompAddChild( &
           gridcomp, &
           child_name, &
           ss_proc, &
           hconfig, &
           timeStep=timeStep, &
           refTime_offset=refTime_offset, &
           _RC)
      call ESMF_HConfigDestroy(hconfig, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine gridcomp_add_child_by_procedure_and_config_file

   subroutine gridcomp_add_child_by_dso_and_config(gridcomp, child_name, shared_obj, user_routine, hconfig, unusable, timeStep, refTime_offset, rc)
      use mapl_UserSetServices_mod
      type(ESMF_GridComp), intent(inout) :: gridcomp
      character(len=*), intent(in) :: child_name
      character(len=*), intent(in) :: shared_obj
      character(len=*), intent(in) :: user_routine
      type(ESMF_HConfig), intent(in) :: hconfig
      class(KeywordEnforcer), optional, intent(out) :: unusable
      type(ESMF_TimeInterval), optional, intent(in) :: timeStep
      type(ESMF_TimeInterval), optional, intent(in) :: refTime_offset
      integer, optional, intent(out) :: rc

      integer :: status
      class(AbstractUserSetServices), allocatable :: setservices
      type(ChildSpec) :: child_spec

      _ASSERT(is_valid_name(child_name), 'Child name <' // child_name //'> does not conform to GEOS standards.')
      setservices = user_setservices(shared_obj, user_routine)

      child_spec = ChildSpec(setServices, hconfig=hconfig, timeStep=timeStep, offset=refTime_offset)
      call GridCompAddChild(gridcomp, child_name, child_spec, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine gridcomp_add_child_by_dso_and_config

   subroutine gridcomp_add_child_by_dso_and_config_file(gridcomp, child_name, shared_obj, user_routine, hconfig_file, unusable, timeStep, refTime_offset, rc)
      use mapl_UserSetServices_mod
      type(ESMF_GridComp), intent(inout) :: gridcomp
      character(len=*), intent(in) :: child_name
      character(len=*), intent(in) :: shared_obj
      character(len=*), intent(in) :: user_routine
      character(len=*), intent(in) :: hconfig_file
      class(KeywordEnforcer), optional, intent(out) :: unusable
      type(ESMF_TimeInterval), optional, intent(in) :: timeStep
      type(ESMF_TimeInterval), optional, intent(in) :: refTime_offset
      integer, optional, intent(out) :: rc

      type(ESMF_HConfig) :: hconfig
      integer :: status

      hconfig = ESMF_HConfigCreate(filename=hconfig_file, _RC)
      call GridCompAddChild( &
           gridcomp, &
           child_name, &
           shared_obj, &
           user_routine, &
           hconfig, &
           timeStep=timeStep, &
           refTime_offset=refTime_offset, &
           _RC)
      call ESMF_HConfigDestroy(hconfig, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine gridcomp_add_child_by_dso_and_config_file

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

      call GridCompGetOuterMeta(gridcomp, outer_meta, _RC)
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

      call GridCompGetOuterMeta(gridcomp, outer_meta, _RC)
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

      call GridCompGetOuterMeta(gridcomp, outer_meta, _RC)
      call outer_meta%run_children(phase_name=phase_name, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine gridcomp_run_children

   function gridcomp_get_child_name_by_index(gridcomp, index, rc) result(name)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      integer, intent(in) :: index
      integer, optional, intent(out) :: rc
      character(len=:), allocatable :: name

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta

      call GridCompGetOuterMeta(gridcomp, outer_meta, _RC)
      name = outer_meta%get_child_name(index, _RC)

      _RETURN(_SUCCESS)
   end function gridcomp_get_child_name_by_index

   subroutine gridcomp_set_entry_point(gridcomp, method_flag, userProcedure, unusable, phase_name, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(ESMF_Method_Flag), intent(in) :: method_flag
      procedure(I_Run) :: userProcedure
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), optional, intent(in) :: phase_name
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta

      call GridCompGetOuterMeta(gridcomp, outer_meta, _RC)
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

      call GridCompGetOuterMeta(gridcomp, outer_meta, _RC)
      component_spec => outer_meta%get_component_spec()
      call component_spec%var_specs%push_back(variable_spec)

      _RETURN(_SUCCESS)
   end subroutine gridcomp_add_varspec_basic

   subroutine gridcomp_add_spec( &
        gridcomp, &
        state_intent, short_name, unusable, &
        standard_name, &
        long_name, &
        geom, &
        units, &
        itemtype, &
        typekind, &
        vertical_grid, &
        vertical_stagger, &
        vertical_alignment, &
        ungridded_dims, &
        fill_value, &
        service_items, &
        attributes, &
        bracket_size, &
        expression, &
        dependencies, &
        regrid_param, &
        horizontal_dims_spec, &
        vector_basis_kind, &
        has_deferred_aspects, &
        use_field_dictionary, &
        restart_mode, &

        dims, &
        add_to_export, &
        export_name, &
        ungridded_dim_array, &
        rc)

      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(ESMF_StateIntent_Flag), intent(in) :: state_intent
      character(*), intent(in) :: short_name
      ! OPTIONAL
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(*), optional, intent(in) :: standard_name
      character(*), optional, intent(in) :: long_name
      type(ESMF_Geom), optional, intent(in) :: geom
      character(*), optional, intent(in) :: units
      character(*), optional, intent(in) :: expression
      type(ESMF_StateItem_Flag), optional, intent(in) :: itemtype
      type(ESMF_TypeKind_Flag), optional, intent(in) :: typekind
      class(VerticalGrid), optional, intent(in) :: vertical_grid
      type(VerticalStaggerLoc), optional, intent(in) :: vertical_stagger
      character(*), optional, intent(in) :: vertical_alignment
      type(UngriddedDims), optional, intent(in) :: ungridded_dims
      real, optional, intent(in) :: fill_value
      type(StringVector), optional :: service_items
      type(StringVector), optional, intent(in) :: attributes
      integer, optional, intent(in) :: bracket_size
      type(StringVector), optional, intent(in) :: dependencies
      type(EsmfRegridderParam), optional, intent(in) :: regrid_param
      type(HorizontalDimsSpec), optional, intent(in) :: horizontal_dims_spec
      character(*), optional, intent(in) :: vector_basis_kind
      logical, optional, intent(in) :: has_deferred_aspects
      logical, optional, intent(in) :: use_field_dictionary
      type(RestartMode), optional, intent(in) :: restart_mode

      character(*), optional, intent(in) :: dims
      logical, optional, intent(in) :: add_to_export
      character(*), optional, intent(in) :: export_name
      type(UngriddedDim), optional, intent(in) :: ungridded_dim_array(:)
      integer, optional, intent(out) :: rc

      type(VariableSpec) :: var_spec
      type(OuterMetaComponent), pointer :: outer_meta
      type(ComponentSpec), pointer :: component_spec
      type(HorizontalDimsSpec) :: horizontal_dims_spec_
      type(UngriddedDims), allocatable :: dim_specs_vec 
      integer :: status

      _FAIL_IF(present(dims) .and. present(horizontal_dims_spec), "dims and horizontal_dims_spec passed")
      _FAIL_IF(present(ungridded_dims) .and. present(ungridded_dim_array), "cannot specify both ungridded_dims and ungridded_dim_array")

      if (present(ungridded_dim_array)) then
         allocate(dim_specs_vec)
         dim_specs_vec = UngriddedDims(ungridded_dim_array)
      else if (present(ungridded_dims)) then
         allocate(dim_specs_vec)
         dim_specs_vec = ungridded_dims
      end if
      horizontal_dims_spec_ = HORIZONTAL_DIMS_GEOM
      if (present(horizontal_dims_spec)) horizontal_dims_spec_ = horizontal_dims_spec 
      if (present(dims)) then
         _ASSERT((dims=="xyz") .or. (dims=="xy") .or. (dims=="z"), "dims can be one of xyz/xy/z")
         horizontal_dims_spec_ = HORIZONTAL_DIMS_GEOM
         if (dims == "z") then
            horizontal_dims_spec_ = HORIZONTAL_DIMS_NONE
         end if
      end if

         var_spec = make_VariableSpec( &
              state_intent, &
              short_name, &
              standard_name=standard_name, &
              long_name=long_name, &
              geom=geom, &
              units=units, &
              expression=expression, &
              itemtype=itemtype, &
              typekind=typekind, &
              vertical_grid=vertical_grid, &
              vertical_stagger=vertical_stagger, &
              vertical_alignment=vertical_alignment, &
              ungridded_dims=dim_specs_vec, &
              fill_value=fill_value, &
              service_items=service_items, &
              attributes=attributes, &
              bracket_size=bracket_size, &
              dependencies=dependencies, &
              regrid_param=regrid_param, &
              horizontal_dims_spec=horizontal_dims_spec_, &
              vector_basis_kind=vector_basis_kind, &
              has_deferred_aspects=has_deferred_aspects, &
              use_field_dictionary=use_field_dictionary, &
              restart_mode=restart_mode, &
              _RC)

      call GridCompGetOuterMeta(gridcomp, outer_meta, _RC)
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
                 new_name=export_name, &
                 _RC)
         end if
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine gridcomp_add_spec

   subroutine gridcomp_advertise_variable(gridcomp, var_spec, rc)
      type(esmf_GridComp), intent(inout) :: gridcomp
      type(VariableSpec), intent(in) :: var_spec
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta

      call GridCompGetOuterMeta(gridcomp, outer_meta, _RC)
      call outer_meta%advertise_variable(var_spec, _RC)

      _RETURN(_SUCCESS)
   end subroutine gridcomp_advertise_variable

   subroutine GridCompSetVerticalGrid(gridcomp, vertical_grid, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      class(VerticalGrid), intent(in) :: vertical_grid
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta

      call GridCompGetOuterMeta(gridcomp, outer_meta, _RC)
      call outer_meta%set_vertical_grid(vertical_grid)

      _RETURN(_SUCCESS)
   end subroutine GridCompSetVerticalGrid

   subroutine GridCompSetGeom(gridcomp, geom, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(ESMF_Geom), intent(in) :: geom
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta

      call GridCompGetOuterMeta(gridcomp, outer_meta, _RC)
      call outer_meta%set_geom(geom)

      _RETURN(_SUCCESS)
   end subroutine GridCompSetGeom

   subroutine GridCompSetGeomGrid(gridcomp, grid, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(ESMF_Grid), intent(in) :: grid
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Geom) :: geom

      geom = ESMF_GeomCreate(grid, ESMF_STAGGERLOC_INVALID, _RC)
      call GridCompSetGeom(gridcomp, geom, _RC)


      _RETURN(_SUCCESS)
   end subroutine GridCompSetGeomGrid

   subroutine GridCompSetGeomMesh(gridcomp, mesh, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(ESMF_Mesh), intent(in) :: mesh
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Geom) :: geom

      geom = ESMF_GeomCreate(mesh, _RC)
      call GridCompSetGeom(gridcomp, geom, _RC)

      _RETURN(_SUCCESS)
   end subroutine GridCompSetGeomMesh

   subroutine GridCompSetGeomXGrid(gridcomp, xgrid, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(ESMF_XGrid), intent(in) :: xgrid
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Geom) :: geom

      geom = ESMF_GeomCreate(xgrid, _RC)
      call GridCompSetGeom(gridcomp, geom, _RC)

      _RETURN(_SUCCESS)
   end subroutine GridCompSetGeomXGrid

   subroutine GridCompSetGeomLocStream(gridcomp, locstream, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(ESMF_LocStream), intent(in) :: locstream
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Geom) :: geom

      geom = ESMF_GeomCreate(locstream, _RC)
      call GridCompSetGeom(gridcomp, geom, _RC)


      _RETURN(_SUCCESS)
   end subroutine GridCompSetGeomLocStream

   subroutine gridcomp_connect_all(gridcomp, src_comp, dst_comp, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      character(*), intent(in) :: src_comp
      character(*), intent(in) :: dst_comp
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta

      call GridCompGetOuterMeta(gridcomp, outer_meta, _RC)
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

      call GridCompGet(gc, hconfig=hconfig, logger=logger, _RC)
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

      call GridCompGet(gc, hconfig=hconfig, logger=logger, _RC)
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

      call GridCompGet(gc, hconfig=hconfig, logger=logger, _RC)
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

      call GridCompGet(gc, hconfig=hconfig, logger=logger, _RC)
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

      call GridCompGet(gc, hconfig=hconfig, logger=logger, _RC)
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

      call GridCompGet(gc, hconfig=hconfig, logger=logger, _RC)
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

      call GridCompGet(gc, hconfig=hconfig, logger=logger, _RC)
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

      call GridCompGet(gc, hconfig=hconfig, logger=logger, _RC)
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

      call GridCompGet(gc, hconfig=hconfig, logger=logger, _RC)
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

      call GridCompGet(gc, hconfig=hconfig, logger=logger, _RC)
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

      call GridCompGet(gc, hconfig=hconfig, logger=logger, _RC)
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

      gridcomp_is_user = .not. GridCompIsGeneric(gridcomp, _RC)

      _RETURN(_SUCCESS)
   end function gridcomp_is_user

   subroutine gridcomp_set_geometry(gridcomp, state_intent, short_name, geom, vertical_grid, rc)
      use mapl_VirtualConnectionPt_mod
      use mapl_ExtensionFamily_mod
      use mapl_StateItemSpec_mod
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
      class(StateItemSpec), pointer :: primary
      class(StateItemSpec), pointer :: spec

      call GridCompGetRegistry(gridcomp, registry=registry, _RC)
      v_pt = VirtualConnectionPt(state_intent, short_name)

      family => registry%get_extension_family(v_pt, _RC)
      _ASSERT(family%has_primary(), 'Should not set geometry on vars from other components.')
      _ASSERT(family%num_variants() == 1, 'No extensions should happen prior to this call.')

      primary => family%get_primary(_RC)
      _ASSERT(associated(primary), 'null pointer for primary')
      spec => primary
      _ASSERT(associated(spec), 'null pointer for spec')

      call spec%set_geometry(geom=geom, vertical_grid=vertical_grid, _RC)

      _RETURN(_SUCCESS)
   end subroutine gridcomp_set_geometry

   subroutine gridcomp_set_geometry_from_hconfig(gridcomp, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      type(ComponentSpec), pointer :: component_spec
      type(ESMF_HConfig) :: hconfig
      type(OuterMetaComponent), pointer :: outer_meta
      type(StateRegistry), pointer :: registry
      character(:), allocatable :: component_name
      integer :: status

      call GridCompGet(gridcomp, hconfig=hconfig, name=component_name, _RC)
      call GridCompGetOuterMeta(gridcomp, outer_meta, _RC)
      component_spec => outer_meta%get_component_spec()
      call GridCompGetRegistry(gridcomp, registry=registry, _RC)
      component_spec%geometry_spec = parse_geometry_spec(hconfig, registry, component_name, _RC)

      _RETURN(_SUCCESS)
   end subroutine gridcomp_set_geometry_from_hconfig

   ! Use "<SELF>" to indicate connection to gridcomp.
   ! src_name and dst_name can be comma-delimited strings for multiple connection
   subroutine gridcomp_add_simple_connection(gridcomp, unusable, src_comp, src_names, dst_comp, dst_names, rc)
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

      call GridCompGetOuterMeta(gridcomp, outer_meta, _RC)
      component_spec => outer_meta%get_component_spec()
      call component_spec%add_connection(src_comp=src_comp, src_names=src_names, dst_comp=dst_comp, dst_names=dst_names, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine gridcomp_add_simple_connection

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

      call GridCompGetOuterMeta(gridcomp, outer_meta, _RC)
      component_spec => outer_meta%get_component_spec()
      call component_spec%reexport(src_comp=src_comp, src_name=src_name, src_intent=src_intent, &
           new_name=new_name, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine gridcomp_reexport

   subroutine gridcomp_timer_start(gridcomp, name, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      character(len=*), intent(in) :: name
      integer, optional, intent(out) :: rc

      type(OuterMetaComponent), pointer :: outer_meta
      integer :: status

      call GridCompGetOuterMeta(gridcomp, outer_meta, _RC)
      call outer_meta%start_timer(name, _RC)

      _RETURN(_SUCCESS)
   end subroutine gridcomp_timer_start

   subroutine gridcomp_timer_stop(gridcomp, name, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      character(len=*), intent(in) :: name
      integer, optional, intent(out) :: rc

      type(OuterMetaComponent), pointer :: outer_meta
      integer :: status

      call GridCompGetOuterMeta(gridcomp, outer_meta, _RC)
      call outer_meta%stop_timer(name, _RC)

      _RETURN(_SUCCESS)
   end subroutine gridcomp_timer_stop

   function gridcomp_get_checkpoint_dir(gridcomp, current_time, unusable, is_private, rc) result(dir)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(ESMF_Time), intent(in) :: current_time
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(in) :: is_private
      integer, optional, intent(out) :: rc
      character(len=:), allocatable :: dir

      type(OuterMetaComponent), pointer :: outer_meta
      integer :: status

      call GridCompGetOuterMeta(gridcomp, outer_meta, _RC)
      dir = outer_meta%get_checkpoint_dir(current_time, is_private=is_private, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function gridcomp_get_checkpoint_dir

   subroutine clock_get_dt(clock, dt, rc)
      type(ESMF_Clock), intent(in) :: clock
      real(ESMF_KIND_R4), intent(out) :: dt ! timestep in seconds
      integer, optional, intent(out) :: rc

      type(ESMF_TimeInterval) :: timestep
      integer :: seconds, status

      call ESMF_ClockGet(clock, timeStep=timestep, _RC)
      call ESMF_TimeIntervalGet(timestep, s=seconds, _RC)
      dt = real(seconds, kind=ESMF_KIND_R4)

       _RETURN(_SUCCESS)
   end subroutine clock_get_dt

   subroutine method_add(state, label, userRoutine, rc)
      use esmf, only: ESMF_State, ESMF_MethodAdd
      use mapl_ESMF_Interfaces_mod, only: I_CallBackMethod
      use mapl_StateAddMethod_mod, only: CallbackMap, CallbackMethodWrapper, get_callbacks
      type(ESMF_State), intent(inout) :: state
      character(len=*), intent(in) :: label
      procedure(I_CallBackMethod) :: userRoutine
      integer, optional, intent(out) :: rc

      integer :: status
      type(CallbackMap), pointer :: callbacks
      type(CallbackMethodWrapper) :: wrapper

      call get_callbacks(state, callbacks, _RC)
      wrapper%userRoutine => userRoutine
      call callbacks%insert(label, wrapper)
      call ESMF_MethodAdd(state, label=label, userRoutine=userRoutine, _RC)

      _RETURN(_SUCCESS)
   end subroutine method_add

end module mapl_Generic_mod
