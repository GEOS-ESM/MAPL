#include "MAPL_ErrLog.h"

#if defined TYPE_
#undef TYPE_
#endif

#if defined SELECT_TYPE
#undef SELECT_TYPE
#endif
#define SELECT_TYPE(T, D, V) select type(D); type is (T); V = D; end select

!---------------------------------------------------------------------

! This module contains procedures that are intended to be called from
! within user-level gridded components.  These are primarily thin
! wrappers that access the internal private state of the gridcomp and
! then invoke methods on that type.

! The names of these procedures are meant to be backward compatible
! with earlier MAPL.  However, not all interfaces will be provided.
! E.g., MAPL2 usually provided gridcomp and meta overloads for many
! procedures.  Now the "meta" interfaces are OO methods in either
! inner or outer MetaComponent.

!---------------------------------------------------------------------

module mapl3g_Generic
   use :: mapl3g_InnerMetaComponent, only: InnerMetaComponent
   use :: mapl3g_InnerMetaComponent, only: get_inner_meta
   use :: mapl3g_OuterMetaComponent, only: OuterMetaComponent
   use :: mapl3g_OuterMetaComponent, only: get_outer_meta
   use :: mapl3g_ComponentSpec, only: ComponentSpec
   use :: mapl3g_VariableSpec, only: VariableSpec
   use :: mapl3g_GriddedComponentDriver, only: GriddedComponentDriver
   use :: mapl3g_UngriddedDimsSpec, only: UngriddedDimsSpec
   use :: mapl3g_Validation, only: is_valid_name
   use :: mapl3g_ESMF_Interfaces, only: I_Run
   use :: mapl3g_StateItemSpec
   use :: mapl3g_VerticalGeom
   use :: mapl3g_HierarchicalRegistry
   use mapl_InternalConstantsMod
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
!   use hconfig3g
   use mapl3hconfig_get
   use :: pflogger, only: logger_t => logger
   use mapl_ErrorHandling
   use mapl_KeywordEnforcer
   implicit none
   private

   public :: get_outer_meta_from_inner_gc

   public :: MAPL_GridCompGet
   public :: MAPL_GridCompSetEntryPoint
   public :: MAPL_AddChild
   public :: MAPL_RunChild
   public :: MAPL_RunChildren

!!$   public :: MAPL_GetInternalState

   public :: MAPL_AddSpec
   public :: MAPL_AddImportSpec
   public :: MAPL_AddExportSpec
   public :: MAPL_AddInternalSpec
!!$
    public :: MAPL_ResourceGet

   ! Accessors
!!$   public :: MAPL_GetOrbit
!!$   public :: MAPL_GetCoordinates
!!$   public :: MAPL_GetLayout

   public :: MAPL_GridCompSetGeom
   public :: MAPL_GridCompSetVerticalGeom

   ! Connections
!#   public :: MAPL_AddConnection
   public :: MAPL_ConnectAll


   ! Interfaces

   interface MAPL_GridCompSetGeom
      module procedure MAPL_GridCompSetGeom
      module procedure MAPL_GridCompSetGeomGrid
      module procedure MAPL_GridCompSetGeomMesh
      module procedure MAPL_GridCompSetGeomXgrid
      module procedure MAPL_GridCompSetGeomLocStream
   end interface MAPL_GridCompSetGeom

   interface MAPL_GridCompGet
      procedure :: gridcomp_get
   end interface MAPL_GridCompGet


!!$   interface MAPL_GetInternalState
!!$      module procedure :: get_internal_state
!!$   end interface MAPL_GetInternalState



   interface MAPL_AddChild
      module procedure :: add_child_by_name
   end interface MAPL_AddChild

   interface MAPL_RunChild
      module procedure :: run_child_by_name
   end interface MAPL_RunChild

   interface MAPL_RunChildren
      module procedure :: run_children
   end interface MAPL_RunChildren

   interface MAPL_AddSpec
      procedure :: add_spec_basic
      procedure :: add_spec_explicit
   end interface MAPL_AddSpec

   interface MAPL_AddImportSpec
      module procedure :: add_import_spec_legacy
   end interface MAPL_AddImportSpec

   interface MAPL_AddExportSpec
      module procedure :: add_export_spec
   end interface MAPL_AddExportSpec

   interface MAPL_AddInternalSpec
      module procedure :: add_internal_spec
   end interface MAPL_AddInternalSpec

   interface MAPL_GridCompSetEntryPoint
      module procedure gridcomp_set_entry_point
   end interface MAPL_GridCompSetEntryPoint

   interface MAPL_ConnectAll
      procedure :: gridcomp_connect_all
   end interface MAPL_ConnectAll

   ! MAPL_ResourceGet
   !  This will have at least 4 public specific procedures:
   !     scalar value from hconfig
   !     array value from hconfig
   !     scalar value from gridcomp
   !     array value from gridcomp
   !
   !  For MAPL3, the messages for MAPL_ResourceGet go to pflogger
   !     instead of to standard output/error directly.
   !  The hconfig procedures use a message parameter instead of a logger.
   !  The gridcomp procedures use the pflogger associated with
   !     the gridcomp to write messages.
   interface MAPL_ResourceGet
      module procedure :: mapl_resource_gridcomp_get_scalar
      module procedure :: mapl_resource_get_scalar
   end interface MAPL_ResourceGet

contains

   subroutine gridcomp_get(gridcomp, unusable, &
        hconfig, &
        registry, &
        logger, &
        rc)

      type(ESMF_GridComp), intent(inout) :: gridcomp
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_Hconfig), optional, intent(out) :: hconfig
      type(HierarchicalRegistry), optional, pointer, intent(out) :: registry
      class(Logger_t), optional, pointer, intent(out) :: logger
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta

      outer_meta => get_outer_meta_from_inner_gc(gridcomp, _RC)

      if (present(hconfig)) hconfig = outer_meta%get_hconfig()
      if (present(registry)) registry => outer_meta%get_registry()
      if (present(logger)) logger => outer_meta%get_lgr()

      _RETURN(_SUCCESS)
   end subroutine gridcomp_get

   subroutine add_child_by_name(gridcomp, child_name, setservices, config, rc)
      use mapl3g_UserSetServices
      type(ESMF_GridComp), intent(inout) :: gridcomp
      character(len=*), intent(in) :: child_name
      class(AbstractUserSetServices), intent(in) :: setservices
      type(ESMF_HConfig), intent(inout) :: config
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

   subroutine run_child_by_name(gridcomp, child_name, unusable, phase_name, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      character(len=*), intent(in) :: child_name
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), optional, intent(in) :: phase_name
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta

      outer_meta => get_outer_meta_from_inner_gc(gridcomp, _RC)
      call outer_meta%run_child(child_name, phase_name=phase_name, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine run_child_by_name


   subroutine run_children(gridcomp, unusable, phase_name, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), intent(in) :: phase_name
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta

      outer_meta => get_outer_meta_from_inner_gc(gridcomp, _RC)
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
      type(GriddedComponentDriver), pointer :: user_component

      outer_meta => get_outer_meta_from_inner_gc(gridcomp, _RC)
      user_component => outer_meta%get_user_component()
      call outer_meta%set_entry_point(method_flag, userProcedure, phase_name=phase_name, _RC)
!#      call user_component%set_entry_point(method_flag, userProcedure, phase_name=phase_name, _RC)

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine gridcomp_set_entry_point


   subroutine add_spec_basic(gridcomp, var_spec, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(VariableSpec), intent(in) :: var_spec
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta
      type(ComponentSpec), pointer :: component_spec

      outer_meta => get_outer_meta_from_inner_gc(gridcomp, _RC)
      component_spec => outer_meta%get_component_spec()
      call component_spec%var_specs%push_back(var_spec)

      _RETURN(_SUCCESS)
   end subroutine add_spec_basic

   subroutine add_spec_explicit(gridcomp, state_intent, unusable, short_name, standard_name, typekind, ungridded_dims, units, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(ESMF_Stateintent_Flag), intent(in) :: state_intent
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(*), intent(in) :: short_name
      character(*), intent(in) :: standard_name
      type(ESMF_TypeKind_Flag), optional, intent(in) :: typekind
      type(UngriddedDimsSpec), intent(in) :: ungridded_dims
      character(*), optional, intent(in) :: units
      integer, optional, intent(out) :: rc

      integer :: status
      type(VariableSpec) :: var_spec

!!$      var_spec = VariableSpec(...)
      call MAPL_AddSpec(gridcomp, var_spec, _RC)

      _RETURN(_SUCCESS)
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

!!$      var_spec = VariableSpec( &
!!$           state_intent=ESMF_STATEINTENT_IMPORT, &
!!$           short_name=short_name, &
!!$           typekind=to_typekind(precision), &
!!$           state_item=to_state_item(datatype), &
!!$           units=units, &
!!$           ungridded_dims=to_ungridded_dims(dims, vlocation, ungridded_dims, ungridded_coords) )

      call MAPL_AddSpec(gc, var_spec, _RC)

      _RETURN(ESMF_SUCCESS)
   end subroutine add_import_spec_legacy

   function to_typekind(precision) result(tk)
      type(ESMF_TypeKind_Flag) :: tk
      integer, optional, intent(in) :: precision

      tk = ESMF_TYPEKIND_R4 ! GEOS default
      if (.not. present(precision)) return

!!$      select case (precision)
!!$      case (?? single)
!!$         tk = ESMF_TYPEKIND_R4
!!$      case (?? double)
!!$         tk = ESMF_TYPEKIND_R8
!!$      case default
!!$         tk = ESMF_NOKIND
!!$      end select

   end function to_typekind

   function to_ungridded_dims(dims, vlocation, legacy_ungridded_dims, ungridded_coords) result(ungridded_dims)
      type(UngriddedDimsSpec) :: ungridded_dims
      integer, optional, intent(in) :: dims
      integer, optional, intent(in) :: vlocation
      integer, optional, intent(in) :: legacy_ungridded_dims(:)
      real, optional, intent(in) :: ungridded_coords(:)
      character(len=11) :: dim_name

      if (any(dims == [MAPL_DimsVertOnly, MAPL_DimsHorzVert])) then
!!$         call extra_dims%add_dim_spec(UngriddedDimSpec('lev', ...))
!!$         call ungridded_dims%add_dim_spec(DefferredDimSpec('lev', ...))
      end if

!!$      do i = 1, size(legacy_ungridded_dims)
!!$         write(dim_name,'("ungridded_", i1)') i
!!$         call ungridded_dims%add_dim_spec(dim_name, 'unknown', ungridded_dims(i))
!!$      end do

   end function to_ungridded_dims

   function to_state_item(datatype) result(state_item)
      type(ESMF_StateItem_Flag) :: state_item
      integer, optional, intent(in) :: datatype

      state_item = ESMF_STATEITEM_FIELD ! GEOS default
      if (.not. present(datatype)) return

      select case (datatype)
      case (MAPL_FieldItem)
         state_item = ESMF_STATEITEM_FIELD
      case (MAPL_BundleItem)
         state_item = ESMF_STATEITEM_FIELDBUNDLE
      case (MAPL_StateItem)
         state_item = ESMF_STATEITEM_STATE
      case default
         state_item = ESMF_STATEITEM_UNKNOWN
      end select
   end function to_state_item


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
      call component_spec%var_specs%push_back(VariableSpec(ESMF_STATEINTENT_EXPORT, &
           short_name=short_name, standard_name=standard_name))

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

      outer_meta => get_outer_meta_from_inner_gc(gridcomp, _RC)
      component_spec => outer_meta%get_component_spec()
      call component_spec%var_specs%push_back(VariableSpec(ESMF_STATEINTENT_INTERNAL, &
           short_name=short_name, standard_name=standard_name))

      _RETURN(ESMF_SUCCESS)
   end subroutine add_internal_spec

   subroutine MAPL_GridCompSetVerticalGeom(gridcomp, vertical_geom, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(VerticalGeom), intent(in) :: vertical_geom
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta

      outer_meta => get_outer_meta(gridcomp, _RC)

      call outer_meta%set_vertical_geom(vertical_geom)

      _RETURN(_SUCCESS)
   end subroutine MAPL_GridCompSetVerticalGeom

   subroutine MAPL_GridCompSetGeom(gridcomp, geom, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(ESMF_Geom), intent(in) :: geom
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta

      outer_meta => get_outer_meta(gridcomp, _RC)
      call outer_meta%set_geom(geom)

      _RETURN(_SUCCESS)
   end subroutine MAPL_GridCompSetGeom

   subroutine MAPL_GridCompSetGeomGrid(gridcomp, grid, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(ESMF_Grid), intent(in) :: grid
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta
      type(ESMF_Geom) :: geom

      outer_meta => get_outer_meta(gridcomp, _RC)

      !TODO - staggerloc not needed in nextgen ESMF
      geom = ESMF_GeomCreate(grid, ESMF_STAGGERLOC_INVALID, _RC)
      call outer_meta%set_geom(geom)

      _RETURN(_SUCCESS)
   end subroutine MAPL_GridCompSetGeomGrid

   subroutine MAPL_GridCompSetGeomMesh(gridcomp, mesh, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(ESMF_Mesh), intent(in) :: mesh
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta
      type(ESMF_Geom) :: geom

      outer_meta => get_outer_meta(gridcomp, _RC)

      geom = ESMF_GeomCreate(mesh, _RC)
      call outer_meta%set_geom(geom)

      _RETURN(_SUCCESS)
   end subroutine MAPL_GridCompSetGeomMesh

   subroutine MAPL_GridCompSetGeomXGrid(gridcomp, xgrid, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(ESMF_XGrid), intent(in) :: xgrid
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta
      type(ESMF_Geom) :: geom

      outer_meta => get_outer_meta(gridcomp, _RC)

      geom = ESMF_GeomCreate(xgrid, _RC)
      call outer_meta%set_geom(geom)

      _RETURN(_SUCCESS)
   end subroutine MAPL_GridCompSetGeomXGrid

   subroutine MAPL_GridCompSetGeomLocStream(gridcomp, locstream, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(ESMF_LocStream), intent(in) :: locstream
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta
      type(ESMF_Geom) :: geom

      outer_meta => get_outer_meta(gridcomp, _RC)

      geom = ESMF_GeomCreate(locstream, _RC)
      call outer_meta%set_geom(geom)

      _RETURN(_SUCCESS)
   end subroutine MAPL_GridCompSetGeomLocStream

   subroutine gridcomp_connect_all(gridcomp, src_comp, dst_comp, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      character(*), intent(in) :: src_comp
      character(*), intent(in) :: dst_comp
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta

      outer_meta => get_outer_meta(gridcomp, _RC)
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

   subroutine mapl_resource_get_scalar(hconfig, keystring, value, unusable, found, default, typestring, valuestring, rc)
      type(ESMF_HConfig), intent(inout) :: hconfig
      character(len=*), intent(in) :: keystring
      class(*), intent(inout) :: value
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: found
      class(*), optional, intent(in) :: default
      character(len=:), optional, allocatable, intent(inout) :: typestring
      character(len=:), optional, allocatable, intent(inout) :: valuestring
      integer, optional, intent(out) :: rc
      integer :: status

      call MAPL_HConfigGet(hconfig, keystring, value, found=found, &
         default=default, typestring=typestring, valuestring=valuestring, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine mapl_resource_get_scalar

   ! Finds value given keystring. If default is present, a value is always found, and
   ! is_default indicates whether the value equals the default. default, is_default, and
   ! found are optional. If you don't pass a default, use the found flag to determine if
   ! the value is found. Otherwise, if the value is not found, an exception occurs.
   subroutine mapl_resource_gridcomp_get_scalar(gc, keystring, value, unusable, default, value_set, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      character(len=*), intent(in) :: keystring
      class(*), intent(inout) :: value
      class(KeywordEnforcer), optional, intent(in) :: unusable
      class(*), optional, intent(in) :: default
      logical, optional, intent(out) :: value_set
      integer, optional, intent(out) :: rc
      character(len=*), parameter :: MISMATCH_MSG = 'value and default are not the same_type.'
      character(len=*), parameter :: DEFAULT_OR_VALUE_SET_MSG = 'default or value_set must be present.'
      integer :: status
      logical :: found 
      type(ESMF_HConfig) :: hconfig
      class(Logger_t), pointer :: logger
      character(len=:), allocatable :: typestring
      character(len=:), allocatable :: valuestring

!      if(present(default)) then
!         _ASSERT(same_type_as(value, default), MISMATCH_MSG)
!      else
!         _ASSERT(present(value_set), DEFAULT_OR_VALUE_SET_MSG)
!      end if

      call MAPL_GridCompGet(gc, hconfig=hconfig, logger=logger, _RC)
      call MAPL_ResourceGet(hconfig, keystring, value, found=found, &
         typestring=typestring, valuestring=valuestring, _RC)

!      if(present(default) .and. .not. found) then
!         found = .TRUE.
!      end if

      call log_resource_message(logger, form_message(typestring, keystring, valuestring), _RC)

      if(present(value_set)) value_set = merge(.TRUE., found, present(default))

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
      
   end subroutine mapl_resource_gridcomp_get_scalar

   subroutine log_resource_message(logger, message, rc)
      class(Logger_t), intent(inout) :: logger
      character(len=*), intent(in) :: message
      integer, optional, intent(out) :: rc

      integer :: status

      _ASSERT(len_trim(message) > 0, 'Log message is empty.')
      call logger%info(message)
      _RETURN(_SUCCESS)

   end subroutine log_resource_message

   function form_message(typestring, keystring, valuestring) result(message)
      character(len=:), allocatable :: message
      character(len=*), intent(in) :: typestring
      character(len=*), intent(in) :: keystring
      character(len=*), intent(in) :: valuestring

      message = typestring //' '// keystring //' = '// valuestring

   end function form_message

   function form_array_message(typestring, keystring, valuestring, valuerank, rc) result(message)
      character(len=:), allocatable :: message
      character(len=*), intent(in) :: typestring
      character(len=*), intent(in) :: keystring
      character(len=*), intent(in) :: valuestring
      integer, intent(in) :: valuerank
      integer, optional, intent(out) :: rc
      integer :: status

      _ASSERT(valuerank > 0, 'Rank must be greater than 0.')
      message = form_message(typestring, keystring // rankstring(valuerank), valuestring)
      _RETURN(_SUCCESS)

   end function form_array_message
      
   function rankstring(valuerank) result(string)
      character(len=:), allocatable :: string
      integer, intent(in) :: valuerank

      string = '(:' // repeat(',:', valuerank-1) // ')'

   end function rankstring

end module mapl3g_Generic

!   subroutine mapl_resource_get_scalar(hconfig, keystring, value, found, &
!         unusable, typestring, valuestring, rc)
!      type(ESMF_HConfig), intent(inout) :: hconfig
!      character(len=*), intent(in) :: keystring
!      class(*), intent(inout) :: value  
!      logical, intent(out) :: found
!      class(KeywordEnforcer), optional, intent(in) :: unusable
!      character(len=:), allocatable, optional, intent(inout) :: typestring
!      character(len=:), allocatable, optional, intent(inout) :: valuestring
!      integer, optional, intent(out) :: rc
!      integer :: status
!
!      call MAPL_HConfigGet(hconfig, keystring, value, found=found, &
!         typestring=typestring, valuestring=valuestring, _RC)
!      _RETURN(_SUCCESS)
!      _UNUSED_DUMMY(unusable)
!
!   end subroutine mapl_resource_get_scalar
