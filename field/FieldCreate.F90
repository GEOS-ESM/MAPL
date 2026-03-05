#include "MAPL.h"

module mapl3g_FieldCreate

   use mapl3g_VerticalGrid_API
   use mapl3g_VerticalStaggerLoc
   use mapl3g_VerticalAlignment
   use mapl3g_FieldInfo
   use mapl3g_FieldSet
   use mapl3g_FieldGet
   use mapl3g_UngriddedDims
   use mapl3g_HorizontalDimsSpec
   use mapl3g_StateItemAllocation
   use mapl3g_LU_Bound
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   use mapl_InternalConstantsMod, only: MAPL_UNDEFINED_REAL
   use esmf, MAPL_FieldEmptyCreate => ESMF_FieldEmptyCreate

   implicit none(type,external)
   private

   public :: MAPL_FieldCreate
   public :: MAPL_FieldEmptyComplete
   public :: MAPL_FieldsAreAliased

   interface MAPL_FieldCreate
      procedure :: field_create
   end interface MAPL_FieldCreate

   interface MAPL_FieldEmptyComplete
      procedure :: field_empty_complete_from_info
      procedure :: field_empty_complete
   end interface MAPL_FieldEmptyComplete

   interface MAPL_FieldsAreAliased
      procedure :: fields_are_aliased
   end interface MAPL_FieldsAreAliased


   ! internal
   interface make_bounds
      procedure :: make_bounds_from_field
      procedure :: make_bounds_from_args
   end interface make_bounds

contains

   function field_create( &
        geom, typekind, &
        unusable, & ! keyword enforcement
        ! Optional ESMF args
        name, &
        gridToFieldMap, ungridded_dims, &
        ! Optional MAPL args
        vgrid, vert_staggerloc, vert_alignment, &
        units, standard_name, long_name, &
        rc) result(field)
      type(ESMF_Field) :: field
      type(ESMF_Geom), intent(in) :: geom
      type(ESMF_TypeKind_Flag), intent(in) :: typekind
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(*), optional, intent(in) :: name
      integer, optional, intent(in) :: gridToFieldMap(:)
      type(UngriddedDims), optional, intent(in) :: ungridded_dims
      class(VerticalGrid), optional, intent(in) :: vgrid
      type(VerticalStaggerLoc), optional, intent(in) :: vert_staggerloc
      type(VerticalAlignment), optional, intent(in) :: vert_alignment
      character(len=*), optional, intent(in) :: units
      character(len=*), optional, intent(in) :: standard_name
      character(len=*), optional, intent(in) :: long_name
      integer, optional, intent(out) :: rc

      type(UngriddedDims) :: ungrd
      integer :: num_levels
      integer :: status

      field = MAPL_FieldEmptyCreate(name=name, _RC)
      call vertical_level_sanity_check(vgrid, vert_staggerloc, _RC)

      ungrd = UngriddedDims()
      if (present(ungridded_dims)) ungrd = ungridded_dims

      ! Derive num_levels from vgrid if present
      num_levels = 0
      if (present(vgrid) .and. present(vert_staggerloc)) then
         num_levels = vert_staggerloc%get_num_levels(vgrid%get_num_levels())
      end if

      call ESMF_FieldEmptySet(field, geom=geom, _RC)
      call MAPL_FieldEmptyComplete(field, &
           typekind=typekind, gridToFieldMap=gridToFieldMap, ungridded_dims=ungrd, &
           num_levels=num_levels, vert_staggerloc=vert_staggerloc, vert_alignment=vert_alignment, &
           units=units, standard_name=standard_name, long_name=long_name, &
           _RC)
      
      ! Set vgrid info
      if (present(vgrid)) then
         call FieldSet(field, vgrid=vgrid, _RC)
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function field_create

   subroutine field_empty_complete_from_info(field, rc)
      type(ESMF_Field), intent(inout) :: field
      integer, optional, intent(out) :: rc

      integer :: status
      integer, allocatable :: grid_to_field_map(:)
      type(LU_Bound), allocatable :: bounds(:)
      type(esmf_TypeKind_Flag) :: typekind
      type(esmf_FieldStatus_Flag) :: fstatus

      call esmf_FieldGet(field, status=fstatus, _RC)
      _ASSERT(fstatus == ESMF_FIELDSTATUS_GRIDSET, "Field must have a grid to allocate.")
      grid_to_field_map = make_grid_to_field_map(field, _RC)
      bounds = make_bounds(field, _RC)

      call FieldGet(field, typekind=typekind, _RC)
      call esmf_FieldEmptyComplete( &
           field, &
           typekind=typekind, &
           gridToFieldMap=grid_to_field_map, &
           ungriddedLBound=bounds%lower, &
           ungriddedUBound=bounds%upper, &
           _RC)

      _RETURN(_SUCCESS)
   end subroutine field_empty_complete_from_info

   function make_grid_to_field_map(field, rc) result(grid_to_field_map)
      integer, allocatable :: grid_to_field_map(:)
      type(ESMF_Field), intent(in) :: field
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Geom), allocatable :: geom
      type(HorizontalDimsSpec) :: horizontal_dims_spec
      integer :: idim, dim_count

      grid_to_field_map = [integer::] ! function result must always be allocated

      call FieldGet(field, geom=geom, horizontal_dims_spec=horizontal_dims_spec, _RC)
      _ASSERT(allocated(geom), "Must specify a geom before FieldComplete.")
      _ASSERT(horizontal_dims_spec /= HORIZONTAL_DIMS_UNKNOWN, "should be one of GEOM/NONE")

      call ESMF_GeomGet(geom, dimCount=dim_count, _RC)
      grid_to_field_map = [(0, idim=1,dim_count)]
      if (horizontal_dims_spec == HORIZONTAL_DIMS_GEOM) then
         grid_to_field_map = [(idim, idim=1,dim_count)]
      end if

      _RETURN(_SUCCESS)
   end function make_grid_to_field_map

   function make_bounds_from_field(field, rc) result(bounds)
      type(LU_Bound), allocatable :: bounds(:)
      type(ESMF_Field), intent(in) :: field
      integer, optional, intent(out) :: rc

      integer :: status
      type(UngriddedDims) :: ungridded_dims
      integer :: num_levels

      bounds = [LU_Bound :: ] ! function result must always be allocated
      call FieldGet(field, num_levels=num_levels, ungridded_dims=ungridded_dims, _RC)
      bounds = make_bounds(num_levels=num_levels, ungridded_dims=ungridded_dims)

      _RETURN(_SUCCESS)
   end function make_bounds_from_field

   function make_bounds_from_args(num_levels, ungridded_dims) result(bounds)
      type(LU_Bound), allocatable :: bounds(:)
      integer, optional, intent(in) :: num_levels
      type(UngriddedDims), optional, intent(in) :: ungridded_dims

      bounds = [LU_Bound :: ]

      if (present(num_levels)) then
         if (num_levels > 0) then
            bounds = [bounds, LU_Bound(1, num_levels)]
         end if
      end if

      if (present(ungridded_dims)) then
         bounds = [bounds, ungridded_dims%get_bounds()]
      end if

   end function make_bounds_from_args

   subroutine field_empty_complete(field, &
        typekind, unusable, &
        gridToFieldMap, ungridded_dims, &
        num_levels, vert_staggerloc, vert_alignment, &
        units, standard_name, &
        long_name, &
        rc)
      type(ESMF_Field), intent(inout) :: field
      type(ESMF_TypeKind_Flag), intent(in) :: typekind
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: gridToFieldMap(:)
      type(UngriddedDims), optional, intent(in) :: ungridded_dims
      integer, optional, intent(in) :: num_levels
      type(VerticalStaggerLoc), optional, intent(in) :: vert_staggerloc
      type(VerticalAlignment), optional, intent(in) :: vert_alignment
      character(len=*), optional, intent(in) :: units
      character(len=*), optional, intent(in) :: standard_name
      character(len=*), optional, intent(in) :: long_name
      integer, optional, intent(out) :: rc

      type(LU_Bound), allocatable :: bounds(:)
      type(ESMF_Info) :: field_info
      type(VerticalStaggerLoc) :: vert_staggerloc_
      integer, allocatable :: grid_to_field_map(:)
      type(ESMF_Geom) :: geom
      integer :: dim_count, idim, status

      ! Note: This is an internal subroutine called after vgrid has been converted to num_levels
      ! No need to call vertical_level_sanity_check here - it's already been checked in field_create
      if (present(gridToFieldMap)) then
         grid_to_field_map = gridToFieldMap
      else
         call ESMF_FieldGet(field, geom=geom, _RC)
         call ESMF_GeomGet(geom, dimCount=dim_count, _RC)
         allocate(grid_to_field_map(dim_count), source=[(idim, idim=1,dim_count)])
      end if
      bounds = make_bounds(num_levels=num_levels, ungridded_dims=ungridded_dims)

      call ESMF_FieldEmptyComplete( &
           field, &
           typekind=typekind, &
           gridToFieldMap=grid_to_field_map, &
           ungriddedLBound=bounds%lower, &
           ungriddedUBound=bounds%upper, &
           _RC)

      ! Initialize field to zero
      call ESMF_FieldFill(field, dataFillScheme="const", const1=0.d0, _RC)

      call ESMF_InfoGetFromHost(field, field_info, _RC)
      vert_staggerloc_ = VERTICAL_STAGGER_NONE
      if (present(vert_staggerloc)) vert_staggerloc_ = vert_staggerloc

      call FieldInfoSetInternal(field_info, &
           typekind=typekind, &
           ungridded_dims=ungridded_dims, &
           vert_staggerloc=vert_staggerloc_, &
           vert_alignment=vert_alignment, &
           units=units, &
           standard_name=standard_name, &
           long_name=long_name, &
           allocation_status=STATEITEM_ALLOCATION_ALLOCATED, &
           _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine field_empty_complete

   subroutine vertical_level_sanity_check(vgrid, vertical_stagger, rc)
      class(VerticalGrid), optional, intent(in) :: vgrid
      type(VerticalStaggerLoc), optional, intent(in) :: vertical_stagger
      integer, optional, intent(out) :: rc

      if (present(vgrid)) then
         _ASSERT(present(vertical_stagger), "vertical_stagger must be specified for 3D fields")
      end if

      _RETURN(_SUCCESS)
   end subroutine vertical_level_sanity_check

   logical function fields_are_aliased(field1, field2, rc) result(are_aliased)
      type(esmf_Field), intent(in) :: field1
      type(esmf_Field), intent(in) :: field2
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: is_created


      is_created = esmf_FieldIsCreated(field1, _RC)
      _ASSERT(is_created, 'invalid field detected')
      is_created = esmf_FieldIsCreated(field2, _RC)
      _ASSERT(is_created, 'invalid field detected')

      are_aliased = associated(field1%ftypep, field2%ftypep)

      _RETURN(_SUCCESS)
   end function fields_are_aliased

end module mapl3g_FieldCreate
