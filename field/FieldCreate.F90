#include "MAPL_Generic.h"

module mapl3g_FieldCreate

   use mapl3g_VerticalStaggerLoc
   use mapl3g_FieldInfo
   use mapl3g_UngriddedDims
   use mapl3g_LU_Bound
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   use mapl_InternalConstantsMod, only: MAPL_UNDEFINED_REAL
   use esmf, MAPL_FieldEmptyCreate => ESMF_FieldEmptyCreate

   implicit none(type,external)
   private

   public :: MAPL_FieldCreate
   public :: MAPL_FieldEmptyComplete

   interface MAPL_FieldCreate
      procedure :: field_create
   end interface MAPL_FieldCreate

   interface MAPL_FieldEmptyComplete
      procedure :: field_empty_complete
   end interface MAPL_FieldEmptyComplete

contains

   function field_create( &
        geom, typekind, &
        unusable, & ! keyword enforcement
        ! Optional ESMF args
        gridToFieldMap, ungridded_dims, &
        ! Optional MAPL args
        num_levels, vert_staggerloc, &
        units, standard_name, long_name, &
        rc) result(field)
      type(ESMF_Field) :: field
      type(ESMF_Geom), intent(in) :: geom
      type(ESMF_TypeKind_Flag), intent(in) :: typekind
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: gridToFieldMap(:)
      type(UngriddedDims), optional, intent(in) :: ungridded_dims
      integer, optional, intent(in) :: num_levels
      type(VerticalStaggerLoc), optional, intent(in) :: vert_staggerloc
      character(len=*), optional, intent(in) :: units
      character(len=*), optional, intent(in) :: standard_name
      character(len=*), optional, intent(in) :: long_name
      integer, optional, intent(out) :: rc

      integer :: status

      field = MAPL_FieldEmptyCreate(_RC)
      call vertical_level_sanity_check(num_levels, vert_staggerloc, _RC)

      call ESMF_FieldEmptySet(field, geom=geom, _RC)
      call MAPL_FieldEmptyComplete(field, &
           typekind=typekind, gridToFieldMap=gridToFieldMap, ungridded_dims=ungridded_dims, &
           num_levels=num_levels, vert_staggerloc=vert_staggerloc, &
           units=units, standard_name=standard_name, long_name=long_name, &
           _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function field_create

   subroutine field_empty_complete( field, &
        typekind, unusable, &
        gridToFieldMap, ungridded_dims, &
        num_levels, vert_staggerloc, &
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

      call vertical_level_sanity_check(num_levels, vert_staggerloc, _RC)
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

      call ESMF_InfoGetFromHost(field, field_info, _RC)
      vert_staggerloc_ = VERTICAL_STAGGER_NONE
      if (present(vert_staggerloc)) vert_staggerloc_ = vert_staggerloc
      call MAPL_FieldInfoSetInternal( &
           field_info, &
           ungridded_dims=ungridded_dims, &
           num_levels=num_levels, &
           vert_staggerloc=vert_staggerloc_, &
           grid_to_field_map=grid_to_field_map, &
           units=units, &
           standard_name=standard_name, &
           long_name=long_name, &
           _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine field_empty_complete

   function make_bounds(num_levels, ungridded_dims) result(bounds)
      type(LU_Bound), allocatable :: bounds(:)
      integer, optional, intent(in) :: num_levels
      type(UngriddedDims), optional, intent(in) :: ungridded_dims

      bounds = [LU_Bound :: ]

      if (present(num_levels)) then
         bounds = [bounds, LU_Bound(1, num_levels)]
      end if

      if (present(ungridded_dims)) then
         bounds = [bounds, ungridded_dims%get_bounds()]
      end if

   end function make_bounds

   subroutine vertical_level_sanity_check(num_levels, vertical_stagger, rc)
      integer, optional, intent(in) :: num_levels
      type(VerticalStaggerLoc), optional, intent(in) :: vertical_stagger
      integer, optional, intent(out) :: rc

      if (present(num_levels)) then
         _ASSERT(present(vertical_stagger), "vertical_stagger must be specified for 3D fields")
      end if

      _RETURN(_SUCCESS)
   end subroutine vertical_level_sanity_check

end module mapl3g_FieldCreate
