#include "MAPL_Generic.h"

module mapl3g_FieldCreate
   use mapl3g_VerticalStaggerLoc
   use mapl3g_FieldInfo
   use mapl3g_UngriddedDims
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   use mapl3g_LU_Bound
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
      _ASSERT(present(num_levels) .eqv. present(vert_staggerloc), "num_levels and vert_staggerloc must be both present or both absent")

      call ESMF_FieldEmptySet(field, geom=geom, _RC)
      call MAPL_FieldEmptyComplete(field, &
           typekind=typekind, gridToFieldMap=gridToFieldMap, ungridded_dims=ungridded_dims, &
           num_levels=num_levels, vert_staggerloc=vert_staggerloc, &
           units=units, standard_name=standard_name, long_name=long_name, &
           _RC)

      _RETURN(_SUCCESS)
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

      integer :: status
      type(LU_Bound), allocatable :: bounds(:)
      type(ESMF_Info) :: field_info
      type(VerticalStaggerLoc) :: vert_staggerloc_

      bounds = make_bounds(num_levels=num_levels, ungridded_dims=ungridded_dims)
      call ESMF_FieldEmptyComplete(field, typekind=typekind, &
           gridToFieldMap=gridToFieldMap, &
           ungriddedLBound=bounds%lower, ungriddedUBound=bounds%upper, _RC)

      call ESMF_InfoGetFromHost(field, field_info, _RC)

      vert_staggerloc_ = VERTICAL_STAGGER_NONE
      if (present(vert_staggerloc)) vert_staggerloc_ = vert_staggerloc
      call MAPL_FieldInfoSetInternal(field_info, &
           ungridded_dims=ungridded_dims, &
           num_levels=num_levels, vert_staggerloc=vert_staggerloc_, &
           units=units, standard_name=standard_name, long_name=long_name, _RC)

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


end module mapl3g_FieldCreate
