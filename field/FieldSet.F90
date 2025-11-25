#include "MAPL.h"

module mapl3g_FieldSet
   use mapl3g_VerticalGrid_API
   use mapl3g_FieldInfo
   use mapl3g_FieldDelta
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   use mapl3g_UngriddedDims
   use esmf
   use gftl2_StringVector
   implicit none (type, external)
   private

   public :: FieldSet

   interface FieldSet
      procedure field_set
   end interface FieldSet

contains


subroutine field_set(field, &
        geom, &
        vertical_grid, &
        vert_staggerloc, &
        typekind, &
        unusable, &
        num_levels, &
        units, &
        ungridded_dims, &
        attributes, &
        rc)


      type(ESMF_Field), intent(inout) :: field
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_Geom), optional, intent(in) :: geom
      class(VerticalGrid), optional, target, intent(in) :: vertical_grid
      type(VerticalStaggerLoc), optional, intent(in) :: vert_staggerloc
      type(esmf_TypeKind_Flag), optional, intent(in) :: typekind
      integer, optional, intent(in) :: num_levels
      character(len=*), optional, intent(in) :: units
      type(UngriddedDims), optional, intent(in) :: ungridded_dims
      type(StringVector), optional, intent(in) :: attributes
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: field_info
      type(FieldDelta) :: field_delta
      type(esmf_FieldStatus_Flag) :: fstatus

      call esmf_FieldGet(field, status=fstatus, _RC)
      if (fstatus == ESMF_FIELDSTATUS_COMPLETE) then
         field_delta = FieldDelta(geom=geom, num_levels=num_levels, typekind=typekind, units=units)
         call field_delta%update_field(field, _RC)
      end if

      call esmf_InfoGetFromHost(field, field_info, _RC)
      call FieldInfoSetInternal(field_info, &
           vertical_grid=vertical_grid, &
           vert_staggerloc=vert_staggerloc, &
           typekind=typekind, units=units, &
           ungridded_dims=ungridded_dims, _RC)

      _RETURN(_SUCCESS)
   end subroutine field_set

end module mapl3g_FieldSet
