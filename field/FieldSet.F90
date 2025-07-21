#include "MAPL.h"

module mapl3g_FieldSet
   use mapl3g_VerticalStaggerLoc
   use mapl3g_FieldInfo
   use mapl3g_FieldDelta
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   use mapl3g_UngriddedDims
   use esmf
   implicit none (type, external)
   private

   public :: FieldSet

   interface FieldSet
      procedure field_set
   end interface FieldSet

contains


   subroutine field_set(field, &
        geom, &
        unusable, &
        num_levels, &
        units, &
        rc)


      type(ESMF_Field), intent(inout) :: field
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_Geom), optional, intent(in) :: geom
      integer, optional, intent(in) :: num_levels
      character(len=*), optional, intent(in) :: units
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: field_info
      type(FieldDelta) :: field_delta


      field_delta = FieldDelta(geom=geom, num_levels=num_levels, units=units)
      call field_delta%update_field(field, _RC)


      _RETURN(_SUCCESS)
   end subroutine field_set


end module mapl3g_FieldSet
