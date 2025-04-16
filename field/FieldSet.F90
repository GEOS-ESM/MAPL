#include "MAPL_Generic.h"

module mapl3g_FieldSet
   use mapl3g_VerticalStaggerLoc
   use mapl3g_FieldInfo
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
        num_levels, &
        vert_staggerloc, &
        ungridded_dims, &
        units, &
        is_connected, &
        rc)


      type(ESMF_Field), intent(inout) :: field
      integer, optional, intent(in) :: num_levels
      type(VerticalStaggerLoc), optional, intent(in) :: vert_staggerloc
      type(UngriddedDims), optional, intent(in) :: ungridded_dims
      character(len=*), optional, intent(in) :: units
      logical, optional, intent(in) :: is_connected
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: field_info

      call ESMF_InfoGetFromHost(field, field_info, _RC)

      call MAPL_FieldInfoSetInternal(field_info, &
           num_levels=num_levels, &
           vert_staggerloc=vert_staggerloc, &
           ungridded_dims=ungridded_dims, &
           units=units, &
           is_connected=is_connected, &
           _RC)

      _RETURN(_SUCCESS)
   end subroutine field_set


end module mapl3g_FieldSet
