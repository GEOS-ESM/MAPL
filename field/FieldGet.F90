#include "MAPL_Generic.h"

module mapl3g_FieldGet
   use mapl3g_VerticalStaggerLoc
   use mapl3g_FieldInfo
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   use mapl3g_UngriddedDims
   use esmf
   implicit none (type, external)
   private

   public :: MAPL_FieldGet

   interface MAPL_FieldGet
      procedure field_get
   end interface MAPL_FieldGet

contains

   subroutine field_get(field, unusable, &
        num_levels, vert_staggerloc, num_vgrid_levels, &
        ungridded_dims, &
        units, &
        rc)

      type(ESMF_Field), intent(in) :: field
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: num_levels
      type(VerticalStaggerLoc), optional, intent(out) :: vert_staggerloc
      integer, optional, intent(out) :: num_vgrid_levels
      type(UngriddedDims), optional, intent(out) :: ungridded_dims
      character(len=:), optional, allocatable, intent(out) :: units

      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: info
      logical :: need_info
      character(:), allocatable :: vert_staggerloc_str

      need_info = any([ &
           present(num_levels), present(vert_staggerloc), present(num_vgrid_levels), &
           present(ungridded_dims), &
           present(units) &
           ])

      if (need_info) then
         call ESMF_InfoGetFromHost(field, info, _RC)
         call MAPL_FieldInfoGetInternal(field, &
              num_levels=num_levels, &
              vert_staggerloc=vert_staggerloc, &
              num_vgrid_levels=num_vgrid_levels, &
              ungridded_dims=ungridded_dims, &
              units=units, _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine field_get
      

end module mapl3g_FieldGet
        
        
   
