#include "MAPL.h"

module state_utils_setup
   use ESMF
   use MAPL_ExceptionHandling

   implicit none

   type(ESMF_Grid) :: grid
   type(ESMF_Field) :: field_2d, field_3d, mask_field, extra_2d, extra_3d, out_field_2d, out_field_3d
   type(ESMF_State) :: state

end module state_utils_setup
