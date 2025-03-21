#include "MAPL_ErrLog.h"

submodule (mapl3g_VectorBasis) destroy_field_smod
   use mapl_base, only: MAPL_GridGetCorners
contains

   module subroutine destroy_fields(this)
      type(VectorBasis), intent(inout) :: this

      integer :: i, j

!#      if (.not. allocated(this%elements)) return
!#      do j = 1, size(this%elements,2)
!#         do i =  1, size(this%elements,1)
!#            call ESMF_FieldDestroy(this%elements(i,j))
!#         end do
!#      end do

   end subroutine destroy_fields
   
end submodule destroy_field_smod
