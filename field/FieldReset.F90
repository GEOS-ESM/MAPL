#include "MAPL_Generic.h"

module mapl3g_FieldReset
   use esmf
   use mapl_ErrorHandling
   implicit none
   private

   public :: MAPL_FieldReset
      
   interface MAPL_FieldReset
      procedure :: field_reset
   end interface MAPL_FieldReset

contains
   
   subroutine field_reset(field, new_status, rc)
      type(ESMF_Field), intent(inout) :: field
      type(ESMF_FieldStatus_Flag), intent(in) :: new_status
      integer, optional, intent(out) :: rc

      type(ESMF_FieldStatus_Flag) :: old_status
      integer :: status

      _ASSERT(any(new_status == [ESMF_FIELDSTATUS_EMPTY, ESMF_FIELDSTATUS_GRIDSET, ESMF_FIELDSTATUS_COMPLETE]), 'unsupported new status')

      call ESMF_FieldGet(field, status=old_status, _RC)
      _ASSERT(old_status /= ESMF_FIELDSTATUS_UNINIT, 'Field status is UNINIT')
      _ASSERT(new_status /= old_status, 'Field already has selected status.')

      field%ftypep%status = new_status

      if (old_status == ESMF_FIELDSTATUS_COMPLETE) then
         call ESMF_ArrayDestroy(field%ftypep%array, _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine field_reset

end module mapl3g_FieldReset
