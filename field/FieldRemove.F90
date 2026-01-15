#include "MAPL.h"

module mapl3g_FieldRemove

   use mapl3g_FieldInfo
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   use esmf

   implicit none (type, external)
   private

   public :: MAPL_FieldRemove

   interface MAPL_FieldRemove
      procedure field_remove
   end interface MAPL_FieldRemove

contains

   subroutine field_remove(field, unusable, units, rc)
      type(ESMF_Field), intent(inout) :: field
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(in) :: units
      integer, optional, intent(out) :: rc
      integer :: status
      type(ESMF_Info) :: field_info

      call ESMF_InfoGetFromHost(field, field_info, _RC)
      call FieldInfoRemoveInternal(field_info, units=units, _RC)

      _RETURN(_SUCCESS)
   end subroutine field_remove

end module mapl3g_FieldRemove
