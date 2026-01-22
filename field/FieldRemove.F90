#include "MAPL.h"

module mapl3g_FieldRemove

   use mapl3g_FieldInfo, only: FieldInfoRemoveInternal
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   use esmf

   implicit none (type, external)
   private

   public :: MAPL_FieldRemove

   interface MAPL_FieldRemove
      procedure remove
   end interface MAPL_FieldRemove

contains

   subroutine remove(field, unusable, units, rc)
      type(ESMF_Field), intent(inout) :: field
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(in) :: units
      integer, optional, intent(out) :: rc
      integer :: status
      type(ESMF_Info) :: info

      call ESMF_InfoGetFromHost(field, info, _RC)
      call FieldInfoRemoveInternal(info, units=units, _RC)

      _RETURN(_SUCCESS)
   end subroutine remove

end module mapl3g_FieldRemove
