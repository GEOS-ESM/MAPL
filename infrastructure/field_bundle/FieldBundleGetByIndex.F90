#include "MAPL.h"

module mapl_FieldBundleGetByIndex_mod
   use mapl_ErrorHandling_mod
   use esmf
   implicit none
   private

   public :: FieldBundleGetByIndex

contains

   ! Get a field from a bundle by its insertion-order index.
   !
   ! Fields are ordered by ESMF add-order (ESMF_ITEMORDER_ADDORDER),
   ! which matches the order fields were added to the bundle.
   ! fieldIndex is 1-based.

   subroutine FieldBundleGetByIndex(bundle, fieldIndex, field, rc)
      type(ESMF_FieldBundle), intent(inout) :: bundle
      integer,                intent(in)    :: fieldIndex
      type(ESMF_Field),       intent(inout) :: field
      integer, optional,      intent(out)   :: rc

      integer :: status
      integer :: fieldCount
      type(ESMF_Field), allocatable :: fieldList(:)
      logical :: in_range

      call ESMF_FieldBundleGet(bundle, fieldCount=fieldCount, _RC)
      in_range = fieldIndex >= 1 .and. fieldIndex <= fieldCount
      _ASSERT(in_range, 'fieldIndex out of range')

      allocate(fieldList(fieldCount), stat=status)
      _VERIFY(status)

      call ESMF_FieldBundleGet(bundle, fieldList=fieldList, &
           itemOrderFlag=ESMF_ITEMORDER_ADDORDER, _RC)

      field = fieldList(fieldIndex)

      _RETURN(_SUCCESS)
   end subroutine FieldBundleGetByIndex

end module mapl_FieldBundleGetByIndex_mod
