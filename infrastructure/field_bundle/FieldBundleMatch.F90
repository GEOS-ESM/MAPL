#include "MAPL.h"
#include "unused_dummy.H"

module mapl_FieldBundleMatch_mod

   use ESMF, only: ESMF_Field, ESMF_FieldBundle, ESMF_FieldBundleGet
   use ESMF, only: ESMF_ITEMORDER_ADDORDER
   use mapl_FieldPointerUtilities_mod, only: FieldSameData
   use mapl_ErrorHandling_mod, only: MAPL_Verify, MAPL_Return

   implicit none(type, external)
   private

   public :: FieldBundleSameData

   interface FieldBundleSameData
      module procedure :: same_data_order_addorder
   end interface

contains

   function same_data_order_addorder(bundle_1, bundle_2, rc) result(match)
      type(ESMF_FieldBundle), intent(inout) :: bundle_1, bundle_2
      integer, optional, intent(out) :: rc
      logical :: match

      integer :: status
      integer :: field_count, n, iter
      type(ESMF_Field), allocatable :: fields_1(:), fields_2(:)

      match = .false.

      ! Match field count
      call ESMF_FieldBundleGet(bundle_1, fieldCount=field_count, _RC)
      call ESMF_FieldBundleGet(bundle_2, fieldCount=n, _RC)
      match = (field_count == n)
      _RETURN_IF(.not. match)

      ! Match field data
      allocate(fields_1(n), fields_2(n))
      call ESMF_FieldBundleGet(bundle_1, itemorderflag=ESMF_ITEMORDER_ADDORDER, fieldList=fields_1, _RC)
      call ESMF_FieldBundleGet(bundle_2, itemorderflag=ESMF_ITEMORDER_ADDORDER, fieldList=fields_2, _RC)
      do iter = 1, field_count
         match = FieldSameData(fields_1(iter), fields_2(iter), _RC)
         _RETURN_IF(.not. match)
      end do

      _RETURN(_SUCCESS)
   end function same_data_order_addorder

end module mapl_FieldBundleMatch_mod
