#include "MAPL.h"

module mapl_FieldBundleFilter_mod

   use ESMF
   use mapl_FieldBundleGet_mod, only: FieldBundleGet
   use mapl_ErrorHandling_mod, only: MAPL_Return, MAPL_Verify

   implicit none(type, external)
   private

   public :: FieldBundleFilter

   interface FieldBundleFilter
      module procedure :: bundle_filter
   end interface

   abstract interface
      function I_field_predicate(field, rc) result(remove)
         use ESMF
         type(ESMF_Field), intent(in) :: field
         integer, optional, intent(out) :: rc
         logical :: remove
      end function I_field_predicate
   end interface

contains

   subroutine bundle_filter(bundle, predicate, rc)
      type(ESMF_FieldBundle), intent(inout) :: bundle
      procedure(I_field_predicate) :: predicate
      integer, optional, intent(out) :: rc

      type(ESMF_Field), allocatable :: field_list(:)
      character(len=ESMF_MAXSTR) :: field_name
      integer :: idx, status

      call FieldBundleGet(bundle, fieldList=field_list, _RC) ! addorder
      do idx = 1, size(field_list)
         if (predicate(field_list(idx))) then
            call ESMF_FieldGet(field_list(idx), name=field_name, _RC)
            call ESMF_FieldBundleRemove(bundle, [field_name], _RC)
         end if
      end do

      _RETURN(_SUCCESS)
   end subroutine bundle_filter

end module mapl_FieldBundleFilter_mod
