#include "MAPL.h"

module mapl3g_FieldBundleRemove
   use mapl3g_FieldBundleInfo
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   use esmf
   implicit none(type, external)
   private
   public :: FieldBundleRemove

   interface FieldBundleRemove
      procedure :: bundle_remove
   end interface FieldBundleRemove

contains

   subroutine bundle_remove(fieldBundle, unusable, units,&
         & standard_name, long_name, rc)
      type(ESMF_FieldBundle), intent(inout) :: fieldBundle
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(in) :: units, standard_name, long_name
      type(ESMF_Info) :: bundle_info
      integer, optional, intent(out) :: rc
      integer :: status

      call ESMF_InfoGetFromHost(fieldBundle, bundle_info, _RC)
      call FieldBundleInfoRemoveInternal(bundle_info, units=units,&
         & standard_name=standard_name, long_name=long_name, _RC)

      _UNUSED_DUMMY(unusable)
      _RETURN(_SUCCESS)
   end subroutine bundle_remove

end module mapl3g_FieldBundleRemove
