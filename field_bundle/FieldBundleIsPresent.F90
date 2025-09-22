#include "MAPL.h"

module mapl3g_FieldBundleIsPresent
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   use mapl3g_Field_API
   use mapl3g_UngriddedDims
   use mapl3g_FieldBundleType_Flag
   use mapl3g_FieldBundleInfo
   use mapl3g_InfoUtilities
   use mapl3g_LU_Bound
   use esmf
   implicit none
   private

   public :: FieldBundleIsPresent

   interface FieldBundleIsPresent
      procedure bundle_is_present
   end interface FieldBundleIsPresent

contains

   ! Supplement ESMF FieldBundleIsPresent
   !
   ! For "bracket" bundles, additional metadata is stored in the info object

   subroutine bundle_is_present(fieldBundle, unusable, &
        do_regrid_transform, &
        rc)

      type(ESMF_FieldBundle), intent(in) :: fieldBundle
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: do_regrid_transform
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: bundle_info

      ! Get these from FieldBundleInfo
      call ESMF_InfoGetFromHost(fieldBundle, bundle_info, _RC)
      call FieldBundleInfoIsPresentInternal(bundle_info, &
           do_regrid_transform=do_regrid_transform, &
           _RC)

      _RETURN(_SUCCESS)

   end subroutine bundle_is_present

end module mapl3g_FieldBundleIsPresent
