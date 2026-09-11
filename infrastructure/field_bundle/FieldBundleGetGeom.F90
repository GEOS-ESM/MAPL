#include "MAPL.h"

module mapl_FieldBundleGetGeom_mod

   use mapl_FieldBundleGet_mod, only: FieldBundleGet
   use mapl_geom_api, only: MAPL_SameGeom => mapl_SameGeom
   use esmf, only: ESMF_Geom, ESMF_FieldBundle, ESMF_Field, ESMF_FieldGet
   use mapl_ErrorHandling_mod

   implicit none(type, external)
   private

   public :: FieldBundleGetGeom

contains

   ! Returns the geom common to all fields in the bundle. Asserts that
   ! all fields in the bundle share the same geom.
   function FieldBundleGetGeom(bundle, rc) result(geom)
      type(ESMF_Geom) :: geom
      type(ESMF_FieldBundle), intent(inout) :: bundle
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i
      type(ESMF_Field), allocatable :: fields(:)
      type(ESMF_Geom) :: trial_geom

      call FieldBundleGet(bundle, fieldList=fields, _RC)
      do i = 1, size(fields)
         call ESMF_FieldGet(fields(i), geom=trial_geom, _RC)
         if (i > 1) then
            _ASSERT(MAPL_SameGeom(trial_geom, geom), 'Fields in bundle have inconsistent geoms')
         end if
         geom = trial_geom
      end do
      _RETURN(_SUCCESS)
   end function FieldBundleGetGeom

end module mapl_FieldBundleGetGeom_mod
