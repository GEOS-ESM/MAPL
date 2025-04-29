#include "MAPL_Generic.h"

module mapl3g_FieldBundleSet
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   use mapl3g_Field_API
   use mapl3g_UngriddedDims
   use mapl3g_FieldBundleType_Flag
   use mapl3g_FieldBundleInfo
   use mapl3g_InfoUtilities
   use mapl3g_LU_Bound
   use esmf
   implicit none(type,external)
   private

   public :: FieldBundleSet


   interface FieldBundleSet
      procedure bundle_set
   end interface FieldBundleSet

contains

  subroutine bundle_set(fieldBundle, unusable, &
        geom, &
        fieldBundleType, typekind, interpolation_weights, &
        ungridded_dims, &
        num_levels, vert_staggerloc, &
        units, standard_name, long_name, &
        is_active, &
        rc)

      type(ESMF_FieldBundle), intent(inout) :: fieldBundle
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_Geom), optional, intent(in) :: geom
      type(FieldBundleType_Flag), optional, intent(in) :: fieldBundleType
      type(ESMF_TypeKind_Flag), optional, intent(in) :: typekind
      real(ESMF_KIND_R4), optional, intent(in) :: interpolation_weights(:)
      type(UngriddedDims), optional, intent(in) :: ungridded_dims
      integer, optional, intent(in) :: num_levels
      type(VerticalStaggerLoc), optional, intent(in) :: vert_staggerloc
      character(*), optional, intent(in) :: units
      character(*), optional, intent(in) :: standard_name
      character(*), optional, intent(in) :: long_name
      logical, optional, intent(in) :: is_active
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_GeomType_Flag) :: geomtype
      type(ESMF_Grid) :: grid
      type(ESMF_Info) :: bundle_info

      if (present(geom)) then
         call ESMF_GeomGet(geom, geomtype=geomtype, _RC)
         if (geomtype == ESMF_GEOMTYPE_GRID) then
            call ESMF_GeomGet(geom, grid=grid, _RC)
            call ESMF_FieldBundleSet(fieldBundle, grid=grid, _RC)
            _RETURN(_SUCCESS)
         end if
         _FAIL('unsupported geomtype')
      end if

      ! Some things are treated as field info:
      call ESMF_InfoGetFromHost(fieldBundle, bundle_info, _RC)
      call FieldBundleInfoSetInternal(bundle_info, &
           fieldBundleType=fieldBundleType, &
           typekind=typekind, interpolation_weights=interpolation_weights, &
           ungridded_dims=ungridded_dims, &
           num_levels=num_levels, vert_staggerloc=vert_staggerloc, &
           units=units, standard_name=standard_name, long_name=long_name, &
           is_active=is_active, &
           _RC)


      _RETURN(_SUCCESS)
   end subroutine bundle_set


end module mapl3g_FieldBundleSet
