#include "MAPL_Generic.h"

module mapl3g_FieldBundleGet
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

   public :: MAPL_FieldBundleGet
   public :: MAPL_FieldBundleSet


   interface MAPL_FieldBundleGet
      procedure bundle_get
   end interface MAPL_FieldBundleGet

   interface MAPL_FieldBundleSet
      procedure bundle_set
   end interface MAPL_FieldBundleSet

   character(*), parameter :: KEY_FIELD_BUNDLE_TYPE = '/fieldBundleType'

contains

   ! Supplement ESMF FieldBundleGet
   !
   ! For "bracket" bundles, additional metadata is stored in the info object

   subroutine bundle_get(fieldBundle, unusable, &
        fieldCount, fieldList, geom, &
        fieldBundleType, &
        ! Bracket specific items
        typekind, interpolation_weights, &
        ! Bracket field-prototype items
        ungridded_dims, num_levels, vert_staggerloc, num_vgrid_levels, &
        units, standard_name, long_name, &
        rc)

      type(ESMF_FieldBundle), intent(in) :: fieldBundle
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: fieldCount
      type(ESMF_Field), optional, allocatable, intent(out) :: fieldList(:)
      type(ESMF_Geom), optional, intent(out) :: geom
      type(FieldBundleType_Flag), optional, intent(out) :: fieldBundleType
      type(ESMF_TypeKind_Flag), optional, intent(out) :: typekind
      real(ESMF_KIND_R4), optional, allocatable, intent(out) :: interpolation_weights(:)
      type(UngriddedDims), optional, intent(out) :: ungridded_dims
      integer, optional, intent(out) :: num_levels
      type(VerticalStaggerLoc), optional, intent(out) :: vert_staggerloc
      integer, optional, intent(out) :: num_vgrid_levels
      character(:), optional, allocatable, intent(out) :: units
      character(:), optional, allocatable, intent(out) :: standard_name
      character(:), optional, allocatable, intent(out) :: long_name
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: fieldCount_
      type(ESMF_Info) :: bundle_info

      if (present(fieldCount) .or. present(fieldList)) then
         call ESMF_FieldBundleGet(fieldBundle, fieldCount=fieldCount_, _RC)
          if (present(fieldCount)) then
             fieldCount = fieldCount_
          end if
      end if

      if (present(fieldList)) then
         allocate(fieldList(fieldCount_))
         call ESMF_FieldBundleGet(fieldBundle, fieldList=fieldList, itemOrderflag=ESMF_ITEMORDER_ADDORDER, _RC)
      end if

      if (present(geom)) then
         call get_geom(fieldBundle, geom, rc)
      end if

      ! Get these from FieldBundleInfo
      call ESMF_InfoGetFromHost(fieldBundle, bundle_info, _RC)
      call MAPL_FieldBundleInfoGetInternal(bundle_info, &
           fieldBundleType=fieldBundleType, &
           typekind=typekind, interpolation_weights=interpolation_weights, &
           ungridded_dims=ungridded_dims, &
           num_levels=num_levels, vert_staggerloc=vert_staggerloc, num_vgrid_levels=num_vgrid_levels, &
           units=units, standard_name=standard_name, long_name=long_name, &
           _RC)


      call MAPL_FieldBundleInfoGetInternal(bundle_info, typekind=typekind, fieldBundleType=fieldBundleType, _RC)

      _RETURN(_SUCCESS)

   contains

      subroutine get_geom(fieldBundle, geom, rc)
         type(ESMF_FieldBundle), intent(in) :: fieldBundle
         type(ESMF_Geom), intent(inout) :: geom
         integer, optional, intent(out) :: rc

         integer :: status
         type(ESMF_GeomType_Flag) :: geomtype
         type(ESMF_Grid) :: grid

         call ESMF_FieldBundleGet(fieldBundle, geomtype=geomtype, _RC)
         if (geomtype == ESMF_GEOMTYPE_GRID) then
            call ESMF_FieldBundleGet(fieldBundle, grid=grid, _RC)
            ! probable memory leak
            geom = ESMF_GeomCreate(grid=grid, _RC)
            _RETURN(_SUCCESS)
         end if

         _FAIL('unsupported geomtype; needs simple extension')

         _RETURN(_SUCCESS)
      end subroutine get_geom

   end subroutine bundle_get

   subroutine bundle_set(fieldBundle, unusable, &
        fieldBundleType, typekind, geom, &
        interpolation_weights, ungridded_dims, &
        num_levels, vert_staggerloc, &
        units, &
        rc)

      type(ESMF_FieldBundle), intent(inout) :: fieldBundle
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(FieldBundleType_Flag), optional, intent(in) :: fieldBundleType
      type(ESMF_TypeKind_Flag), optional, intent(in) :: typekind
      type(ESMF_Geom), optional, intent(in) :: geom
      real(ESMF_KIND_R4), optional, intent(in) :: interpolation_weights(:)
      type(UngriddedDims), optional, intent(in) :: ungridded_dims
      integer, optional, intent(in) :: num_levels
      type(VerticalStaggerLoc), optional, intent(in) :: vert_staggerloc
      character(*), optional, intent(in) :: units
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_GeomType_Flag) :: geomtype
      type(ESMF_Grid) :: grid
      type(ESMF_Info) :: bundle_info

      ! Some things are treated as field info:
      call ESMF_InfoGetFromHost(fieldBundle, bundle_info, _RC)
      call MAPL_FieldBundleInfoSetInternal(bundle_info, ungridded_dims=ungridded_dims, typekind=typekind, &
           fieldBundleType=fieldBundleType, interpolation_weights=interpolation_weights, units=units, num_levels=num_levels, &
           vert_staggerloc=vert_staggerloc, _RC)

      if (present(geom)) then
         call ESMF_GeomGet(geom, geomtype=geomtype, _RC)
         if (geomtype == ESMF_GEOMTYPE_GRID) then
            call ESMF_GeomGet(geom, grid=grid, _RC)
            call ESMF_FieldBundleSet(fieldBundle, grid=grid, _RC)
            _RETURN(_SUCCESS)
         end if
         _FAIL('unsupported geomtype')
      end if

      _RETURN(_SUCCESS)
   end subroutine Bundle_Set


end module mapl3g_FieldBundleGet
