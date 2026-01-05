#include "MAPL.h"

module mapl3g_FieldBundleGet
   use mapl3g_VerticalGrid_API
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

   public :: FieldBundleGet


   interface FieldBundleGet
      procedure bundle_get
   end interface FieldBundleGet

contains

   ! Supplement ESMF FieldBundleGet
   !
   ! For "bracket" bundles, additional metadata is stored in the info object

   subroutine bundle_get(fieldBundle, unusable, &
        fieldCount, fieldList, geom, vgrid, &
        fieldBundleType, &
        ! Bracket specific items
        typekind, interpolation_weights, &
        ! Bracket field-prototype items
        ungridded_dims, num_levels, vert_staggerloc, num_vgrid_levels, &
        units, standard_name, long_name, &
        allocation_status, &
        bracket_updated, &
        has_deferred_aspects, &
        rc)

      type(ESMF_FieldBundle), intent(in) :: fieldBundle
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: fieldCount
      type(ESMF_Field), optional, allocatable, intent(out) :: fieldList(:)
      type(ESMF_Geom), allocatable, optional, intent(out) :: geom
      class(VerticalGrid), pointer, optional, intent(out) :: vgrid
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
      type(StateItemAllocation), optional, intent(out) :: allocation_status
      logical, optional, intent(out) :: bracket_updated
      logical, optional, intent(out) :: has_deferred_aspects
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: fieldCount_
      type(ESMF_Info) :: bundle_info
      logical :: has_geom
      integer :: vgrid_id
      type(VerticalGridManager), pointer :: vgrid_manager

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

     ! Get these from FieldBundleInfo
      call ESMF_InfoGetFromHost(fieldBundle, bundle_info, _RC)
      call FieldBundleInfoGetInternal(bundle_info, &
           fieldBundleType=fieldBundleType, &
           typekind=typekind, interpolation_weights=interpolation_weights, &
           ungridded_dims=ungridded_dims, &
           num_levels=num_levels, vert_staggerloc=vert_staggerloc, num_vgrid_levels=num_vgrid_levels, &
           units=units, standard_name=standard_name, long_name=long_name, &
           allocation_status=allocation_status, &
           bracket_updated=bracket_updated, &
           has_geom=has_geom, &
           vgrid_id=vgrid_id, &
           has_deferred_aspects=has_deferred_aspects, &
           _RC)

      if (present(geom) .and. has_geom) then
         allocate(geom)
         call get_geom(fieldBundle, geom, rc)
      end if

      if (present(vgrid)) then
         if (vgrid_id == VERTICAL_GRID_NOT_FOUND) then
            vgrid => null()
         else
            vgrid_manager => get_vertical_grid_manager()
            vgrid => vgrid_manager%get_grid(id=vgrid_id, _RC)
         end if
      end if

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

end module mapl3g_FieldBundleGet
