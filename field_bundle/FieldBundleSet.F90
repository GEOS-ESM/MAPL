#include "MAPL.h"

module mapl3g_FieldBundleSet
   use mapl3g_VerticalGrid_API
   use mapl3g_Field_API
   use mapl3g_UngriddedDims
   use mapl3g_FieldBundleType_Flag
   use mapl3g_VectorBasisKind
   use mapl3g_FieldBundleInfo
   use mapl3g_InfoUtilities
   use mapl3g_FieldBundleGet
   use mapl3g_LU_Bound
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   use esmf
   implicit none(type,external)
   private

   public :: FieldBundleSet
   public :: FieldBundleReset


   interface FieldBundleSet
      procedure bundle_set
   end interface FieldBundleSet

   interface FieldBundleReset
      procedure bundle_reset
   end interface FieldBundleReset

contains

  subroutine bundle_set(fieldBundle, unusable, &
        geom, vgrid, &
        fieldBundleType, typekind, interpolation_weights, &
        ungridded_dims, &
        num_levels, vert_staggerloc, &
        units, standard_name, long_name, &
        allocation_status, &
        bracket_updated, &
        has_deferred_aspects, &
        regridder_param_info, &
        vector_basis_kind, &
        rc)

      type(ESMF_FieldBundle), intent(inout) :: fieldBundle
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_Geom), optional, intent(in) :: geom
      class(VerticalGrid), optional, intent(in) :: vgrid
      type(FieldBundleType_Flag), optional, intent(in) :: fieldBundleType
      type(ESMF_TypeKind_Flag), optional, intent(in) :: typekind
      real(ESMF_KIND_R4), optional, intent(in) :: interpolation_weights(:)
      type(UngriddedDims), optional, intent(in) :: ungridded_dims
      integer, optional, intent(in) :: num_levels
      type(VerticalStaggerLoc), optional, intent(in) :: vert_staggerloc
      character(*), optional, intent(in) :: units
      character(*), optional, intent(in) :: standard_name
      character(*), optional, intent(in) :: long_name
      type(StateItemAllocation), optional, intent(in) :: allocation_status
      logical, optional, intent(in) :: bracket_updated
      logical, optional, intent(in) :: has_deferred_aspects
      type(esmf_Info), optional, intent(in) :: regridder_param_info
      type(VectorBasisKind), optional, intent(in) :: vector_basis_kind
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_GeomType_Flag) :: geomtype
      type(ESMF_Info) :: bundle_info
      type(ESMF_Grid) :: grid
      integer :: i
      type(ESMF_Field), allocatable :: fieldList(:)
      logical, allocatable :: has_geom
      integer, allocatable :: vgrid_id
      type(FieldBundleType_Flag) :: bundle_type
      logical :: has_bundle_type

      if (present(geom)) then
         ! ToDo - update when ESMF makes this interface public.
!#         call ESMF_FieldBundleSet(fieldBundle, geom=geom, _RC)
         call ESMF_GeomGet(geom, geomtype=geomtype, _RC)
         if (geomtype == ESMF_GEOMTYPE_GRID) then
            call ESMF_GeomGet(geom, grid=grid, _RC)
            call FieldBundleReset(fieldBundle)
            call ESMF_FieldBundleSet(fieldBundle, grid=grid, _RC)

            call FieldBundleGet(fieldBundle, fieldList=fieldList, _RC)
            do i = 1, size(fieldList)
               call MAPL_FieldSet(fieldList(i), geom=geom, _RC)
            end do
         else
            _FAIL('unsupported geomtype')
         end if

      end if

      if (present(vgrid)) then
         vgrid_id = vgrid%get_id() ! allocate so "present" below
      end if

      ! Propagate vertical grid information to fields in bundle
      if (present(num_levels) .or. present(vert_staggerloc) .or. present(vgrid)) then
         call FieldBundleGet(fieldBundle, fieldList=fieldList, _RC)
         do i = 1, size(fieldList)
            call MAPL_FieldSet(fieldList(i), vgrid=vgrid, num_levels=num_levels, vert_staggerloc=vert_staggerloc, _RC)
         end do
      end if
      
      ! Note it is important that the next line ALLOCATEs has_geom we
      ! don't want to set it either way in info if geom is not
      ! present.
      if (present(geom)) then
         has_geom = .true.
      end if

      ! Validate vector_basis_kind is only used with vector bundles
      if (present(vector_basis_kind)) then
         if (present(fieldBundleType)) then
            ! Use the fieldBundleType from this call
            bundle_type = fieldBundleType
            has_bundle_type = .true.
         else
            ! Check if bundle already has a type set
            call ESMF_InfoGetFromHost(fieldBundle, bundle_info, _RC)
            call FieldBundleInfoGetInternal(bundle_info, fieldBundleType=bundle_type, _RC)
            has_bundle_type = .true.
         end if

         if (has_bundle_type) then
            if (bundle_type /= FIELDBUNDLETYPE_VECTOR .and. &
                bundle_type /= FIELDBUNDLETYPE_VECTORBRACKET) then
               _FAIL('vector_basis_kind can only be set for vector field bundles')
            end if
         end if
      end if

      ! Some things are treated as field info:
      call ESMF_InfoGetFromHost(fieldBundle, bundle_info, _RC)
      call FieldBundleInfoSetInternal(bundle_info, &
           vgrid_id=vgrid_id, &
           fieldBundleType=fieldBundleType, &
           typekind=typekind, interpolation_weights=interpolation_weights, &
           ungridded_dims=ungridded_dims, &
           num_levels=num_levels, vert_staggerloc=vert_staggerloc, &
           units=units, standard_name=standard_name, long_name=long_name, &
           allocation_status=allocation_status, &
           bracket_updated=bracket_updated, &
           has_geom=has_geom, &
           has_deferred_aspects=has_deferred_aspects, &
           regridder_param_info=regridder_param_info, &
           vector_basis_kind=vector_basis_kind, &
          _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine bundle_set

   subroutine bundle_reset(fieldBundle, status)
      type(ESMF_FieldBundle), intent(inout) :: fieldBundle
      type(ESMF_FieldBundleStatus), optional, intent(in) :: status

      type(ESMF_FieldBundleStatus) :: status_

      status_ = ESMF_FieldBundleStatus(2) ! ESMF_FBSTATUS_EMPTY - default
      if (present(status)) status_ = status
      fieldBundle%this%status = status_
      
   end subroutine bundle_reset


end module mapl3g_FieldBundleSet
