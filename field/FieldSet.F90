#include "MAPL.h"

module mapl3g_FieldSet

   use mapl3g_VerticalGrid_API
   use mapl3g_VerticalStaggerLoc
   use mapl3g_VerticalAlignment
   use mapl3g_FieldInfo
   use mapl3g_FieldGet
   use mapl3g_FieldDelta
   use mapl3g_StateItemAllocation
   use mapl3g_QuantityTypeMetadata
   use mapl3g_NormalizationMetadata
   use mapl3g_ConservationMetadata
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   use mapl_FieldPointerUtilities, only: FieldGetLocalElementCount
   use mapl3g_UngriddedDims
   use mapl3g_HorizontalDimsSpec, only: HorizontalDimsSpec
   use esmf
   use gftl2_StringVector

   implicit none (type, external)
   private

   public :: FieldSet

   interface FieldSet
      procedure field_set
   end interface FieldSet

contains

   subroutine field_set(field, &
        geom, &
        horizontal_dims_spec, &
        vgrid, &
        vert_staggerloc, vert_alignment, &
        typekind, &
        unusable, &
        num_levels, &
        units, standard_name, long_name, &
       ungridded_dims, &
       quantity_type_metadata, &
       normalization_metadata, &
       conservation_metadata, &
       attributes, &
        allocation_status, &
        has_deferred_aspects, &
        regridder_param_info, &
        rc)
      type(ESMF_Field), intent(inout) :: field
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_Geom), optional, intent(in) :: geom
      type(HorizontalDimsSpec), optional, intent(in) :: horizontal_dims_spec
      class(VerticalGrid), optional, intent(in) :: vgrid
      type(VerticalStaggerLoc), optional, intent(in) :: vert_staggerloc
      type(VerticalAlignment), optional, intent(in) :: vert_alignment
      type(esmf_TypeKind_Flag), optional, intent(in) :: typekind
      integer, optional, intent(in) :: num_levels
      character(len=*), optional, intent(in) :: units
      character(len=*), optional, intent(in) :: standard_name
      character(len=*), optional, intent(in) :: long_name
       type(UngriddedDims), optional, intent(in) :: ungridded_dims
       type(QuantityTypeMetadata), optional, intent(in) :: quantity_type_metadata
       type(NormalizationMetadata), optional, intent(in) :: normalization_metadata
       type(ConservationMetadata), optional, intent(in) :: conservation_metadata
       type(StringVector), optional, intent(in) :: attributes
      type(StateItemAllocation), optional, intent(in) :: allocation_status
      logical, optional, intent(in) :: has_deferred_aspects
      type(esmf_Info), optional, intent(in) :: regridder_param_info
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: field_info
      type(FieldDelta) :: field_delta
      type(esmf_FieldStatus_Flag) :: fstatus
      integer, allocatable :: vgrid_id
      integer :: derived_num_levels, current_num_levels
      integer :: rank, ungriddedDimCount
      integer, allocatable :: localElementCount(:)
      type(VerticalStaggerLoc) :: stagger

      call esmf_FieldGet(field, status=fstatus, _RC)
      if (fstatus == ESMF_FIELDSTATUS_COMPLETE) then
         ! Determine num_levels to pass to FieldDelta
         if (present(vgrid) .and. .not. present(num_levels)) then
            ! vgrid is present but num_levels wasn't explicitly provided
            ! Derive num_levels from vgrid and only pass to FieldDelta if it changed
            if (present(vert_staggerloc)) then
               stagger = vert_staggerloc
            else
               call FieldGet(field, vert_staggerloc=stagger, _RC)
            end if
            derived_num_levels = stagger%get_num_levels(vgrid%get_num_layers())
            
            ! Check if num_levels actually changed
            ! Get current num_levels from field array dimensions, not from vgrid
            localElementCount = FieldGetLocalElementCount(field, _RC)
            call ESMF_FieldGet(field, rank=rank, ungriddedDimCount=ungriddedDimCount, _RC)
            if (ungriddedDimCount > 0) then
               current_num_levels = localElementCount(rank-ungriddedDimCount+1)
            else
               current_num_levels = 0  ! No vertical dimension
            end if
            if (derived_num_levels /= current_num_levels) then
               ! num_levels changed - pass it to FieldDelta
               field_delta = FieldDelta(geom=geom, num_levels=derived_num_levels, typekind=typekind, units=units)
            else
               ! num_levels didn't change - don't pass it to FieldDelta
               field_delta = FieldDelta(geom=geom, typekind=typekind, units=units)
            end if
         else
            ! Either no vgrid, or num_levels was explicitly provided
            field_delta = FieldDelta(geom=geom, num_levels=num_levels, typekind=typekind, units=units)
         end if
         call field_delta%update_field(field, _RC)
      end if

      if (fstatus /= ESMF_FIELDSTATUS_COMPLETE .and. present(geom)) then
         call esmf_FieldEmptyReset(field, status=ESMF_FIELDSTATUS_EMPTY, _RC)
         call esmf_FieldEmptySet(field, geom=geom, _RC)
      end if

      if (present(vgrid)) then
         vgrid_id = vgrid%get_id() ! allocate so "present" below
      end if

      call esmf_InfoGetFromHost(field, field_info, _RC)
      call FieldInfoSetInternal(field_info, &
           horizontal_dims_spec=horizontal_dims_spec, &
           vgrid_id=vgrid_id, &
           vert_staggerloc=vert_staggerloc, &
           vert_alignment=vert_alignment, &
           typekind=typekind, &
           units=units, standard_name=standard_name, long_name=long_name, &
           ungridded_dims=ungridded_dims, &
           quantity_type_metadata=quantity_type_metadata, &
           normalization_metadata=normalization_metadata, &
           conservation_metadata=conservation_metadata, &
           allocation_status=allocation_status, &
           has_deferred_aspects=has_deferred_aspects, &
           regridder_param_info=regridder_param_info, &
           _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(attributes)
   end subroutine field_set

end module mapl3g_FieldSet
