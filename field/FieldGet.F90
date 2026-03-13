#include "MAPL.h"

module mapl3g_FieldGet

   use mapl3g_VerticalGrid_API
   use mapl3g_VerticalAlignment
   use mapl3g_FieldInfo
   use mapl3g_StateItemAllocation
   use mapl3g_QuantityTypeMetadata
   use mapl3g_NormalizationMetadata
   use mapl3g_ConservationMetadata
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   use mapl3g_UngriddedDims
   use mapl3g_VerticalGridManager
   use mapl3g_HorizontalDimsSpec, only: HorizontalDimsSpec
   use esmf

   implicit none (type,external)
   private

   public :: FieldGet

   interface FieldGet
      procedure field_get
   end interface FieldGet

contains

   subroutine field_get(field, unusable, &
        short_name, typekind, &
        geom, horizontal_dims_spec, &
        vgrid, num_levels, num_layers, vert_staggerloc, vert_alignment, num_vgrid_levels, &
        ungridded_dims, &
        quantity_type_metadata, &
        normalization_metadata, &
        conservation_metadata, &
        units, standard_name, long_name, &
        allocation_status, &
        has_deferred_aspects, &
        regridder_param_info, &
        rc)
      type(ESMF_Field), intent(in) :: field
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_Geom), allocatable, optional, intent(out) :: geom
      type(HorizontalDimsSpec), optional, intent(out) :: horizontal_dims_spec
      character(len=:), optional, allocatable, intent(out) :: short_name
      type(ESMF_TypeKind_Flag), optional, intent(out) :: typekind
      class(VerticalGrid), pointer, optional, intent(out) :: vgrid
      integer, optional, intent(out) :: num_levels     ! Actual field levels (depends on vgrid + stagger)
      integer, optional, intent(out) :: num_layers     ! Number of layers from vgrid (CENTER levels)
      type(VerticalStaggerLoc), optional, intent(out) :: vert_staggerloc
      type(VerticalAlignment), optional, intent(out) :: vert_alignment
      integer, optional, intent(out) :: num_vgrid_levels  ! Deprecated: use num_layers instead
      type(UngriddedDims), optional, intent(out) :: ungridded_dims
      type(QuantityTypeMetadata), optional, intent(out) :: quantity_type_metadata
      type(NormalizationMetadata), optional, intent(out) :: normalization_metadata
      type(ConservationMetadata), optional, intent(out) :: conservation_metadata
      character(len=:), optional, allocatable, intent(out) :: units
      character(len=:), optional, allocatable, intent(out) :: standard_name
      character(len=:), optional, allocatable, intent(out) :: long_name
      type(StateItemAllocation), optional, intent(out) :: allocation_status
      logical, optional, intent(out) :: has_deferred_aspects
      type(esmf_Info), optional, allocatable,  intent(out) :: regridder_param_info
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: field_info
      character(len=ESMF_MAXSTR) :: fname
      type(ESMF_FieldStatus_Flag) :: fstatus
      integer :: vgrid_id
      type(VerticalGridManager), pointer :: vgrid_manager

      if (present(short_name)) then
         call ESMF_FieldGet(field, name=fname, _RC)
         short_name = trim(fname)
      end if

      if (present(geom)) then
         call esmf_FieldGet(field, status=fstatus, _RC)
         if (fstatus == ESMF_FIELDSTATUS_EMPTY) then
            ! no op - already deallocated
         end if
         if (any(fstatus == [ESMF_FIELDSTATUS_GRIDSET, ESMF_FIELDSTATUS_COMPLETE])) then
            allocate(geom)
            call ESMF_FieldGet(field, geom=geom, _RC)
         end if
      end if

      call ESMF_InfoGetFromHost(field, field_info, _RC)
      call FieldInfoGetInternal(field_info, &
           typekind=typekind, &
           horizontal_dims_spec=horizontal_dims_spec, &
           vgrid_id=vgrid_id, &
           num_levels=num_levels, &
           num_layers=num_layers, &
           vert_staggerloc=vert_staggerloc, &
           vert_alignment=vert_alignment, &
           num_vgrid_levels=num_vgrid_levels, &
           ungridded_dims=ungridded_dims, &
           quantity_type_metadata=quantity_type_metadata, &
           normalization_metadata=normalization_metadata, &
           conservation_metadata=conservation_metadata, &
           units=units, standard_name=standard_name, long_name=long_name, &
           allocation_status=allocation_status, &
           has_deferred_aspects=has_deferred_aspects, &
           regridder_param_info=regridder_param_info, &
           _RC)

      if (present(vgrid)) then
         if (vgrid_id == VERTICAL_GRID_NOT_FOUND) then
            vgrid => null()
         else
            vgrid_manager => get_vertical_grid_manager()
            vgrid => vgrid_manager%get_grid(id=vgrid_id, _RC)
         end if
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine field_get

end module mapl3g_FieldGet
