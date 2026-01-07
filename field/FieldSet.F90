#include "MAPL.h"

module mapl3g_FieldSet
   use mapl3g_VerticalGrid_API
   use mapl3g_FieldInfo
   use mapl3g_FieldDelta
   use mapl3g_StateItemAllocation
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   use mapl3g_UngriddedDims
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
        vgrid, &
        vert_staggerloc, &
        typekind, &
        unusable, &
        num_levels, &
        units, standard_name, long_name, &
        ungridded_dims, &
        attributes, &
        allocation_status, &
        has_deferred_aspects, &
        regridder_param_info, &
        rc)


      type(ESMF_Field), intent(inout) :: field
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_Geom), optional, intent(in) :: geom
      class(VerticalGrid), optional, intent(in) :: vgrid
      type(VerticalStaggerLoc), optional, intent(in) :: vert_staggerloc
      type(esmf_TypeKind_Flag), optional, intent(in) :: typekind
      integer, optional, intent(in) :: num_levels
      character(len=*), optional, intent(in) :: units
      character(len=*), optional, intent(in) :: standard_name
      character(len=*), optional, intent(in) :: long_name
      type(UngriddedDims), optional, intent(in) :: ungridded_dims
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

      call esmf_FieldGet(field, status=fstatus, _RC)
      if (fstatus == ESMF_FIELDSTATUS_COMPLETE) then
         field_delta = FieldDelta(geom=geom, num_levels=num_levels, typekind=typekind, units=units)
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
           vgrid_id=vgrid_id, &
           vert_staggerloc=vert_staggerloc, &
           num_levels=num_levels, &
           typekind=typekind, &
           units=units, standard_name=standard_name, long_name=long_name, &
           ungridded_dims=ungridded_dims, &
           allocation_status=allocation_status, &
           has_deferred_aspects=has_deferred_aspects, &
           regridder_param_info=regridder_param_info, &
           _RC)

      _RETURN(_SUCCESS)
   end subroutine field_set

end module mapl3g_FieldSet
