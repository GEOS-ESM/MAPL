#include "MAPL.h"

module mapl3g_FieldGet
   use mapl3g_VerticalGrid_API
   use mapl3g_FieldInfo
   use mapl3g_StateItemAllocation
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   use mapl3g_UngriddedDims
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
        geom, &
        num_levels, vert_staggerloc, num_vgrid_levels, &
        ungridded_dims, &
        units, standard_name, long_name, &
        allocation_status, &
        rc)
      type(ESMF_Field), intent(in) :: field
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_Geom), allocatable, optional, intent(out) :: geom
      character(len=:), optional, allocatable, intent(out) :: short_name
      type(ESMF_TypeKind_Flag), optional, intent(out) :: typekind
      integer, optional, intent(out) :: num_levels
      type(VerticalStaggerLoc), optional, intent(out) :: vert_staggerloc
      integer, optional, intent(out) :: num_vgrid_levels
      type(UngriddedDims), optional, intent(out) :: ungridded_dims
      character(len=:), optional, allocatable, intent(out) :: units
      character(len=:), optional, allocatable, intent(out) :: standard_name
      character(len=:), optional, allocatable, intent(out) :: long_name
      type(StateItemAllocation), optional, intent(out) :: allocation_status
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: field_info
      character(len=ESMF_MAXSTR) :: fname
      type(ESMF_FieldStatus_Flag) :: fstatus

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

      if (present(typekind)) then
         call ESMF_FieldGet(field, typekind=typekind, _RC)
      end if

      call ESMF_InfoGetFromHost(field, field_info, _RC)
      call FieldInfoGetInternal(field_info, &
           num_levels=num_levels, &
           vert_staggerloc=vert_staggerloc, &
           num_vgrid_levels=num_vgrid_levels, &
           ungridded_dims=ungridded_dims, &
           units=units, standard_name=standard_name, long_name=long_name, &
           allocation_status=allocation_status, &
           _RC)

      _RETURN(_SUCCESS)
   end subroutine field_get
      
end module mapl3g_FieldGet
        
        
   
