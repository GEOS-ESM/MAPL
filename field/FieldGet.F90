#include "MAPL_Generic.h"

module mapl3g_FieldGet
   use mapl3g_VerticalStaggerLoc
   use mapl3g_FieldInfo
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   use mapl3g_UngriddedDims
   use esmf
   implicit none (type,external)
   private

   public :: FieldGet
   public :: FieldSet

   interface FieldGet
      procedure field_get
   end interface FieldGet

   interface FieldSet
      procedure field_set
   end interface FieldSet

contains

   subroutine field_get(field, unusable, &
        short_name, typekind, &
        num_levels, vert_staggerloc, num_vgrid_levels, &
        ungridded_dims, &
        units, standard_name, long_name, &
        is_connected, &
        rc)

      type(ESMF_Field), intent(in) :: field
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(len=:), optional, allocatable, intent(out) :: short_name
      type(ESMF_TypeKind_Flag), optional, intent(out) :: typekind
      integer, optional, intent(out) :: num_levels
      type(VerticalStaggerLoc), optional, intent(out) :: vert_staggerloc
      integer, optional, intent(out) :: num_vgrid_levels
      type(UngriddedDims), optional, intent(out) :: ungridded_dims
      character(len=:), optional, allocatable, intent(out) :: units
      character(len=:), optional, allocatable, intent(out) :: standard_name
      character(len=:), optional, allocatable, intent(out) :: long_name
      logical, optional, intent(out) :: is_connected

      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: field_info
      character(len=ESMF_MAXSTR) :: fname

      if (present(short_name)) then
         call ESMF_FieldGet(field, name=fname, _RC)
         short_name = trim(fname)
      end if

      if (present(typekind)) then
         call ESMF_FieldGet(field, typekind=typekind, _RC)
      end if

      call ESMF_InfoGetFromHost(field, field_info, _RC)
      call MAPL_FieldInfoGetInternal(field_info, &
           num_levels=num_levels, &
           vert_staggerloc=vert_staggerloc, &
           num_vgrid_levels=num_vgrid_levels, &
           ungridded_dims=ungridded_dims, &
           units=units, standard_name=standard_name, long_name=long_name, &
           is_connected=is_connected, &
           _RC)

      _RETURN(_SUCCESS)
   end subroutine field_get
      

   subroutine field_set(field, num_levels, vert_staggerloc, &
        ungridded_dims, &
        units, &
        is_connected, &
        rc)

      type(ESMF_Field), intent(inout) :: field
      integer, optional, intent(in) :: num_levels
      type(VerticalStaggerLoc), optional, intent(in) :: vert_staggerloc
      type(UngriddedDims), optional, intent(in) :: ungridded_dims
      character(len=*), optional, intent(in) :: units
      logical, optional, intent(in) :: is_connected

      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: field_info

      call ESMF_InfoGetFromHost(field, field_info, _RC)

      call MAPL_FieldInfoSetInternal(field_info, &
           num_levels=num_levels, &
           vert_staggerloc=vert_staggerloc, &
           ungridded_dims=ungridded_dims, &
           units=units, &
           is_connected=is_connected, &
           _RC)
      

      _RETURN(_SUCCESS)
   end subroutine field_set


end module mapl3g_FieldGet
        
        
   
