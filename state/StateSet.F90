#include "MAPL_Generic.h"

module mapl3g_StateSet
   use mapl3g_Field_API
   use mapl3g_UngriddedDims
   use mapl3g_VerticalStaggerLoc
   use mapl_ErrorHandling
   use mapl_KeywordEnforcer
   use esmf
   implicit none(type,external)
   private

   public :: StateSet

   interface StateSet
      procedure state_set
   end interface StateSet

contains

   subroutine state_set(state, itemName, unusable, &
        typekind, &
        num_levels, vert_staggerloc, num_vgrid_levels, &
        ungridded_dims, &
        units, standard_name, long_name, &
        is_active, &
        rc)

      type(ESMF_State), intent(inout) :: state
      character(*), intent(in) :: itemName
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_TypeKind_Flag), optional, intent(out) :: typekind
      integer, optional, intent(out) :: num_levels
      type(VerticalStaggerLoc), optional, intent(out) :: vert_staggerloc
      integer, optional, intent(out) :: num_vgrid_levels
      type(UngriddedDims), optional, intent(out) :: ungridded_dims
      character(len=:), optional, allocatable, intent(out) :: units
      character(len=:), optional, allocatable, intent(out) :: standard_name
      character(len=:), optional, allocatable, intent(out) :: long_name
      logical, optional, intent(out) :: is_active
      integer, optional, intenT(out) :: rc

      type(ESMF_Field) :: field
      integer :: status

      call ESMF_StateGet(state, itemName=itemName, field=field, _RC)
      call MAPL_FieldSet(field, &
           num_levels=num_levels, &
           vert_staggerloc=vert_staggerloc, &
           ungridded_dims=ungridded_dims, &
           units=units, &
           is_active=is_active, &
           _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine state_set

end module mapl3g_StateSet
