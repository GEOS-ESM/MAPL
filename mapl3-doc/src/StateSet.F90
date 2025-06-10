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
        num_levels, num_vgrid_levels, &
        units, &
        rc)

      type(ESMF_State), intent(inout) :: state
      character(*), intent(in) :: itemName
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_TypeKind_Flag), optional, intent(out) :: typekind
      integer, optional, intent(out) :: num_levels
      integer, optional, intent(out) :: num_vgrid_levels
      character(len=:), optional, allocatable, intent(out) :: units
      integer, optional, intenT(out) :: rc

      type(ESMF_Field) :: field
      integer :: status

      call ESMF_StateGet(state, itemName=itemName, field=field, _RC)
      call MAPL_FieldSet(field, &
           num_levels=num_levels, &
           units=units, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine state_set

end module mapl3g_StateSet
