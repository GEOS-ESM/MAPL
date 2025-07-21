#include "MAPL.h"

submodule (mapl3g_OuterMetaComponent) get_child_by_name_smod
   use mapl_ErrorHandling
   implicit none

contains

   ! Deep copy of shallow ESMF objects - be careful using result
   ! TODO: Maybe this should return a POINTER
   module function get_child_by_name(this, child_name, rc) result(child_component)
      type(GriddedComponentDriver) :: child_component
      class(OuterMetaComponent), intent(in) :: this
      character(len=*), intent(in) :: child_name
      integer, optional, intent(out) :: rc

      integer :: status
      class(GriddedComponentDriver), pointer :: child_ptr

      child_ptr => this%children%at(child_name, rc=status)
      _ASSERT(associated(child_ptr), 'Child not found: <'//child_name//'>.')

      child_component = child_ptr

      _RETURN(_SUCCESS)
   end function get_child_by_name

end submodule get_child_by_name_smod
