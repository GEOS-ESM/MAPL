#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) get_child_driver_smod
   use mapl_ErrorHandling_mod
   implicit none(type,external)

contains

   module function get_child_driver(this, child_name, rc) result(driver)
      class(GriddedComponentDriver), pointer :: driver
      class(OuterMetaComponent), target, intent(in) :: this
      character(*), intent(in) :: child_name
      integer, optional, intent(out) :: rc

      integer :: status

      driver => this%children%at(child_name, rc=status)
      _ASSERT(associated(driver), 'OuterMetaComponent: get_child_driver - child not found: <'//child_name//'>.')

      _RETURN(_SUCCESS)
   end function get_child_driver

end submodule get_child_driver_smod
