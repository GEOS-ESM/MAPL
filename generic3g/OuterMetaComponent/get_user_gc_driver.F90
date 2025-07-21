#include "MAPL.h"

submodule (mapl3g_OuterMetaComponent) get_user_gc_driver_smod
   implicit none

contains

   module function get_user_gc_driver(this) result(user_gc_driver)
      type(GriddedComponentDriver), pointer :: user_gc_driver
      class(OuterMetaComponent), target, intent(in) :: this
      user_gc_driver => this%user_gc_driver
   end function get_user_gc_driver

end submodule get_user_gc_driver_smod
