#include "MAPL.h"

submodule (mapl3g_GriddedComponentDriver) get_name_smod
   implicit none

contains

   module function get_name(this, rc) result(name)
      character(:), allocatable :: name
      class(GriddedComponentDriver), intent(in) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: buffer

      call ESMF_GridCompGet(this%gridcomp, name=buffer, _RC)
      name = trim(buffer)

      _RETURN(ESMF_SUCCESS)
   end function get_name

end submodule get_name_smod
