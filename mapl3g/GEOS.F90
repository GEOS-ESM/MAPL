#define I_AM_MAIN
#include "MAPL_Generic.h"

program geos
   use mapl3
   use esmf
   implicit none

   integer :: status
   type(MaplFramework), pointer :: mapl

   call mapl_get(mapl=mapl)
   call mapl%initialize(configFilenameFromArgNum=1, _RC)

   call run_geos(mapl, _RC)

   call mapl%finalize(_RC)

contains

#undef I_AM_MAIN
#include "MAPL_Generic.h"

   subroutine run_geos(mapl, rc)
      type(MaplFramework), intent(inout) :: mapl
      integer, optional, intent(out) :: rc

      type(ESMF_HConfig) :: cap_hconfig
      integer :: status

      call mapl%get(hconfig=cap_hconfig)
      call MAPL_run_driver(cap_hconfig, _RC)

      _RETURN(_SUCCESS)
   end subroutine run_geos

end program geos
