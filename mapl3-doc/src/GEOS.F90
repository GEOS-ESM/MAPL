#define I_AM_MAIN
#include "MAPL_Generic.h"

program geos
   use mapl3
   use esmf
   implicit none

   integer :: status
   type(ESMF_HConfig) :: hconfig

   call MAPL_Initialize(hconfig, _RC)
   call run_geos(hconfig, _RC)
   call MAPL_Finalize(_RC)

contains

#undef I_AM_MAIN
#include "MAPL_Generic.h"

   subroutine run_geos(hconfig, rc)
      type(ESMF_HConfig), intent(inout) :: hconfig
      integer, optional, intent(out) :: rc

      logical :: has_cap_hconfig
      type(ESMF_HConfig) :: cap_hconfig
      integer :: status

      has_cap_hconfig = ESMF_HConfigIsDefined(hconfig, keystring='cap', _RC)
      _ASSERT(has_cap_hconfig, 'No cap section found in configuration file')
      cap_hconfig = ESMF_HConfigCreateAt(hconfig, keystring='cap', _RC)
      call MAPL_run_driver(cap_hconfig, _RC)
      call ESMF_HConfigDestroy(cap_hconfig, _RC)

      _RETURN(_SUCCESS)
   end subroutine run_geos

end program geos
