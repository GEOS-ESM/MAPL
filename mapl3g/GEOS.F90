#define I_AM_MAIN
#include "MAPL_Generic.h"

program geos
   use mapl3
   use esmf
   implicit none

   integer :: status
   type(ESMF_HConfig) :: hconfig
   logical :: is_model_pet

   call MAPL_Initialize(hconfig, is_model_pet=is_model_pet, _RC)
   call run_geos(hconfig, is_model_pet=is_model_pet, _RC)
   call MAPL_Finalize(_RC)

contains

#undef I_AM_MAIN
#include "MAPL_Generic.h"

   subroutine run_geos(hconfig, is_model_pet, rc)
      type(ESMF_HConfig), intent(inout) :: hconfig
      logical, intent(in) :: is_model_pet
      integer, optional, intent(out) :: rc

      logical :: has_cap_hconfig
      type(ESMF_HConfig) :: cap_hconfig
      integer :: status

      has_cap_hconfig = ESMF_HConfigIsDefined(hconfig, keystring='cap', _RC)
      _ASSERT(has_cap_hconfig, 'No cap section found in configuration file')
      cap_hconfig = ESMF_HConfigCreateAt(hconfig, keystring='cap', _RC)

      call MAPL_run_driver(cap_hconfig, is_model_pet=is_model_pet, _RC)
      call ESMF_HConfigDestroy(cap_hconfig, _RC)

      _RETURN(_SUCCESS)
   end subroutine run_geos

end program geos
