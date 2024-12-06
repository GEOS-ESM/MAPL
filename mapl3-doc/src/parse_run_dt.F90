#include "MAPL_ErrLog.h"

submodule (mapl3g_ComponentSpecParser) parse_run_dt_smod
   use MAPL_TimeStringConversion, only: parse_isostring => string_to_esmf_timeinterval
contains

      module function parse_run_dt(hconfig, rc) result(run_dt)
         type(ESMF_TimeInterval) :: run_dt
         type(ESMF_HConfig), intent(in) :: hconfig
         integer, optional, intent(out) :: rc
         
         integer :: status
         logical :: has_run_dt
         character(len=:), allocatable :: iso_duration

         has_run_dt = ESMF_HConfigIsDefined(hconfig, keyString=KEY_RUN_DT, _RC)
         _RETURN_UNLESS(has_run_dt)
         iso_duration = ESMF_HConfigAsString(hconfig, keyString=KEY_RUN_DT, _RC)
         run_dt = parse_isostring(iso_duration, _RC)
         _RETURN(_SUCCESS)

      end function parse_run_dt

end submodule parse_run_dt_smod
