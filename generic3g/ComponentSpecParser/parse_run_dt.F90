#include "MAPL_ErrLog.h"

submodule (mapl3g_ComponentSpecParser) parse_run_dt_smod

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
         call parse_isostring(isostring, run_dt, _RC) 
         _RETURN(_SUCCESS)

      end function parse_run_dt

      subroutine parse_isostring(isostring, ti, rc)
         character(len=*), intent(in) :: isostring
         type(ESMF_TimeInterval), intent(out) :: ti
         integer, optional, intent(out) :: rc

         integer :: status

         call ESMF_TimeIntervalSet(ti, isostring, _RC)

      end subroutine parse_isostring

end submodule parse_run_dt_smod
