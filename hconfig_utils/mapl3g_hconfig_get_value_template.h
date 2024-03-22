! vim:ft=fortran
#include "mapl3g_hconfig_macro_init.h"
#include "mapl3g_hconfig_get_private_macros.h"
#define SET_STATUS(L) merge(_SUCCESS, _FAILURE, L)

   subroutine GET_VALUE_ (hconfig, found, label, value, valuestring, value_equals_default, unusable, default, rc)
      type(ESMF_HConfig), intent(in) :: hconfig
      logical, intent(in) :: found
      character(len=*), intent(in) :: label
      VALTYPE, intent(out) :: value
      character(len=:), allocatable, intent(out) :: valuestring
      logical, intent(out) :: value_equals_default
      class(KeywordEnforcer), optional, intent(in) :: unusable
      class(*), optional, intent(in) :: default
      integer, optional, intent(out) :: rc
      integer :: status
      character(len=MAXSTRLEN) :: buffer

      ! need hconfig(in), value(out), label(in), valuestring(out), default(in, optional), value_equals_default(out, optional)
      ! If label was not found, default must be present. So set value to default.
     
      value_equals_default = present(default) .and. .not. found
      if(found) then
         value = ESMF_HCONFIG_AS (hconfig, keyString=label, _RC)
      end if
      if(present(default)) then
         select type(default)
         type is (DEFTYPE)
            if(.not. found) value = default
            value_equals_default = found .and. RELATION(value, default)
         class default
            _FAIL('Unrecoginized type for label ' // trim(label))
         end select
      end if

      write(buffer, fmt=fmt_, iostat=status) value
      _VERIFY(status)
      valuestring = trim(buffer)
      
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine GET_VALUE_
