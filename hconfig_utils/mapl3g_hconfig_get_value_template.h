! vim:ft=fortran
#include "mapl3g_hconfig_get_value_declarations.h"

      found = ESMF_HConfigIsDefined(params%hconfig, keyString=params%label, _RC)
      if(present(rc)) rc = merge(_SUCCESS, _FAILURE, params%check_value_set)
      params%value_set = .FALSE.
      if(.not. (found .or. present(default))) return
      ! At this point, either the label was found or default is present.
      
      value_equals_default = present(default) .and. .not. found
      if(found) then
         value = ESMF_HCONFIG_AS (params%hconfig, keyString=params%label, _RC)
      end if

      if(present(default)) then
            if(.not. found) value = default
            value_equals_default = found .and. RELATION(value, default)
      end if

      params%value_set = .TRUE.

      ! If there is no logger, can return now.
      _RETURN_UNLESS(params%has_logger() .or. present(valuestring))
      
      write(buffer, fmt=fmtstr, iostat=status) value
      _VERIFY(status)
      valuestring_ = trim(buffer)
      if(present(valuestring)) valuestring = valuestring_

      _RETURN_UNLESS(params%has_logger())
      call params%log_message(typestring, valuestring_, _RC)
      
      _RETURN(_SUCCESS)

