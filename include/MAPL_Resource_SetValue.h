#include "MAPL_Resource_preamble.h"

if (label_is_present) then 

#ifdef IS_ARRAY_ 
   call ESMF_ConfigGetAttribute(config, valuelist = VAL_, count = count, label = actual_label, _RC)
#else
   call ESMF_ConfigGetAttribute(config, VAL_, label = actual_label, _RC) 
#endif

else 
   select type(default) 
   type is(TYPE_) 
      VAL_ = default 
   class default 
      _FAIL(MISMATCH_MESSAGE_) 
   end select 
   value_is_default = .TRUE. 
end if 
call set_do_print(actual_label, do_print)

#include "MAPL_Resource_postscript.h"
