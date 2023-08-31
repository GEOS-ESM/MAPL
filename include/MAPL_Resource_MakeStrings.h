#include "MAPL_Resource_preamble.h"

if (label_is_present) then 

   if(default_is_present) then 
      select type(default) 
      type is(TYPE_) 
#ifdef IS_ARRAY_
         value_is_default = all(are_equal(VAL_, default)) 
#else
         value_is_default = are_equal(VAL, default) 
#endif
      class default 
         _FAIL(MISMATCH_MESSAGE_) 
      end select 
   else 
      value_is_default = .FALSE. 
   end if 

else 

   value_is_default = .TRUE. 

end if 

if (.not. (print_nondefault_only .and. value_is_default)) then 
   type_string = TYPE_STRING_
   type_format = FMT_
#ifdef IS_ARRAY_
   write(array_size_string, '(i2)', iostat=io_stat) size(VAL_) 
   _ASSERT((io_stat == IO_SUCCESS), 'Failure writing array size string: ' // trim(actual_label)) 
   type_format = array_format(type_format, array_size_string) 
#endif
   write(formatted_value, type_format, iostat=io_stat) VAL_ 
   _ASSERT((io_stat == IO_SUCCESS), 'Failure writing formatted_value: ' // trim(actual_label)) 
else 
   do_print = .FALSE. 
end if

#include "MAPL_Resource_postscript.h"
