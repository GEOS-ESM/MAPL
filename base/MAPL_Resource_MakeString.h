#include "MAPL_Resource_preamble.h"

if(do_print) then
   if (label_is_present) then 

      if(default_is_present) then 
         select type(default) 
         type is(TYPE_) 
            value_is_default = ARE_EQUAL(VALUE_, default) 
         class default 
            _FAIL(MISMATCH_MESSAGE) 
         end select 
      else 
         value_is_default = .FALSE. 
      end if 

   else 

      value_is_default = .TRUE. 

   end if 

   if (.not. (print_nondefault_only .and. value_is_default)) then 
      type_string = TYPE_STRING
      type_format = FMT_
#if defined(IS_ARRAY)
      type_format = array_format(type_format) 
#endif
      formatted_value = EMPTY_STRING
      !wdb FIXME Probably don't want this to cause failure provided value is set.
      _ASSERT(len_trim(type_format) > 0, 'Type format is empty: ')
      write(formatted_value, type_format, iostat=io_stat) VALUE_ 
      !wdb FIXME Probably don't want this to cause failure provided value is set.
      _ASSERT(io_stat == 0, 'Error producing formatted value: ' // trim(actual_label) // trim(formatted_value))
!     if(io_stat /= IO_SUCCESS) formatted_value = EMPTY_STRING
   else 
      do_print = .FALSE. 
   end if
end if

#if defined(TYPE_STRING)
#undef TYPE_STRING
#endif

#if defined(FMT_)
#undef FMT_
#endif

#if defined(ARE_EQUAL)
#undef ARE_EQUAL
#endif

! vim:ft=fortran
