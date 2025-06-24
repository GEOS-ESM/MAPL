#include "MAPL_Resource_preamble.h"

type_format = FMT_

! do_print indicates to print out
if(do_print) then
   ! If the label was found in config, the value in config is used.
   if (label_is_present) then 
      ! Only compare value to default value if the subroutine call included default
      if(default_is_present) then 
         select type(default) 
         type is(TYPE_) 
            value_is_default = ARE_EQUAL(VALUE_, default) 
         class default 
            __FAIL(MISMATCH_MESSAGE) 
         end select 
      else 
         ! There is no default to compare to, so .FALSE.
         value_is_default = .FALSE. 
      end if 
      ! END DEFAULT_CHECK
   else 
      ! If the label wasn't found in config, the default value MUST be used.
      value_is_default = .TRUE. 
   end if
   ! END LABEL_CHECK

   ! Print if print_all OR value==default
   if (.not. (print_nondefault_only .and. value_is_default)) then 
      type_string = TYPE_STRING
#if (TYPENUM == TYPENUM_CHARACTER)
! Character type resources use a simpler function to make the formatted value.
      formatted_value = MAKE_STRING_FUNCTION(VALUE_, MAX_LINE_LENGTH)  
#else
! for all other resource types, make the formatted string with a write statement.
! If value is an array, the format speicifier must include the repeat specification.
#if defined(IS_ARRAY)
      type_format = array_format(type_format) 
#endif
      formatted_value = EMPTY_STRING
      !wdb FIXME Probably don't want this to cause failure provided value is set.
      __ASSERT(len_trim(type_format) > 0, 'Type format is empty: ')
      write(formatted_value, type_format, iostat=io_stat) VALUE_ 
      !wdb FIXME Probably don't want this to cause failure provided value is set.
      __ASSERT(io_stat == 0, 'Error producing formatted value: ' // trim(actual_label) // trim(formatted_value))
#endif
   else 
      ! DonD
      do_print = .FALSE. 
   end if
   ! END PRINT_CHECK
end if
! END OUTER BLOCK
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
