#include "MAPL_Resource_preamble.h"

#if defined(ARE_EQUAL)
! AE = ARE_EQUAL
#endif

if(do_print) then
    if (label_is_present) then 

       if(default_is_present) then 
          select type(default) 
          type is(TYPE_) 
             value_is_default = ARE_EQUAL(VAL_, default) 
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
       type_string = TYPE_STRING_
       type_format = FMT_
#if defined(IS_ARRAY)
       write(array_size_string, '(i2)', iostat=io_stat) size(VAL_) 
       _ASSERT((io_stat == IO_SUCCESS), "Failure writing array size string") 
       type_format = array_format(type_format, array_size_string) 
#endif
       write(formatted_value, type_format, iostat=io_stat) VAL_ 
       _ASSERT((io_stat == IO_SUCCESS), "Failure writing formatted_value") 
    else 
       do_print = .FALSE. 
    end if
end if

#if defined(IS_ARRAY)
#undef IS_ARRAY
#endif

#if defined(TYPE_STRING_)
#undef TYPE_STRING_
#endif

#if defined(FMT_)
#undef FMT_
#endif

#if defined(ARE_EQUAL)
#undef ARE_EQUAL
#endif
