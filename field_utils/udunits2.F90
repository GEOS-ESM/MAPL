module udunits2mod

   use iso_c_binding, only:   c_char, c_int, c_short, c_double, c_size_t, c_null_char, c_null_ptr, & 
                              c_ptr, c_funptr 
   implicit none

#include "udunits2enumerators.h"

#include "udunits2types.h"
  
#include "udunits2interfaces.h"

contains

   logical true(n, success)
      integer(c_int), intent(in) :: n
      integer, optional, intent(in) :: success
      
      true = merge(n == success, n /= 0, present(success))

   end function true

   character(kind=c_char, len=MAXLEN) &
   function cstring(fstring)
       character(len=*) :: fstring
       
       cstring = fstring // c_null_char

   end function cstring

end module udunits2mod
