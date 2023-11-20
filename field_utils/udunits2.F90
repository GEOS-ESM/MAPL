module udunits2mod

   ! The kinds and derived types that follow are needed for the following include files.
   use iso_c_binding, only:   c_char, c_int, c_short, c_double, c_size_t, c_null_char, c_null_ptr, & 
                              c_ptr, c_funptr 
   implicit none

#include "udunits2enumerators.h"
#include "udunits2types.h"
#include "udunits2interfaces.h"

end module udunits2mod
