#include "MAPL_Generic.h"
#define I_AM_MAIN

program generic
   use mapl
   implicit none

   integer :: status

   call run_cap('cap.yaml', _RC)

end program generic
