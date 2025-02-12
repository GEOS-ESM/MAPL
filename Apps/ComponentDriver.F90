#define I_AM_MAIN
#include "MAPL_Generic.h"

program main

   use mapl3g_MaplFramework, only: MAPL_Initialize, MAPL_Finalize
   use esmf

   implicit none

   type(ESMF_HConfig) :: hconfig
   logical :: is_model_pet
   type(ESMF_GridComp), allocatable :: servers(:)
   integer :: status

   call MAPL_Initialize(hconfig, is_model_pet=is_model_pet, servers=servers, _RC)
   ! ...
   call MAPL_Finalize(_RC)

end program main

#undef I_AM_MAIN

