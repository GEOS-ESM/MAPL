subroutine init_tests()
   use MAPL_pFUnit_ThrowMod, only: pFUnit_throw
   use mapl_Throw_mod
   implicit none

   call MAPL_set_throw_method(pfunit_throw)

end subroutine init_tests
