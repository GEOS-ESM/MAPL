! Internal umbrella for the MAPL infrastructure/state subdirectory.
! Aggregates leaf modules from state/ for use by other MAPL subdirectories.
module mapl_state_internal

   use mapl_StateGet_mod
   use mapl_StateSet_mod
   use mapl_StateGetPointer_mod
   use mapl_StateGetGeom_mod
   use mapl_StateAddMethod_mod
   use mapl_StateUtils
   use mapl_StateArithmeticParser_mod
   use mapl_StateMask_mod
   use mapl_StateFilter_mod
   use mapl_StateDestroy_mod

   implicit none

end module mapl_state_internal
