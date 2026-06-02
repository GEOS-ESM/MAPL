! Internal umbrella for the MAPL infrastructure/state subdirectory.
! Aggregates leaf modules from state/ for use by other MAPL subdirectories.
module mapl_state_internal

   use mapl_StateGetImpl_mod
   use mapl_StateSet_mod
   use mapl_StateGetPointerImpl_mod
   use mapl_StateGetGeomImpl_mod
   use mapl_StateAddMethodImpl_mod
   use mapl_StateUtils
   use mapl_StateArithmeticParser_mod
   use mapl_StateMask_mod
   use mapl_StateFilter_mod
   use mapl_StateDestroyImpl_mod

   implicit none

end module mapl_state_internal
