! Thin compatibility wrapper: MAPL_LocStreamMod has moved to base3g/.
! This wrapper re-exports all public symbols under the original name.
! It will be removed in a future PR.
module MAPL_LocStreamMod
   use mapl_LocStreamMod_impl
   implicit none
end module MAPL_LocStreamMod
