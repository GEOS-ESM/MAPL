! Thin compatibility wrapper: NCIOMod has moved to base3g/.
! This wrapper re-exports all public symbols under the original name.
! It will be removed once all callers migrate to 'use MAPL'.
module NCIOMod
   use mapl_NCIO
   implicit none
end module NCIOMod
