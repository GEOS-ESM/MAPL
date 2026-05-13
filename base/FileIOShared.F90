! Thin compatibility wrapper: FileIOSharedMod has moved to base3g/.
! This wrapper re-exports all public symbols under the original name.
! It will be removed once all callers migrate to 'use MAPL'.
module FileIOSharedMod
   use mapl_FileIOShared
   implicit none
end module FileIOSharedMod
