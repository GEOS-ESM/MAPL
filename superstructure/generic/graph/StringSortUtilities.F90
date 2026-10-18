#include "MAPL.h"

!------------------------------------------------------------------------------
! StringSortUtilities: one shared `sorted_names` helper, extracted from
! two near-identical private copies that had each been independently
! reimplemented (AttributesCharacteristic.F90, VerticalCoordinateCharacteristic.F90)
! because each file's own copy was private to its own module
! (docs/graph/spec/20-implementation-roadmap.md §20.4.19, Phase 19).
! Both call sites use this verbatim in place of their own former private
! copy - no behavior change.
!------------------------------------------------------------------------------
module mapl_StringSortUtilities_mod
   use gFTL2_StringVector, only: StringVector
   implicit none(type, external)
   private

   public :: sorted_names

contains

   ! Simple insertion sort - these call sites' own name counts are
   ! expected to be small (a handful of attribute/dimension names per
   ! item), so this is not performance-sensitive; chosen over pulling in
   ! a generic sort algorithm for this small, local use.
   function sorted_names(names) result(sorted)
      type(StringVector), intent(in) :: names
      character(:), allocatable :: sorted(:)

      integer :: i, j, n
      character(:), allocatable :: key
      integer :: maxlen

      n = names%size()
      maxlen = 0
      do i = 1, n
         maxlen = max(maxlen, len(names%of(i)))
      end do
      allocate(character(len=maxlen) :: sorted(n))
      do i = 1, n
         sorted(i) = names%of(i)
      end do

      do i = 2, n
         key = trim(sorted(i))
         j = i - 1
         do while (j >= 1)
            if (trim(sorted(j)) <= key) exit
            sorted(j + 1) = sorted(j)
            j = j - 1
         end do
         sorted(j + 1) = key
      end do
   end function sorted_names

end module mapl_StringSortUtilities_mod
