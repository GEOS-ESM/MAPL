#include "MAPL.h"

!------------------------------------------------------------------------------
! StringSortUtilities: one shared `sorted_names` helper, used by
! AttributesCharacteristic.F90 and VerticalGridCharacteristic.F90 so
! each does not need its own private copy.
!------------------------------------------------------------------------------
module mapl_StringSortUtilities_mod
   use gFTL2_StringVector, only: StringVector
   implicit none(type, external)
   private

   public :: sorted_names

contains

   ! Simple insertion sort: name counts at these call sites are small (a
   ! handful of attribute/dimension names per item), so it is not
   ! performance-sensitive and a generic sort algorithm is not worth pulling in.
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
