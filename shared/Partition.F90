#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"

module mapl_Partition
   use mapl_KeywordEnforcerMod
   use mapl_ErrorHandlingMod
   implicit none(type,external)
   private

   public :: mapl_GetPartition

   interface mapl_GetPartition
      procedure :: get_partition
   end interface


contains

   ! Return a partition of n items split among k bins. Typically to
   ! support balannced domain decomposition.
   ! 
   ! Options:
   !
   !    symmetric (logical) - attempt to impose mirror symmetry on the
   !                          partition (soft constraint).  Will fudge for
   !                          even partition of odd number of items.
   !
   !    min_extent - mininumum # of items to place in any *non-empty*
   !                 bin. Supports ESMF use cases where DEs must have
   !                 at least an extent of 2.
   !
   recursive function get_partition(n, k, unusable, symmetric, min_extent, rc) result(partition)
      integer, intent(in) :: n
      integer, intent(in) :: k
      class (KeywordEnforcer), optional, intent(in) :: unusable
      logical, intent(in), optional :: symmetric
      integer, optional, intent(in) :: min_extent
      integer, optional, intent(out) :: rc
      ! result
      integer :: partition(1:k)

      integer :: k_used
      integer :: im, remainder
      integer :: i_mid
      logical :: symmetric_
      integer :: min_extent_
      integer :: status
      integer, allocatable :: sub_partition_a(:), sub_partition_b(:)

      _ASSERT(n >= 0, 'n must be non-negative')
      _ASSERT(k >= 0, 'k must be non-negative')
      if (n == 0) then
         _ASSERT(k == 0, 'k cannot be 0 unless n is zero')
      end if

      min_extent_ = 0
      if (present(min_extent)) min_extent_ = min_extent
      _ASSERT(min_extent_ <= n, 'min_extent cannot be larger than n')

      k_used = k
      if (min_extent_ > 0)  then
         k_used = min(k, n / min_extent_)
      end if
      partition(k_used+1:) = 0

      im = n/ k_used ! average number of items per bin - rounded down

      symmetric_ = .false.
      if (present(symmetric)) symmetric_ = symmetric

      if (.not. symmetric_) then
         partition(1:k_used) = im
         remainder = n - k_used * im
         partition(:remainder) = partition(:remainder) + 1
         _RETURN(_SUCCESS)
      end if

      ! Symmetric cases
      if (is_even(k)) then
         sub_partition_a = get_partition((n+1)/2, k/2, min_extent=min_extent, _RC)
         sub_partition_b = get_partition(n/2, k/2, min_extent=min_extent, _RC)
         partition = [sub_partition_a, sub_partition_b(k/2:1:-1)]
         _RETURN(_SUCCESS)
      end if

      ! k in odd
      ! We must ensure that the middle bin count leaves an even number
      ! to be split across the 2 halves.
      if (is_even(n)) then ! i_mid must be even
         i_mid = make_even(im)
         if (i_mid < min_extent_) then ! just set to zero
            i_mid = 0
         end if
      else ! i_mid must be odd
         i_mid = make_odd(im)
         if (i_mid < min_extent_) then
            ! Cannot set to 0 due to parity constraint, so must be at least min_extent
            i_mid = make_odd(min_extent_)
         end if
      end if

      sub_partition_a = get_partition((n-i_mid)/2, (k-1)/2, min_extent=min_extent_, _RC) ! first half
      partition = [sub_partition_a, i_mid, sub_partition_a((k-1)/2:1:-1) ]

      _RETURN(_SUCCESS)

   contains

      pure logical function is_even(n)
         integer, intent(in) :: n
         is_even = (mod(n,2) == 0)
      end function is_even

      pure integer function make_even(n) result(n_even)
         integer, intent(in) :: n
         n_even = n + mod(n,2)
      end function make_even

      pure integer function make_odd(n) result(n_even)
         integer, intent(in) :: n
         n_even = n + mod(n+1,2)
      end function make_odd

   end function get_partition
   
end module mapl_Partition
