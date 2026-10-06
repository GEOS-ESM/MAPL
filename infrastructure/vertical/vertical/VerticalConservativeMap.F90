#include "MAPL.h"

module mapl_VerticalConservativeMap_mod

   use mapl_ErrorHandling_mod
   use mapl_CSR_SparseMatrix_mod, only: SparseMatrix_sp => CSR_SparseMatrix_sp
   use mapl_CSR_SparseMatrix_mod, only: add_row
   use, intrinsic :: iso_fortran_env, only: REAL32

   implicit none(type,external)
   private

   public :: compute_conservative_map

contains

   !> Compute conservative vertical regridding transformation matrix using overlap method
   !!
   !! For mass-conserving regridding, compute weights based on layer overlap fractions.
   !! Each destination layer receives contributions from source layers weighted by
   !! the fraction of overlap.
   !!
   !! @param src_interfaces  Source layer interfaces (edges), size nlev_src + 1
   !! @param dst_interfaces  Destination layer interfaces (edges), size nlev_dst + 1
   !! @param matrix          Output sparse transformation matrix
   !! @param rc              Return code
   !!
   !! Conservation property: sum of weights in each row = 1.0
   !! Usage: dst_field = matmul(matrix, src_field)
   ! Conservative vertical regridding with strict validation
   ! Requires is_monotonic() function from linear regridding module

   subroutine compute_conservative_map(src_interfaces, dst_interfaces, matrix, rc)
      real(REAL32), intent(in) :: src_interfaces(:)   ! nlev_src + 1
      real(REAL32), intent(in) :: dst_interfaces(:)   ! nlev_dst + 1
      type(SparseMatrix_sp), intent(out) :: matrix
      integer, optional, intent(out) :: rc

      integer :: nlev_src, nlev_dst
      integer :: j, k
      real(REAL32) :: overlap_bot, overlap_top, overlap_thickness
      real(REAL32) :: dest_thickness
      real(REAL32), allocatable :: row_weights(:)
      real(REAL32), parameter :: epsilon_sp = tiny(1.0_REAL32)
      real(REAL32), parameter :: tolerance = 1.0e-5_REAL32  ! For conservation check
      real(REAL32) :: row_sum, range_tol
      integer :: status

      nlev_src = size(src_interfaces) - 1
      nlev_dst = size(dst_interfaces) - 1

      ! Basic size validation
      _ASSERT(nlev_src > 0, "Source must have at least one layer")
      _ASSERT(nlev_dst > 0, "Destination must have at least one layer")

#ifndef NDEBUG
      ! Validate monotonicity of interfaces
      _ASSERT(is_monotonic(src_interfaces), "Source interfaces are not monotonic")
      _ASSERT(is_monotonic(dst_interfaces), "Destination interfaces are not monotonic")

      ! Strict validation: destination range must be within source range
      ! This ensures conservative regridding without extrapolation
      ! Allow round-off level differences (relative to coordinate magnitude)
      range_tol = 100.0_REAL32 * epsilon(1.0_REAL32) * &
           max(maxval(abs(src_interfaces)), maxval(abs(dst_interfaces)))
      _ASSERT(minval(dst_interfaces) >= minval(src_interfaces) - range_tol, "Destination extends below source domain")
      _ASSERT(maxval(dst_interfaces) <= maxval(src_interfaces) + range_tol, "Destination extends above source domain")

      ! Validate no zero-thickness source layers
      do k = 1, nlev_src
         _ASSERT(abs(src_interfaces(k+1) - src_interfaces(k)) > epsilon_sp, "Source layer has zero thickness")
      end do

      ! Validate no zero-thickness destination layers
      do j = 1, nlev_dst
         _ASSERT(abs(dst_interfaces(j+1) - dst_interfaces(j)) > epsilon_sp, "Destination layer has zero thickness")
      end do
#endif

      ! Allocate temporary array for building each row
      allocate(row_weights(nlev_src), stat=status)
      _VERIFY(status)

      ! Initialize sparse matrix - each row may contain up to nlev_src columns
      matrix = SparseMatrix_sp(nlev_dst, nlev_src, nlev_dst*nlev_src)

      ! For each destination layer
      do j = 1, nlev_dst
         ! Initialize this row's weights to zero for all source layers
         row_weights(:) = 0.0_REAL32

         ! Compute destination layer thickness
         dest_thickness = abs(dst_interfaces(j+1) - dst_interfaces(j))

         ! Find all source layers that overlap with this destination layer
         do k = 1, nlev_src
            ! Compute overlap interval (works for both increasing and decreasing coords)
            ! overlap_bot is the maximum of the two bottom interfaces
            ! overlap_top is the minimum of the two top interfaces
            overlap_bot = max(min(dst_interfaces(j), dst_interfaces(j+1)), &
                             min(src_interfaces(k), src_interfaces(k+1)))
            overlap_top = min(max(dst_interfaces(j), dst_interfaces(j+1)), &
                             max(src_interfaces(k), src_interfaces(k+1)))

            ! Check if there's actually an overlap (overlap_top must be > overlap_bot)
            if (overlap_top > overlap_bot + epsilon_sp) then
               overlap_thickness = overlap_top - overlap_bot

               ! Weight = fraction of destination layer covered by this overlap
               ! This ensures row sums = 1.0 for conservative (intensive) regridding
               row_weights(k) = overlap_thickness / dest_thickness
            end if
         end do

#ifndef NDEBUG
         ! Verify conservation: row weights should sum to 1.0
         ! This is a critical check for conservative regridding
         row_sum = sum(row_weights)
         _ASSERT(abs(row_sum - 1.0_REAL32) < tolerance, "Row weights do not sum to 1.0 (conservation violated)")
#endif

         ! Add this row to the sparse matrix (all columns, starting from column 1)
         call add_row(matrix, j, 1, row_weights(1:nlev_src))
      end do

      deallocate(row_weights, stat=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)
   end subroutine compute_conservative_map

   ! Helper function to check if array is monotonic (increasing or decreasing)
   ! This should be shared with the linear regridding module
   logical function is_monotonic(array)
      real(REAL32), intent(in) :: array(:)

      is_monotonic = is_increasing(array) .or. is_decreasing(array)
   end function is_monotonic

   ! Check if array is monotonically increasing
   logical function is_increasing(array)
      real(REAL32), intent(in) :: array(:)
      integer :: i

      is_increasing = .true.
      do i = 1, size(array) - 1
         if (array(i) >= array(i+1)) then
            is_increasing = .false.
            exit
         end if
      end do
   end function is_increasing

   ! Check if array is monotonically decreasing
   logical function is_decreasing(array)
      real(REAL32), intent(in) :: array(:)
      integer :: i

      is_decreasing = .true.
      do i = 1, size(array) - 1
         if (array(i) <= array(i+1)) then
            is_decreasing = .false.
            exit
         end if
      end do
   end function is_decreasing

end module mapl_VerticalConservativeMap_mod
