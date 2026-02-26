#include "MAPL.h"

module mapl3g_VerticalConservativeMap

   use mapl_ErrorHandling
   use mapl3g_CSR_SparseMatrix, only: SparseMatrix_sp => CSR_SparseMatrix_sp
   use mapl3g_CSR_SparseMatrix, only: add_row
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
   subroutine compute_conservative_map(src_interfaces, dst_interfaces, matrix, rc)
      real(REAL32), intent(in) :: src_interfaces(:)   ! nlev_src + 1
      real(REAL32), intent(in) :: dst_interfaces(:)   ! nlev_dst + 1
      type(SparseMatrix_sp), intent(out) :: matrix
      integer, optional, intent(out) :: rc
      
      integer :: nlev_src, nlev_dst
      integer :: j, k, num_overlaps
      real(REAL32) :: overlap_bot, overlap_top, overlap_thickness
      real(REAL32) :: source_thickness, weight
      real(REAL32), allocatable :: row_weights(:)
      integer, allocatable :: row_indices(:)
      real(REAL32), parameter :: epsilon_sp = tiny(1.0_REAL32)
      integer :: status
      
      nlev_src = size(src_interfaces) - 1
      nlev_dst = size(dst_interfaces) - 1
      
      _ASSERT(nlev_src > 0, "Source must have at least one layer")
      _ASSERT(nlev_dst > 0, "Destination must have at least one layer")
      
      ! Allocate temporary arrays for building each row
      ! Worst case: all source layers overlap with one destination layer
      allocate(row_weights(nlev_src), row_indices(nlev_src))
      
      ! Initialize sparse matrix (estimate ~3 overlaps per destination layer)
      matrix = SparseMatrix_sp(nlev_dst, nlev_src, 3*nlev_dst)
      
      ! For each destination layer
      do j = 1, nlev_dst
         num_overlaps = 0
         
         ! Find all source layers that overlap with this destination layer
         do k = 1, nlev_src
            ! Compute overlap interval (works for both increasing and decreasing coords)
            overlap_bot = max(min(dst_interfaces(j), dst_interfaces(j+1)), &
                             min(src_interfaces(k), src_interfaces(k+1)))
            overlap_top = min(max(dst_interfaces(j), dst_interfaces(j+1)), &
                             max(src_interfaces(k), src_interfaces(k+1)))
            
            overlap_thickness = abs(overlap_top - overlap_bot)
            
            ! If there's an overlap
            if (overlap_thickness > epsilon_sp) then
               source_thickness = abs(src_interfaces(k+1) - src_interfaces(k))
               
               _ASSERT(source_thickness > epsilon_sp, "Source layer has zero thickness")
               
               ! Weight = fraction of source layer that overlaps with dest layer
               weight = overlap_thickness / source_thickness
               
               num_overlaps = num_overlaps + 1
               row_indices(num_overlaps) = k
               row_weights(num_overlaps) = weight
            end if
         end do
         
         _ASSERT(num_overlaps > 0, "Destination layer has no overlapping source layers")
         
         ! Add this row to the sparse matrix
         call add_row(matrix, j, row_indices(1), row_weights(1:num_overlaps))
      end do
      
      deallocate(row_weights, row_indices)
      
      _RETURN(_SUCCESS)
   end subroutine compute_conservative_map

end module mapl3g_VerticalConservativeMap
