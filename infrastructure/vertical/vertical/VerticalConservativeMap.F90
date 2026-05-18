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
       integer :: j, k
       real(REAL32) :: overlap_bot, overlap_top, overlap_thickness
       real(REAL32) :: dest_thickness
       real(REAL32), allocatable :: row_weights(:)
       real(REAL32), parameter :: epsilon_sp = tiny(1.0_REAL32)
       integer :: status
      
      nlev_src = size(src_interfaces) - 1
      nlev_dst = size(dst_interfaces) - 1
      
      _ASSERT(nlev_src > 0, "Source must have at least one layer")
      _ASSERT(nlev_dst > 0, "Destination must have at least one layer")
      
      ! Allocate temporary array for building each row
      allocate(row_weights(nlev_src))
      
      ! Initialize sparse matrix - each row will contain all nlev_src columns
      matrix = SparseMatrix_sp(nlev_dst, nlev_src, nlev_dst*nlev_src)
      
       ! For each destination layer
       do j = 1, nlev_dst
          ! Initialize this row's weights to zero for all source layers
          row_weights(:) = 0.0
          
          ! Compute destination layer thickness
          dest_thickness = abs(dst_interfaces(j+1) - dst_interfaces(j))
          _ASSERT(dest_thickness > epsilon_sp, "Destination layer has zero thickness")
          
          ! Find all source layers that overlap with this destination layer
          do k = 1, nlev_src
             ! Compute overlap interval (works for both increasing and decreasing coords)
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
          
          ! Add this row to the sparse matrix (all columns, starting from column 1)
          call add_row(matrix, j, 1, row_weights(1:nlev_src))
       end do
      
      deallocate(row_weights)
      
      _RETURN(_SUCCESS)
   end subroutine compute_conservative_map

end module mapl3g_VerticalConservativeMap
