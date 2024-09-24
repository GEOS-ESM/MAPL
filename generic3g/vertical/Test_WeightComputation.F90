#define I_AM_MAIN
#include "MAPL_Generic.h"

program Test_WeightComputation

   use mapl_ErrorHandling
   use mapl3g_CSR_SparseMatrix, only: SparseMatrix_sp => CSR_SparseMatrix_sp
   use mapl3g_WeightComputation, only: compute_linear_map_fixedlevels_to_fixedlevels
   use mapl3g_WeightComputation, only: apply_linear_map
   use, intrinsic :: iso_fortran_env, only: REAL32

   implicit none

   real(REAL32), allocatable :: src(:), dst(:), fin(:), fout(:)
   ! real(REAL32), allocatable :: matrix(:, :)
   type(SparseMatrix_sp) :: matrix
   integer :: status

   src = [30., 20., 10.]
   dst = [20., 10.]
   call compute_linear_map_fixedlevels_to_fixedlevels(src, dst, matrix, _RC)
   fin = [7., 8., 3.]
   call apply_linear_map(matrix, fin, fout)
   print *, "Expected: [8.0, 3.0]", ", found: ", fout

   src = [30., 20., 10.]
   dst = [25., 15.]
   call compute_linear_map_fixedlevels_to_fixedlevels(src, dst, matrix, _RC)
   fin = [7., 8., 3.]
   call apply_linear_map(matrix, fin, fout)
   print *, "Expected: [7.5, 5.5]", ", found: ", fout

   src = [30., 20., 10.]
   dst = [28., 11.]
   call compute_linear_map_fixedlevels_to_fixedlevels(src, dst, matrix, _RC)
   fin = [7., 8., 3.]
   call apply_linear_map(matrix, fin, fout)
   print *, "Expected: [7.2, 3.5]", ", found: ", fout
end program Test_WeightComputation
