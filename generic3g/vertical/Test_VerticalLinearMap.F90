#define I_AM_MAIN
#include "MAPL_Generic.h"

program Test_VerticalLinearMap

   use mapl_ErrorHandling
   use mapl3g_CSR_SparseMatrix, only: SparseMatrix_sp => CSR_SparseMatrix_sp, matmul
   use mapl3g_VerticalLinearMap, only: compute_linear_map
   ! use mapl3g_VerticalLinearMap, only: apply_linear_map
   use, intrinsic :: iso_fortran_env, only: REAL32

   implicit none

   real(REAL32), allocatable :: src(:), dst(:), fin(:)
   ! real(REAL32), allocatable :: matrix(:, :)
   type(SparseMatrix_sp) :: matrix
   integer :: status

   src = [30., 20., 10.]
   dst = [20., 10.]
   call compute_linear_map(src, dst, matrix, _RC)
   fin = [7., 8., 3.]
   print *, "Expected: [8.0, 3.0]", ", found: ", matmul(matrix, fin)

   src = [30., 20., 10.]
   dst = [25., 15.]
   call compute_linear_map(src, dst, matrix, _RC)
   fin = [7., 8., 3.]
   print *, "Expected: [7.5, 5.5]", ", found: ", matmul(matrix, fin)

   src = [30., 20., 10.]
   dst = [28., 11.]
   call compute_linear_map(src, dst, matrix, _RC)
   fin = [7., 8., 3.]
   print *, "Expected: [7.2, 3.5]", ", found: ", matmul(matrix, fin)

end program Test_VerticalLinearMap
