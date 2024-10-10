#define I_AM_MAIN
#include "MAPL_Generic.h"

program Test_VerticalLinearMap

   use mapl_ErrorHandling
   use mapl3g_CSR_SparseMatrix, only: SparseMatrix_sp => CSR_SparseMatrix_sp, matmul
   use mapl3g_VerticalLinearMap, only: compute_linear_map
   use mapl3g_tmp, only: compute_centered_var_from_edge
   ! use mapl3g_VerticalLinearMap, only: apply_linear_map
   use, intrinsic :: iso_fortran_env, only: REAL32

   implicit none

   real(REAL32), allocatable :: src(:), dst(:), fin(:)
   real(REAL32), allocatable :: edge(:), centered(:)
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

   allocate(edge(1:4), source=[60., 50., 40., 30.])
   call compute_centered_var_from_edge(edge, centered)
   print *, "edge var: ", edge
   print *, "centered var: ", centered
   print *, "centered var bounds: ", [lbound(centered), ubound(centered)]

   deallocate(edge)
   allocate(edge(0:3), source=[100., 90., 70., 30.])
   call compute_centered_var_from_edge(edge, centered)
   print *, "edge var: ", edge
   print *, "centered var: ", centered
   print *, "centered var bounds: ", [lbound(centered), ubound(centered)]

end program Test_VerticalLinearMap
