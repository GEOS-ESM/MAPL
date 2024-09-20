#define I_AM_MAIN
#include "MAPL_Generic.h"

program Test_WeightComputation

   use mapl_ErrorHandling
   use mapl3g_CSR_SparseMatrix
   use mapl3g_WeightComputation, only: get_weights_fixedlevels_to_fixedlevels_linear
   use, intrinsic :: iso_fortran_env, only: REAL32

   implicit none

   ! type(CSR_SparseMatrix_sp) :: weights
   real(REAL32), allocatable :: src(:), dst(:)
   real(REAL32), allocatable :: weights(:, :)
   integer :: status

   src = [40., 30., 20., 10.]
   dst = [40., 32., 38., 25., 21., 13., 10.]
   call get_weights_fixedlevels_to_fixedlevels_linear(src, dst, weights, _RC)
   print *, "dst:    ", dst
   print *, "result: ", matmul(weights, src)

end program Test_WeightComputation
