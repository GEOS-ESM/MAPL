#define I_AM_MAIN
#include "MAPL_Generic.h"

program Test_WeightComputation

   use mapl_ErrorHandling
   use mapl3g_VerticalRegridMethod
   use mapl3g_CSR_SparseMatrix
   use mapl3g_WeightComputation
   use, intrinsic :: iso_fortran_env, only: REAL32

   implicit none

   ! type(CSR_SparseMatrix_sp) :: weights
   real(REAL32), allocatable :: vcoord_src(:), vcoord_dst(:), result(:)
   real(REAL32), allocatable :: weights(:, :)
   integer :: status
   
   vcoord_src = [50., 40., 30., 20., 10.]
   vcoord_dst = [40., 20., 10.]
   call get_weights_fixedlevels_subset(vcoord_src, vcoord_dst, VERTICAL_REGRID_SUBSET, weights, _RC)
   print *, "weights: ", weights
   result = matmul(weights, vcoord_src)

   print *, ""
   print *, "vcoord_dst: ", vcoord_dst
   print *, "result: ", result

end program Test_WeightComputation
