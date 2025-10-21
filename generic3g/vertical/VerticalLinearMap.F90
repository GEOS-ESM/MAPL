#include "MAPL.h"

module mapl3g_VerticalLinearMap

   use mapl_ErrorHandling
   use mapl3g_CSR_SparseMatrix, only: SparseMatrix_sp => CSR_SparseMatrix_sp
   use mapl3g_CSR_SparseMatrix, only: SparseMatrix_dp => CSR_SparseMatrix_dp
   use mapl3g_CSR_SparseMatrix, only: add_row
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64

   implicit none
   private

   public :: compute_linear_map

   type :: IndexValuePairR32
      integer :: index = 0
      real(kind=REAL32) :: value_ = 0
   end type IndexValuePairR32

   type :: IndexValuePairR64
      integer :: index = 0
      real(kind=REAL64) :: value_ = 0
   end type IndexValuePairR64

   interface operator(==)
      procedure equal_to_R32
      procedure equal_to_R64
   end interface operator(==)

   interface operator(/=)
      procedure not_equal_to_R32
      procedure not_equal_to_R64
   end interface operator(/=)

   interface compute_linear_map
      module procedure :: compute_linear_map_R32
      module procedure :: compute_linear_map_R64
   end interface compute_linear_map

contains

!===============================================================================
! Compute linear interpolation transformation matrix,
! src*matrix = dst, when regridding (vertical) from src to dst
! NOTE: find_bracket_ below ASSUMEs that src array is monotonic and decreasing

#define COMPUTE_LINEAR_MAP_ compute_linear_map_R32
#define EQUAL_TO_ equal_to_R32
#define NOT_EQUAL_TO_ not_equal_to_R32
#include "VerticalLinearMapProcedure.h"
#undef COMPUTE_LINEAR_MAP_
#undef EQUAL_TO_
#undef NOT_EQUAL_TO_

#define DP_
#define COMPUTE_LINEAR_MAP_ compute_linear_map_R64
#define EQUAL_TO_ equal_to_R64
#define NOT_EQUAL_TO_ not_equal_to_R64
#include "VerticalLinearMapProcedure.h"
#undef COMPUTE_LINEAR_MAP_
#undef EQUAL_TO_
#undef NOT_EQUAL_TO_
#undef DP_

end module mapl3g_VerticalLinearMap
