#include "MAPL_Generic.h"

! When generic procedures are available, this package should be
! redesigned.
module mapl3g_CSR_SparseMatrix
   use mapl_KeywordEnforcer
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64
   implicit none (type, external)
   private

#define IDENTITY(x) x
#define CONCAT(a,b) IDENTITY(a)IDENTITY(b)
#define CONCAT3(a,b,c) IDENTITY(a)IDENTITY(b)IDENTITY(c)
#define T(kz) CONCAT(CSR_SparseMatrix_,kz)


   public :: T(sp)
   public :: T(dp)
   public :: matmul
   public :: add_row
   public :: shape

   integer, parameter :: sp = REAL32
   integer, parameter :: dp = REAL64

#define CSR_SPARSEMATRIX(kz)                                    \
   type :: T(kz);                                               \
      private;                                                  \
      integer :: n_rows;                                        \
      integer :: n_columns;                                     \
      integer :: nnz;                                           \
                                                                \
      integer, allocatable :: row_offsets(:);                   \
      integer, allocatable :: run_starts(:);                    \
      integer, allocatable :: run_lengths(:);                   \
      real(kind=kz), allocatable :: v(:);                       \
   end type T(kz)                                              ;\
   interface matmul                                            ;\
      procedure CONCAT3(matmul_vec_,kz,sp)                     ;\
      procedure CONCAT3(matmul_vec_,kz,dp)                     ;\
      procedure CONCAT3(matmul_multi_vec_,kz,sp)               ;\
      procedure CONCAT3(matmul_multi_vec_,kz,dp)               ;\
   end interface matmul                                        ;\
   interface add_row                                           ;\
      procedure CONCAT(add_row_,kz)                            ;\
   end interface add_row                                       ;\
   interface shape                                             ;\
      procedure CONCAT(shape_,kz)                              ;\
   end interface shape                                         ;\
   interface T(kz)                                             ;\
      procedure CONCAT(new_csr_matrix_,kz)                     ;\
   end interface T(kz)

CSR_SPARSEMATRIX(sp)

CSR_SPARSEMATRIX(dp)

contains

#define NEW_CSR_MATRIX(kz)                                                        \
   function CONCAT(new_csr_matrix_,kz)(n_rows, n_columns, nnz) result(mat)       ;\
      type(T(kz)) :: mat                                                         ;\
      integer, intent(in) :: n_rows                                              ;\
      integer, intent(in) :: n_columns                                           ;\
      integer, intent(in) :: nnz                                                 ;\
      mat%n_rows = n_rows                                                        ;\
      mat%n_columns = n_columns                                                  ;\
      mat%nnz = nnz                                                              ;\
      allocate(mat%row_offsets(n_rows+1))                                        ;\
      allocate(mat%run_starts(n_rows))                                           ;\
      allocate(mat%run_lengths(n_rows))                                          ;\
      allocate(mat%v(nnz))                                                       ;\
      mat%row_offsets(1) = 0                                                     ;\
   end function


#define ADD_ROW(kz)                                                  \
   pure subroutine CONCAT(add_row_,kz)(this, row, start_column, v)  ;\
      type(T(kz)), intent(inout) :: this                            ;\
      integer, intent(in) :: row                                    ;\
      integer, intent(in) :: start_column                           ;\
      real(kz), intent(in) :: v(:)                                  ;\
                                                                     \
      associate (n => size(v), offset => this%row_offsets(row))     ;\
                                                                     \
        this%run_lengths(row) = n                                   ;\
        this%run_starts(row) = start_column                         ;\
        this%v(offset+1:offset+n) = v                               ;\
        this%row_offsets(row+1) = offset + n                        ;\
                                                                     \
      end associate                                                 ;\
                                                                     \
   end subroutine

#define SHAPE(kz)                                                    \
   pure function CONCAT(shape_,kz)(A) result(s)                    ;\
      type(T(kz)), intent(in) :: A                                  ;\
      integer :: s(2)                                               ;\
                                                                     \
      s = [A%n_rows, A%n_columns]                                   ;\
   end function

#define MATMUL_VEC(kz,kx)                                            \
   pure function CONCAT3(matmul_vec_,kz,kx)(A, x) result(y)          ;\
      type(T(kz)), intent(in) :: A                                  ;\
      real(kx), intent(in) :: x(:)                                  ;\
      real(kx) :: y(A%n_rows)                                       ;\
                                                                     \
      integer :: i, j                                               ;\
                                                                     \
      do i = 1, A%n_rows                                            ;\
                                                                     \
         y(i) = 0                                                   ;\
         associate (n => A%run_lengths(i))                           ;\
           if (n == 0) cycle                                        ;\
                                                                     \
           associate (j0 => A%run_starts(i))                        ;\
             associate (j1 => j0 + n - 1)                           ;\
                                                                     \
               do j = j0, j1                                        ;\
                  associate (jj => A%row_offsets(i) + (j-j0) + 1)   ;\
                    y(i) = y(i) + A%v(jj) * x(j)                    ;\
                  end associate                                     ;\
               end do                                               ;\
                                                                     \
             end associate                                          ;\
           end associate                                            ;\
                                                                     \
         end associate                                              ;\
      end do                                                        ;\
                                                                     \
   end function

#define MATMUL_MULTI_VEC(kz,kx)                                      \
   pure function CONCAT3(matmul_multi_vec_,kz,kx)(A, x) result(b)   ;\
      type(T(kz)), intent(in) :: A(:)                               ;\
      real(kx), intent(in) :: x(:,:)                                ;\
      real(kx) :: b(size(A,1),A(1)%n_rows)                          ;\
      integer :: i                                                  ;\
      do i = 1, size(A)                                             ;\
         b(i,:) = matmul(A(i), x(i,:))                              ;\
      end do                                                        ;\
   end function

   NEW_CSR_MATRIX(sp)
   ADD_ROW(sp)
   SHAPE(sp)
   MATMUL_VEC(sp,sp)
   MATMUL_VEC(sp,dp)
   MATMUL_MULTI_VEC(sp,sp)
   MATMUL_MULTI_VEC(sp,dp)

   NEW_CSR_MATRIX(dp)
   ADD_ROW(dp)
   SHAPE(dp)
   MATMUL_VEC(dp,sp)
   MATMUL_VEC(dp,dp)
   MATMUL_MULTI_VEC(dp,sp)
   MATMUL_MULTI_VEC(dp,dp)


end module mapl3g_CSR_SparseMatrix
