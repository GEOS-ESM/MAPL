#include "MAPL_Generic.h"

! When generic procedures are available, this package should be
! redesigned.
module mapl3g_CSR_SparseMatrix
   use mapl_KeywordEnforcer
   use, intrinsic :: iso_fortran_env, only: REAL32
   implicit none (type, external)
   private

#define IDENTITY(x) x
#define CONCAT(a,b) IDENTITY(a)IDENTITY(b)
#define T(k,suffix) CONCAT(CSR_SparseMatrix,suffix)


   public :: T(REAL32,_sp)
   public :: matmul
   public :: add_row

#define CSR_SPARSEMATRIX(k,suffix)                              \
   type :: T(k,suffix);                                         \
      private;                                                  \
      integer :: n_rows;                                        \
      integer :: n_columns;                                     \
      integer :: nnz;                                           \
                                                                \
      integer, allocatable :: row_offsets(:);                   \
      integer, allocatable :: run_starts(:);                    \
      integer, allocatable :: run_lengths(:);                   \
      real(kind=k), allocatable :: v(:);                        \
   end type T(k,suffix)                                        ;\
   \
   interface matmul                                            ;\
      procedure CONCAT(matmul_vec,suffix)                     ;\
      procedure CONCAT(matmul_multi_vec,suffix)               ;\
   end interface matmul                                        ;\
   \
   interface add_row                                           ;\
      procedure :: CONCAT(add_row,suffix)                     ;\
   end interface add_row                                       ;\
   \
   interface T(k,suffix)                                       ;\
      procedure CONCAT(new_csr_matrix,suffix)                  ;\
   end interface                                               ;\
                                                               ;\

CSR_SPARSEMATRIX(REAL32,_sp)   

contains

#define NEW_CSR_MATRIX(k,suffix)                                                  \
   function CONCAT(new_csr_matrix,suffix)(n_rows, n_columns, nnz) result(mat)    ;\
      type(T(k,suffix)) :: mat                                                   ;\
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


#define ADD_ROW(k,suffix)                                           \
   pure subroutine add_row_sp(this, row, start_column, v)           ;\
      type(T(k,suffix)), intent(inout) :: this                      ;\
      integer, intent(in) :: row                                    ;\
      integer, intent(in) :: start_column                           ;\
      real(k), intent(in) :: v(:)                                   ;\
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


#define MATMUL_VEC(k,suffix)                                         \
   pure function CONCAT(matmul_vec,suffix)(A, x) result(y)          ;\
      type(T(k,suffix)), intent(in) :: A                            ;\
      real(k), intent(in) :: x(:)                                   ;\
      real(k) :: y(A%n_rows)                                        ;\
                                                                     \
      integer :: i, j                                               ;\
                                                                     \
      do concurrent (i = 1:A%n_rows)                                ;\
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

#define MATMUL_MULTI_VEC(k,suffix)                                   \
   pure function CONCAT(matmul_multi_vec,suffix)(A, x) result(b)    ;\
      type(T(k,suffix)), intent(in) :: A(:)                         ;\
      real(k), intent(in) :: x(:,:)                                 ;\
      real(k) :: b(size(A,1),A(1)%n_rows)                           ;\
      integer :: i                                                  ;\
      do concurrent (i=1:size(A))                                   ;\
         b(i,:) = matmul(A(i), x(i,:))                              ;\
      end do                                                        ;\
   end function

   NEW_CSR_MATRIX(REAL32,_sp)
   ADD_ROW(REAL32,_sp)
   MATMUL_VEC(REAL32,_sp)
   MATMUL_MULTI_VEC(REAL32,_sp)

end module mapl3g_CSR_SparseMatrix
