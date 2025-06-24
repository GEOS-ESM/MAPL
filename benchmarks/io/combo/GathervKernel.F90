#include "MAPL_ErrLog.h"

module mapl_GathervKernel
   use mapl_ErrorHandlingMod
   use Kernel_mod
   use mpi
   implicit none
   private

   public :: GathervKernel

   type, extends(Kernel_T) :: GathervKernel
      integer :: n
      integer :: comm
      integer :: rank
      integer :: np
      real, allocatable :: buffer(:)
      integer, allocatable :: displs(:)
      integer, allocatable :: recvcnts(:)
   contains
      procedure :: init
      procedure :: run
   end type GathervKernel

   interface GathervKernel
      procedure new_GathervKernel
   end interface GathervKernel

contains

   function new_GathervKernel(n, comm) result(kernel)
      type(GathervKernel) :: kernel
      integer, intent(in) :: n
      integer, intent(in) :: comm

      kernel%n = n
      kernel%comm = comm


   end function new_GathervKernel

   subroutine init(this, rc)
      class(GathervKernel), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i



      call MPI_Comm_rank(this%comm, this%rank, status)
      __VERIFY(status)
      call MPI_Comm_size(this%comm, this%np, status)
      __VERIFY(status)

      associate (np => this%np, n => this%n)
        allocate(this%buffer(this%n))
        call random_number(this%buffer)
        allocate(this%recvcnts(0:np-1))
        allocate(this%displs(0:np-1))

        __ASSERT(this%n < huge(1)/np, "integer overflow in displs calculation")
        this%recvcnts(:) = this%n
        this%displs(:) = [(this%n*i, i=0,np-1)]
      end associate
      
      __RETURN(__SUCCESS)
   end subroutine init
      


   subroutine run(this, rc)
      class(GathervKernel), intent(in) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      real, allocatable :: global_arr(:,:)

      if (this%rank == 0) then
         allocate(global_arr(this%n, this%np))
      else
         allocate(global_arr(1,1))
      end if

      call MPI_Gatherv( &
           this%buffer, this%n, MPI_REAL,  &
           global_arr, this%recvcnts, this%displs, MPI_REAL, 0, this%comm, status)
      __VERIFY(status)

      deallocate(global_arr)

      __RETURN(__SUCCESS)
   end subroutine run

   
end module mapl_GathervKernel
