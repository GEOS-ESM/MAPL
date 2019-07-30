#include "pFIO_ErrLog.h"
#include "unused_dummy.H"

module pFIO_RDMAReferenceMod
   use, intrinsic :: iso_c_binding, only: c_ptr
   use, intrinsic :: iso_fortran_env, only: INT64
   use pFIO_ErrorHandlingMod
   use pFIO_UtilitiesMod
   use pFIO_AbstractDataReferenceMod
   use mpi

   implicit none

   private

   public :: RDMAReference

   type,extends(AbstractDataReference) :: RDMAReference
      integer :: win         
      integer :: comm 
      integer :: mem_rank
      integer(kind=INT64) :: msize_word
      logical :: RDMA_allocated = .false.
   contains
      procedure :: get_length
      procedure :: serialize
      procedure :: deserialize
      procedure :: allocate
      procedure :: deallocate
      procedure :: fence
   end type RDMAReference

   interface RDMAReference
      module procedure new_RDMAReference
   end interface RDMAReference

contains

   function new_RDMAReference(type_kind,msize_word,comm, rank, rc) result(reference)
      type (RDMAReference) :: reference
      integer, intent(in) :: type_kind
      integer(kind=INT64), intent(in) :: msize_word
      integer, intent(in) :: comm
      integer, intent(in) :: rank
      integer, optional, intent(out) :: rc
      integer :: status

      reference%msize_word = msize_word
      reference%type_kind  = type_kind
      call Mpi_comm_dup(Comm,reference%comm,status)
      reference%mem_rank = rank
      call reference%allocate(rc=status)
      _VERIFY(status)
      _RETURN(_SUCCESS)
   end function new_RDMAReference

   integer function get_length(this) result(length)
      class (RDMAReference), intent(in) :: this
      integer(kind=INT64) :: long_dummy
      integer ::  size_msize

      size_msize  = size(transfer(long_dummy,[1]))
      length      = this%get_length_base() + size_msize + &
                    4  ! win, comm, mem_rank, allocated
   end function get_length

   subroutine serialize(this, buffer, rc)
      class (RDMAReference), intent(in) :: this
      integer, allocatable :: buffer(:)
      integer, optional, intent(out) :: rc

      integer, allocatable :: tmp_buff(:)
      integer :: n, status

      if(allocated(buffer)) deallocate(buffer)
      allocate(buffer(this%get_length()))

      call this%serialize_base(tmp_buff, status)
      _VERIFY(status)
      n = this%get_length_base()
      buffer(1:n)  = tmp_buff(:)
      buffer(n+1)  = this%win
      buffer(n+2)  = this%comm
      buffer(n+3)  = this%mem_rank
      buffer(n+4:n+4) = serialize_intrinsic(this%RDMA_allocated)
      buffer(n+5:) = transfer(this%msize_word, [1])
      _RETURN(_SUCCESS)
   end subroutine serialize

   subroutine deserialize(this, buffer, rc)
      class (RDMAReference), intent(inout) :: this
      integer, intent(in) :: buffer(:)
      integer, optional, intent(out) :: rc
      integer(kind=INT64) :: long_dummy
      integer :: size_msize,n, n1, status

      size_msize  = size(transfer(long_dummy,[1]))
      n = size(buffer)
      n1 = n - size_msize + 1
      this%msize_word   = transfer(buffer(n1:n), this%msize_word)
      call  deserialize_intrinsic(buffer(n1-1:n1-1), this%RDMA_allocated)
      call  deserialize_intrinsic(buffer(n1-2:n1-2), this%mem_rank)
      call  deserialize_intrinsic(buffer(n1-3:n1-3), this%comm)
      call  deserialize_intrinsic(buffer(n1-4:n1-4), this%win)
      call  this%deserialize_base(buffer(1:n1-5), rc=status)
      _VERIFY(status)
      _RETURN(_SUCCESS)
   end subroutine deserialize
      
   subroutine allocate(this, rc)
      class (RDMAReference), intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer(kind=MPI_ADDRESS_KIND) :: windowsize
      integer :: disp_unit,status, Rank
      integer(kind=MPI_ADDRESS_KIND) :: n_bytes

      n_bytes =  this%msize_word * 4_MPI_ADDRESS_KIND

      call MPI_Comm_rank(this%comm,Rank,status)

      disp_unit  = word_size(this%type_kind)*4
      windowsize = 0_MPI_ADDRESS_KIND  
      if (Rank == this%mem_rank) windowsize = n_bytes
   
      call MPI_Win_allocate(windowsize, disp_unit, MPI_INFO_NULL, this%comm, &
               this%base_address, this%win, status)
      call MPI_Win_fence(0, this%win, status)
     
      this%RDMA_allocated = .true.
      _RETURN(_SUCCESS)
   end subroutine allocate

   subroutine deallocate(this, rc)
      class (RDMAReference), intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer :: status

      if ( this%RDMA_allocated ) then
         call MPI_Win_fence(0, this%win, status)
         call MPI_Win_free(this%win,status)
      endif

      this%RDMA_allocated = .false.
      _RETURN(_SUCCESS)
   end subroutine deallocate

   subroutine fence(this, rc)
     class(RDMAReference),intent(inout) :: this
     integer, optional, intent(out) :: rc
     integer :: status

     if( this%RDMA_allocated ) then
        call Mpi_Win_fence(0, this%win, status)
     endif
     _RETURN(_SUCCESS)
   end subroutine fence

end module pFIO_RDMAReferenceMod
