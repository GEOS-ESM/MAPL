#include "pFIO_ErrLog.h"
#include "unused_dummy.H"

module pFIO_ShmemReferenceMod
   use, intrinsic :: iso_c_binding, only: c_ptr
   use, intrinsic :: iso_fortran_env, only: INT64
   use pFIO_ErrorHandlingMod
   use pFIO_UtilitiesMod
   use pFIO_AbstractDataReferenceMod
   use mpi

   implicit none

   private

   public :: ShmemReference

   type,extends(AbstractDataReference) :: ShmemReference
      integer :: win         ! used for shared memory
      integer :: InNode_Comm !
      integer(kind=INT64) :: msize_word
      logical :: shmem_allocated = .false.
   contains
      procedure :: get_length
      procedure :: serialize
      procedure :: deserialize
      procedure :: allocate
      procedure :: deallocate
      procedure :: fence
   end type ShmemReference

   interface ShmemReference
      module procedure new_ShmemReference
   end interface ShmemReference

contains

   function new_ShmemReference(type_kind,msize_word,InNode_Comm, rc) result(reference)
      type (ShmemReference) :: reference
      integer, intent(in) :: type_kind
      integer(kind=INT64), intent(in) :: msize_word
      integer, intent(in) :: InNode_Comm
      integer, optional, intent(out) :: rc
      integer :: status

      reference%msize_word = msize_word
      reference%type_kind  = type_kind
      call Mpi_comm_dup(InNode_Comm,reference%InNode_Comm,status)

      call reference%allocate(rc=status)
      _VERIFY(status)
      _RETURN(_SUCCESS)
   end function new_ShmemReference

   integer function get_length(this) result(length)
      class (ShmemReference), intent(in) :: this
      integer(kind=INT64) :: long_dummy
      integer :: size_msize

      size_msize  = size(transfer(long_dummy,[1]))
      length = this%get_length_base() + size_msize + &
                    3 ! win + InNode_comm + Shmem_alloacted

   end function get_length

   subroutine serialize(this, buffer,rc)
      class (ShmemReference), intent(in) :: this
      integer, allocatable :: buffer(:)
      integer, optional, intent(out) :: rc

      integer, allocatable :: tmp_buff(:)
      integer :: n, status

      if(allocated(buffer)) deallocate(buffer)
      allocate(buffer(this%get_length()))
      
      call this%serialize_base(tmp_buff, rc=status)
      _VERIFY(status)
      n = this%get_length_base()
      buffer(1:n)  = tmp_buff(:)
      buffer(n+1)  = this%win
      buffer(n+2)  = this%InNode_Comm
      buffer(n+3:n+3) = serialize_intrinsic(this%shmem_allocated)
      buffer(n+4:) = transfer(this%msize_word, [1])
      _RETURN(_SUCCESS)
   end subroutine serialize

   subroutine deserialize(this, buffer, rc)
      class (ShmemReference), intent(inout) :: this
      integer, intent(in) :: buffer(:)
      integer, optional, intent(out) :: rc
      integer(kind=INT64) :: long_dummy
      integer :: size_msize,n, n1, status

      size_msize  = size(transfer(long_dummy,[1]))
      n = size(buffer)
      n1 = n - size_msize + 1
      this%msize_word   = transfer(buffer(n1:n), this%msize_word)
      call  deserialize_intrinsic(buffer(n1-1:n1-1), this%shmem_allocated)
      call  deserialize_intrinsic(buffer(n1-2:n1-2), this%InNode_Comm)
      call  deserialize_intrinsic(buffer(n1-3:n1-3), this%win)
      call  this%deserialize_base(buffer(1:n1-4), rc=status)
      _VERIFY(status)
      _RETURN(_SUCCESS)
   end subroutine deserialize
      
   subroutine allocate(this, rc)
      class (ShmemReference), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer(kind=MPI_ADDRESS_KIND) :: windowsize
      integer :: disp_unit,ierr, InNode_Rank
      integer(kind=MPI_ADDRESS_KIND) :: n_bytes

      n_bytes =  this%msize_word * 4_MPI_ADDRESS_KIND

      call MPI_Comm_rank(this%InNode_Comm,InNode_Rank,ierr)

      disp_unit  = 1
      windowsize = 0_MPI_ADDRESS_KIND  
      if (InNode_Rank == 0) windowsize = n_bytes
   
      call MPI_Win_allocate_shared(windowsize, disp_unit, MPI_INFO_NULL, this%InNode_Comm, &
               this%base_address, this%win, ierr)

      if (InNode_Rank /= 0)  then
          call MPI_Win_shared_query(this%win, 0, windowsize, disp_unit, this%base_address,ierr)
      endif
     
      this%shmem_allocated = .true.
      _RETURN(_SUCCESS)
   end subroutine allocate

   subroutine deallocate(this, rc)
      class (ShmemReference), intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer :: ierr

      if (.not. this%shmem_allocated)  then
         _RETURN(_SUCCESS)
      endif

      call MPI_Win_fence(0, this%win, ierr)
      call MPI_Win_free(this%win,ierr)

      this%shmem_allocated = .false.
      _RETURN(_SUCCESS)
   end subroutine deallocate

   subroutine fence(this, rc)
     class(ShmemReference),intent(inout) :: this
      integer, optional, intent(out) :: rc
     integer :: ierr

     if(.not. this%shmem_allocated) then
        _RETURN(_SUCCESS)
     endif
     call Mpi_Win_fence(0, this%win, ierr)
      _RETURN(_SUCCESS)
   end subroutine fence

end module pFIO_ShmemReferenceMod

module pFIO_IntegerShmemReferenceMapMod
   use pFIO_ShmemReferenceMod

#include "types/key_integer.inc"
#define _value type(ShmemReference)

#define _map IntegerShmemReferenceMap
#define _iterator IntegerShmemReferenceMapIterator
#define _alt
#include "templates/map.inc"

end module pFIO_IntegerShmemReferenceMapMod
