module pFIO_ShmemReferenceMod
   use, intrinsic :: iso_c_binding, only: c_ptr
   use pFIO_UtilitiesMod, only: word_size
   use pFIO_AbstractDataReferenceMod
   use mpi

   implicit none

   private

   public :: ShmemReference

   type,extends(AbstractDataReference) :: ShmemReference
      integer :: win      ! used for shared memory
      integer :: InNode_Comm !
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

   function new_ShmemReference(type_kind,shp,InNode_Comm) result(reference)
      type (ShmemReference) :: reference
      integer, intent(in) :: type_kind
      integer, intent(in) :: shp(:)
      integer, intent(in) :: InNode_Comm
      integer :: ierr

      reference%shape = shp
      reference%type_kind = type_kind
      call Mpi_comm_dup(InNode_Comm,reference%InNode_Comm,ierr)

      call reference%allocate()

   end function new_ShmemReference

   integer function get_length(this) result(length)
      class (ShmemReference), intent(in) :: this
      type (c_ptr) :: dummy

      integer :: size_c_ptr

      size_c_ptr = size(transfer(dummy,[1]))
      length = 4 + size_c_ptr + size(this%shape)

   end function get_length

   subroutine serialize(this, buffer)
      class (ShmemReference), intent(in) :: this
      integer, allocatable :: buffer(:)

      allocate(buffer(this%get_length()))

      buffer(1:2) = transfer(this%base_address, [1])
      buffer(3)   = this%type_kind
      buffer(4)   = this%win
      buffer(5)   = this%InNode_Comm
      buffer(6)   = size(this%shape)
      buffer(7:)  = this%shape

   end subroutine serialize

   subroutine deserialize(this, buffer)
      class (ShmemReference), intent(inout) :: this
      integer, intent(in) :: buffer(:)

      integer :: n

      this%base_address = transfer(buffer(1:2), this%base_address)
      this%type_kind    = buffer(3)
      this%win          = buffer(4)
      this%InNode_Comm  = buffer(5)
      n                 = buffer(6)
      this%shape        = buffer(7:7+n-1)

   end subroutine deserialize
      
   subroutine allocate(this)
      class (ShmemReference), intent(inout) :: this
      integer :: n_bytes
      integer(kind=MPI_ADDRESS_KIND) :: windowsize
      integer :: disp_unit,ierr, Total, InNode_Rank
      integer :: n

      n= product(this%shape)
      n_bytes = 4* n * word_size(this%type_kind)

      call MPI_Comm_rank(this%InNode_Comm,InNode_Rank,ierr)

      disp_unit  = 1
      windowsize = 0_MPI_ADDRESS_KIND  
      if (InNode_Rank == 0) windowsize = int(n_bytes,MPI_ADDRESS_KIND)
   
      call MPI_Win_allocate_shared(windowsize, disp_unit, MPI_INFO_NULL, this%InNode_Comm, &
               this%base_address, this%win, ierr)

      if (InNode_Rank /= 0)  then
          call MPI_Win_shared_query(this%win, 0, windowsize, disp_unit, this%base_address,ierr)
      endif

   end subroutine allocate

   subroutine deallocate(this)
      class (ShmemReference), intent(inout) :: this
      integer :: ierr

      call MPI_Win_fence(0, this%win, ierr)
      call MPI_Win_free(this%win,ierr)

   end subroutine deallocate

   subroutine fence(this)
     class(ShmemReference),intent(inout) :: this
     integer :: ierr

     call Mpi_Win_fence(0, this%win, ierr)

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
