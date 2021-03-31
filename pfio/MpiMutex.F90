! Lifted from logger project and renamed from MpiLock.  Tests were not
! brought over, but was tested using MockMpi prototype.

module pFIO_MpiMutexMod
   use mpi
   use iso_fortran_env, only: INT64
   use iso_c_binding, only: c_ptr, c_f_pointer
   implicit none
   private

   public :: MpiMutex

   type :: MpiMutex
      private
      integer :: comm
      integer :: npes
      integer :: rank
      integer :: window
      integer :: pe_locks_type
      type (c_ptr) :: locks_ptr
      logical, allocatable :: local_data(:)
   contains
      procedure :: acquire
      procedure :: release
      procedure :: free_mpi_resources
   end type MpiMutex

   integer, parameter :: LOCK_TAG = 10

   interface MpiMutex
      module procedure new_MpiMutex
   end interface MpiMutex

contains


   function new_MpiMutex(comm) result(lock)
      type (MpiMutex) :: lock
      integer, intent(in) :: comm

      integer :: ierror
      integer(kind=MPI_ADDRESS_KIND) :: sz

      call MPI_Comm_dup(comm, lock%comm, ierror)
      call MPI_Comm_rank(lock%comm, lock%rank, ierror)
      call MPI_Comm_size(lock%comm, lock%npes, ierror)

      ! This type is used to copy the status of locks on other PE's
      ! into a table that can be examined on the local process.
      block
        integer :: blklens(2)
        integer :: displs(2)
        blklens = [lock%rank, lock%npes - lock%rank - 1]
        displs = [0, lock%rank + 1]
        call MPI_Type_indexed(2, blklens, displs, MPI_LOGICAL, lock%pe_locks_type, ierror);
        call MPI_Type_commit(lock%pe_locks_type, ierror)
      end block

      ! Create windows
      if (lock%rank == 0) then

         block
           logical, pointer :: scratchpad(:)
           integer :: sizeof_logical
          
           call MPI_Type_extent(MPI_LOGICAL, sizeof_logical, ierror)
           sz = lock%npes * sizeof_logical
           call MPI_Alloc_mem(sz, MPI_INFO_NULL, lock%locks_ptr, ierror)

           call c_f_pointer(lock%locks_ptr, scratchpad, [lock%npes])
           scratchpad = .false.

           call MPI_Win_create(scratchpad, sz, sizeof_logical, &
                & MPI_INFO_NULL, lock%comm, lock%window, ierror)
         end block

      else ! local window memory is size 0, but have to pass something
         block
           logical :: buffer(1)
           sz = 0
           call MPI_Win_create(buffer, sz, 1, MPI_INFO_NULL, lock%comm, lock%window, ierror)
         end block
      end if

      allocate(lock%local_data(lock%npes-1))

   end function new_MpiMutex



   subroutine acquire(this)
      class (MpiMutex), intent(inout) :: this

      integer :: ierror

      call MPI_Win_lock(MPI_LOCK_EXCLUSIVE, 0, 0, this%window, ierror)
      call MPI_Get(this%local_data, this%npes-1, MPI_LOGICAL, 0, &
           & 0_MPI_ADDRESS_KIND, 1, this%pe_locks_type, this%window, ierror)
      call MPI_Put(.true., 1, MPI_LOGICAL, 0, int(this%rank,kind=MPI_ADDRESS_KIND), &
           & 1, MPI_LOGICAL, this%window, ierror)

      call MPI_Win_unlock(0, this%window, ierror)

      ! Check other processes for holding the lock
      if (any(this%local_data)) then ! wait for signal from process with the lock
         block
           integer :: buffer ! unused
           call MPI_Recv(buffer, 0, MPI_LOGICAL, MPI_ANY_SOURCE, &
                & LOCK_TAG, this%comm, MPI_STATUS_IGNORE, ierror)
         end block
      end if

   end subroutine acquire



   subroutine release(this)
      class (MpiMutex), intent(inout) :: this

      integer :: ierror

      call MPI_Win_lock(MPI_LOCK_EXCLUSIVE, 0, 0, this%window, ierror)
      call MPI_Get(this%local_data, this%npes-1, MPI_LOGICAL, 0, &
           & 0_MPI_ADDRESS_KIND, 1, this%pe_locks_type, this%window, ierror)
      call MPI_Put(.false., 1, MPI_LOGICAL, 0, int(this%rank,kind=MPI_ADDRESS_KIND), &
           & 1, MPI_LOGICAL, this%window, ierror)
      call MPI_Win_unlock(0, this%window, ierror)

      ! who needs the lock next (if anyone)?
      block
        integer :: p, next_rank, buffer
        p = this%rank
        next_rank = -1
        do p = this%rank+1, this%npes-1
           if (this%local_data(p)) then
              next_rank = p
              exit
           end if
        end do
        if (next_rank == -1) then
           do p = 1, this%rank
              if (this%local_data(p)) then
                 next_rank = p-1
                 exit
              end if
           end do
        end if
        
        if (next_rank /= -1) then
           call MPI_Send(buffer, 0, MPI_LOGICAL, next_rank, &
                & LOCK_TAG, this%comm, ierror)
        end if
      end block

   end subroutine release

   subroutine free_mpi_resources(this)
      class (MpiMutex), intent(inout) :: this

      logical, pointer :: scratchpad(:)
      integer :: ierror

      ! Release resources
      call MPI_Type_free(this%pe_locks_type, ierror)
      call MPI_Win_free(this%window, ierror)

      if (this%rank == 0) then
         call c_f_pointer(this%locks_ptr, scratchpad, [this%npes])
         call MPI_Free_mem(scratchpad, ierror)
      end if

   end subroutine free_mpi_resources

end module pFIO_MpiMutexMod
