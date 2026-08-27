#include "MAPL_ErrLog.h"

module pFIO_AsyncInputServerMod
   use mapl_ErrorHandling_mod
   use mapl_Profiler_mod
   use pFIO_ServerThreadMod
   use pFIO_ServerThreadVectorMod
   use pFIO_BaseServerMod
   use mpi

   implicit none
   private

   public :: AsyncInputServer

   type, extends(BaseServer) :: AsyncInputServer
      character(len=:), allocatable :: port_name
      integer :: model_comm = MPI_COMM_NULL
      integer :: model_node_comm = MPI_COMM_NULL
      integer :: node_comm = MPI_COMM_NULL
      integer :: node_npes = 0
      integer :: model_npes_on_node = 0
      integer :: reader_capacity_on_node = 0
      logical :: synchronous_fallback = .true.
      contains
       procedure :: start
     end type AsyncInputServer

   interface AsyncInputServer
      module procedure new_AsyncInputServer
   end interface AsyncInputServer

contains

   function new_AsyncInputServer(comm, port_name, model_comm, profiler_name, with_profiler, rc) result(s)
      type(AsyncInputServer) :: s
      integer, intent(in) :: comm
      character(*), intent(in) :: port_name
      integer, optional, intent(in) :: model_comm
      character(*), optional, intent(in) :: profiler_name
      logical, optional, intent(in) :: with_profiler
      integer, optional, intent(out) :: rc
      integer :: status

      s%port_name = trim(port_name)
      s%threads = ServerThreadVector()
      s%model_comm = comm
      if (present(model_comm)) s%model_comm = model_comm
      call s%init(s%model_comm, port_name, profiler_name=profiler_name, with_profiler=with_profiler, _RC)
      call initialize_role_accounting(s, comm, _RC)

      _RETURN(_SUCCESS)
   end function new_AsyncInputServer

   subroutine initialize_role_accounting(this, comm, rc)
      class(AsyncInputServer), intent(inout) :: this
      integer, intent(in) :: comm
      integer, optional, intent(out) :: rc

      integer :: ierror

      call MPI_Comm_split_type(comm, MPI_COMM_TYPE_SHARED, 0, MPI_INFO_NULL, this%node_comm, ierror)
      _VERIFY(ierror)
      call MPI_Comm_split_type(this%model_comm, MPI_COMM_TYPE_SHARED, 0, MPI_INFO_NULL, this%model_node_comm, ierror)
      _VERIFY(ierror)
      call MPI_Comm_size(this%model_node_comm, this%model_npes_on_node, ierror)
      _VERIFY(ierror)
      call MPI_Comm_size(this%node_comm, this%node_npes, ierror)
      _VERIFY(ierror)

      this%reader_capacity_on_node = this%node_npes - this%model_npes_on_node
      _ASSERT(this%reader_capacity_on_node >= 0, 'reader_capacity_on_node must be non-negative')
      this%synchronous_fallback = (this%reader_capacity_on_node == 0)

      if (this%InNode_Rank == 0) then
         write(*,'(A,1X,A,1X,A,I0,1X,A,I0,1X,A,I0,1X,A,L1)') &
              'INFO: AsyncInputServer:', trim(this%port_name), &
              'model_size_on_node=', this%model_npes_on_node, &
              'node_size=', this%node_npes, &
              'reader_capacity_on_node=', this%reader_capacity_on_node, &
              'synchronous_fallback=', this%synchronous_fallback
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(comm)
   end subroutine initialize_role_accounting

   subroutine start(this, rc)
      class(AsyncInputServer), target, intent(inout) :: this
      integer, optional, intent(out) :: rc
      class(ServerThread), pointer :: thread_ptr => null()
      integer :: i, client_size
      logical, allocatable :: mask(:)
      integer :: status

      client_size = this%threads%size()

      allocate(this%serverthread_done_msgs(client_size))
      this%serverthread_done_msgs(:) = .false.

      allocate(mask(client_size))
      mask = .false.
      do while (.true.)

         do i = 1, client_size

            if (mask(i)) cycle

            thread_ptr => this%threads%at(i)
            call thread_ptr%run(_RC)
            if (thread_ptr%do_terminate()) then
               mask(i) = .true.
            end if
         end do

         if (all(mask)) exit

      end do

      call this%threads%clear()
      deallocate(mask)

      call this%report_profile(_RC)

      if (this%model_node_comm /= MPI_COMM_NULL) then
         call MPI_Comm_free(this%model_node_comm, status)
         _VERIFY(status)
         this%model_node_comm = MPI_COMM_NULL
      end if
      if (this%node_comm /= MPI_COMM_NULL) then
         call MPI_Comm_free(this%node_comm, status)
         _VERIFY(status)
         this%node_comm = MPI_COMM_NULL
      end if

      _RETURN(_SUCCESS)
   end subroutine start

end module pFIO_AsyncInputServerMod
