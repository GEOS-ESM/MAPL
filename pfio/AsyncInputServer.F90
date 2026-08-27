#include "MAPL_ErrLog.h"

module pFIO_AsyncInputServerMod
   use mapl_ErrorHandling_mod
   use mapl_Profiler_mod
   use pFIO_ServerThreadMod
   use pFIO_ServerThreadVectorMod
   use pFIO_BaseServerMod

   implicit none
   private

   public :: AsyncInputServer

   type, extends(BaseServer) :: AsyncInputServer
      character(len=:), allocatable :: port_name
   contains
      procedure :: start
   end type AsyncInputServer

   interface AsyncInputServer
      module procedure new_AsyncInputServer
   end interface AsyncInputServer

contains

   function new_AsyncInputServer(comm, port_name, profiler_name, with_profiler, rc) result(s)
      type(AsyncInputServer) :: s
      integer, intent(in) :: comm
      character(*), intent(in) :: port_name
      character(*), optional, intent(in) :: profiler_name
      logical, optional, intent(in) :: with_profiler
      integer, optional, intent(out) :: rc
      integer :: status

      call s%init(comm, port_name, profiler_name=profiler_name, with_profiler=with_profiler, _RC)
      s%port_name = trim(port_name)
      s%threads = ServerThreadVector()
      _RETURN(_SUCCESS)
   end function new_AsyncInputServer

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

      _RETURN(_SUCCESS)
   end subroutine start

end module pFIO_AsyncInputServerMod
