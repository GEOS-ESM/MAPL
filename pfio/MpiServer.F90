#include "MAPL_ErrLog.h"

module pFIO_MpiServerMod
   use MAPL_ExceptionHandling
   use MAPL_Profiler
   use pFIO_AbstractDirectoryServiceMod
   use pFIO_ServerThreadMod
   use pFIO_ServerThreadVectorMod
   use pFIO_AbstractSocketMod
   use pFIO_AbstractSocketVectorMod
   use pFIO_AbstractDataReferenceMod
   use pFIO_AbstractServerMod
   use pFIO_BaseServerMod

   implicit none
   private

   public :: MpiServer

   type,extends (BaseServer) :: MpiServer
      character(len=:), allocatable :: port_name
   contains
      procedure :: start
   end type MpiServer

   interface MpiServer
      module procedure new_MpiServer
   end interface MpiServer

contains

   function new_MpiServer(comm, port_name, profiler_name, with_profiler, rc) result(s)
      type (MpiServer) :: s
      integer, intent(in) :: comm
      character(*), intent(in) :: port_name
      character(*), optional, intent(in) :: profiler_name
      logical, optional, intent(in) :: with_profiler
      integer, optional, intent(out) :: rc
      integer :: status

      call s%init(comm, port_name, profiler_name=profiler_name, with_profiler = with_profiler, _rc)
      s%port_name = trim(port_name)
      s%threads = ServerThreadVector()
      _return(_success)
   end function new_MpiServer

   subroutine start(this, rc)
      class (MpiServer), target, intent(inout) :: this
      integer, optional, intent(out) :: rc
      class (ServerThread), pointer :: thread_ptr => null()
      integer :: i,client_size
      logical, allocatable :: mask(:)
      integer :: status

      client_size = this%threads%size()

      allocate(this%serverthread_done_msgs(client_size))
      this%serverthread_done_msgs(:) = .false.

      allocate(mask(client_size))
      mask = .false.
      ! loop untill terminate
      do while (.true.)

         do i = 1,client_size

            if ( mask(i)) cycle

            thread_ptr=>this%threads%at(i)
            !handle the message
            call thread_ptr%run(_rc)
            !delete the thread object if it terminates
            if(thread_ptr%do_terminate()) then
               mask(i) = .true.
            endif
         enddo

         if (all(mask)) exit

      enddo

      call this%threads%clear()
      deallocate(mask)

      call this%report_profile(_rc)

      _return(_success)
   end subroutine start

end module pFIO_MpiServerMod
