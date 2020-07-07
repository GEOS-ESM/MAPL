module pFIO_MpiServerMod

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

   function new_MpiServer(comm, port_name) result(s)
      type (MpiServer) :: s
      integer, intent(in) :: comm
      character(*), intent(in) :: port_name

      call s%init(comm)

      s%port_name = trim(port_name)
      s%threads = ServerThreadVector()

   end function new_MpiServer

   subroutine start(this)
      class (MpiServer), target, intent(inout) :: this

      class (ServerThread), pointer :: thread_ptr => null()
      integer :: i,client_size
      logical, allocatable :: mask(:)

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
            call thread_ptr%run()
            !delete the thread object if it terminates 
            if(thread_ptr%do_terminate()) then
               mask(i) = .true.
            endif
         enddo

         if (all(mask)) exit

      enddo

      call this%threads%clear()
      deallocate(mask)

   end subroutine start

end module pFIO_MpiServerMod
