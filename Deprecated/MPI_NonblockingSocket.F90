module CFIO_MPI_NonblockingSocketMod
   use CFIO_AbstractSocketMod
   private
   implicit none

   public :: MPI_NonblockingSocket

   type, extends(AbstractSocket) :: MPI_NonblockingSocket
   end type MPI_NonblockingSocket


contains


   subroutine write(this, message)
      select type (message)
      type is (data)
         ! when do we do the MPI_wait?
         call MPI_Isend(...)
      end select
   end subroutine write

   

end module CFIO_MPI_NonblockingSocketMod
