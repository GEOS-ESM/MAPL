module pfio_base
   integer, save :: debug_unit = 0
contains

   subroutine pfio_init()
      use MPI
      character(len=5) :: buf
      integer :: rank, ierror

      if (debug_unit == 0) then
         call MPI_Comm_rank(MPI_Comm_world, rank, ierror)
         write(buf,'(i5.5)') rank
         open(newunit=debug_unit,file='pfio_debug.'//buf,status='unknown', form='formatted')
      end if
   end subroutine pfio_init

end module pfio_base


