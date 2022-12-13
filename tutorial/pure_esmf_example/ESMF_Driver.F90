program Example_Driver
   use MPI
   use ESMF
   implicit none

   integer :: status,mpi_stat

   call ESMF_Initialize(rc=status)
   if (status/=0) call MPI_Abort(MPI_COMM_WORLD,mpi_stat,status)

   contains
  
   function create_clock(driver_cf,rc) result(clock)
      type(ESMF_Clock) :: clock
      type(ESMF_Config), intent(in) :: driver_cf
      integer, intent(out), optional :: rc

      integer :: status, start_hms, start_ymd, cap_rs_unit

      open(newunit=cap_rs_unit,file="cap_restart",form='formatted')
      read(cap_rs_unit)start_ymd, start_hms
      close(cap_rs_unit)

   end function

end program Example_Driver

