program Example_Driver
   use MPI
   use ESMF
   implicit none

   integer :: status,mpi_stat

   call ESMF_Initialize(rc=status)
   if (status/=0) call abort_program()


   call ESMF_Finalize(rc=status)
   if (status/=0) call abort_program()
   contains
  
   function create_clock(driver_cf,rc) result(clock)
      type(ESMF_Clock) :: clock
      type(ESMF_Config), intent(in) :: driver_cf
      integer, intent(out), optional :: rc

      integer :: status, start_hms, start_ymd

      ! get some information from the ESMF config file so we can create
      ! a clock, we can't do anything without a clock
      call ESMF_ConfigGetAttribute(driver_cf,value=start_hms,lable="Start_Time:",rc=status)
      call ESMF_ConfigGetAttribute(driver_cf,value=start_ymd,lable="Start_Date:",rc=status)

   end function

   subroutine abort_program()
     integer :: status, mpi_stat

     call MPI_Abort(MPI_COMM_WORLD,mpi_stat,status)

   end subroutine

end program Example_Driver

