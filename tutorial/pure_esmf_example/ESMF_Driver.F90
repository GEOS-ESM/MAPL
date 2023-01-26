program Example_Driver
   use MPI
   use ESMF
!  use for our component
   ! use Kitty_GridCompMod,  only:  Kitty_SetServices => SetServices
   implicit none

   integer :: status,userRc
   type(ESMF_GridComp :: gc

   call ESMF_Initialize(rc=status)
   if (status/=0) call abort_program()

   gc = ESMF_GridCompCreate(name="cute_kitten_component",rc=status
   call ESMF_GridCompSetServices (gc, Kittye_SetServices, userRC=userRC, rc=status)
!  call component's initialize

!  do some time stepping and run

!  finalize component

   call ESMF_Finalize(rc=status)
   if (status/=0) call abort_program()
   contains
  
   function create_clock(driver_cf,rc) result(clock)
      type(ESMF_Clock) :: clock
      type(ESMF_Config), intent(in) :: driver_cf
      integer, intent(out), optional :: rc

      integer :: status, start_hms, start_ymd
      type(ESMF_Time) :: start_time, stop_time
      type(ESMF_TimeInterval) :: time_step
      integer :: year,month,day,hour,minute,second

      ! get some information from the ESMF config file so we can create
      ! a clock, we can't do anything without a clock
      call ESMF_ConfigGetAttribute(driver_cf,value=start_hms,label="Start_Time:",rc=status)
      call ESMF_ConfigGetAttribute(driver_cf,value=start_ymd,label="Start_Date:",rc=status)
      call unpack_triple(start_ymd,year,month,day)
      call unpack_triple(start_hms,hour,minute,second)

      call ESMF_TimeSet(start_time,yy=year,mm=month,dd=day,h=hour,m=minute,s=second,rc=status)

      call ESMF_ConfigGetAttribute(driver_cf,value=start_hms,label="End_Time:",rc=status)
      call ESMF_ConfigGetAttribute(driver_cf,value=start_ymd,label="End_Date:",rc=status)
      call unpack_triple(end_ymd,year,month,day)
      call unpack_triple(end_hms,hour,minute,second)
      call ESMF_TimeSet(end_time,yy=year,mm=month,dd=day,h=hour,m=minute,s=second,rc=status)

   end function

   !
   subroutine unpack_triple(packed_input, i1, i2, i3)
      integer, intent(in) :: packed_input
      integer, intent(out) :: i1,i2,i3

      i1 = packed_input/10000
      i2 = mod(packed_input/100,100)
      i3 = mod(packed_input,100)

   end subroutine
     
   subroutine abort_program()
     integer :: status, mpi_stat

     call MPI_Abort(MPI_COMM_WORLD,mpi_stat,status)

   end subroutine

end program Example_Driver

