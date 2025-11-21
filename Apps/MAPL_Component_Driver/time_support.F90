#include "MAPL.h"
module timeSupport
   use mapl3
   use esmf
   implicit none

   public timeVar

   type :: timeVar
      type(ESMF_Time) :: refTime
      character(len=10) :: timeUnits
      integer :: climYear
      logical :: have_offset
      integer :: update_ref_time
      type(ESMF_TimeInterval) :: update_offset
   contains
      procedure :: init_time
      procedure :: evaluate_time
      procedure :: set_time_for_date
   end type timeVar

contains

   subroutine init_time(this,hconfig,currTime,rc)
      class(timeVar), intent(inout) :: this
      type(ESMF_HConfig), intent(inout) :: hconfig
      type(ESMF_Time), intent(inout) :: currTime
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: isPresent

      character(len=:), allocatable :: iso_time

      isPresent = ESMF_HConfigIsDefined(hconfig, keyString='REF_TIME', _RC)
      if (isPresent) then
         iso_time = ESMF_HConfigAsString(Hconfig, keyString='REF_TIME', _RC)
         call ESMF_TimeSet(this%refTime, timeString=iso_time, _RC)
      else
         this%refTime=currTime
      end if
      
      this%timeUnits = 'days'
      isPresent = ESMF_HConfigIsDefined(hconfig, keyString='TIME_UNITS', _RC)
      if (isPresent) then
         this%timeUnits = ESMF_HConfigAsString(hconfig, keyString='TIME_UNITS', _RC)
      end if

      this%climYear = -1
      isPresent = ESMF_HConfigIsDefined(hconfig, keyString='CLIM_YEAR', _RC)
      if (isPresent) then
         this%climYear = ESMF_HConfigAsI4(hconfig, keyString='CLIM_YEAR', _RC)
      end if

      this%have_offset = .false.
      this%update_ref_time = -1
      isPresent = ESMF_HConfigIsDefined(hconfig, keyString='UPDATE_OFFSET', _RC)
      if (isPresent) then
         this%update_offset = mapl_HConfigAsTimeInterval(hconfig, keystring='UDATE_OFFSET', _RC)
         this%have_offset = .true.
      end if
      
      isPresent = ESMF_HConfigIsDefined(hconfig, keyString='UPDATE_REF_TIME', _RC)
      if (isPresent) then
         this%update_ref_time = ESMF_HConfigAsI4(hconfig, keyString='UPDATE_REF_TIME', _RC) 
      end if
      _RETURN(_SUCCESS)

   end subroutine init_time

   function evaluate_time(this,currTime,rc) result(dt)
      class(timeVar), intent(in) :: this
      type(ESMF_Time), intent(in) :: currTime
      integer, optional, intent(out) :: rc
      real(kind=ESMF_KIND_R8) :: dt

      integer :: status

      type(ESMF_TimeInterval) :: timeInterval, yearInterval
      integer :: ycurr,yint
      type(ESMF_Time) :: periodic_time, temp_time

      temp_time = currTime
      if (this%climYear > 0) then
         call ESMF_TimeGet(currTime,yy=ycurr,_RC)
         yint=this%climYear-ycurr
         call ESMF_TimeIntervalSet(yearInterval,yy=yint,_RC)
         temp_time = temp_time+yearInterval
      end if
      periodic_time = this%set_time_for_date(temp_time,_RC)
      if (this%have_offset) then
         timeInterval = periodic_time + this%update_offset - this%refTime
      else
         timeInterval = periodic_time - this%refTime
      end if
      select case(trim(this%timeUnits))
      case ('days')
         call ESMF_TimeIntervalGet(timeInterval,d_r8=dt,_RC)
      case ('hours')
         call ESMF_TimeIntervalGet(timeInterval,h_r8=dt,_RC)
      case ('minutes')
         call ESMF_TimeIntervalGet(timeInterval,m_r8=dt,_RC)
      case ('seconds')
         call ESMF_TimeIntervalGet(timeInterval,s_r8=dt,_RC)
      case default
         _FAIL("Unsupported time units specify for interval")
      end select
      _RETURN(_SUCCESS)

   end function evaluate_time

   function set_time_for_date(this,input_time,rc) result(returned_time)
      type(ESMF_Time) :: returned_time

      class(timeVar), intent(in) :: this
      type(ESMF_Time), intent(inout) :: input_time
      integer, optional, intent(out) :: rc

      integer :: hour,minute,second,year,month,day,status
      type(ESMF_Time) :: new_time

      if (this%update_ref_time /= -1) then
         call ESMF_TimeGet(input_time,yy=year,mm=month,dd=day,_RC)
         hour = this%update_ref_time/10000
         minute = mod(this%update_ref_time/100,100)
         second = mod(this%update_ref_time,100) 
         call ESMF_TimeSet(new_time,yy=year,mm=month,dd=day,h=hour,m=minute,s=second,_RC)
         if (new_time == input_time) then
            returned_time = input_time
         else if (new_time < input_time) then
            returned_time = new_time
         else if (new_time > input_time) then
            call ESMF_TimeSet(new_time,yy=year,mm=month,dd=day-1,h=hour,m=minute,s=second,_RC)
            returned_time = new_time
         end if
      else
         returned_time = input_time
      end if
      _RETURN(_SUCCESS)
   end function

end module timeSupport

