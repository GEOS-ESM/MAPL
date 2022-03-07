#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"

module MAPL_ExtDataPointerUpdate
   use ESMF
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   use MAPL_TimeStringConversion
   implicit none
   private

   public :: ExtDataPointerUpdate

   type :: ExtDataPointerUpdate
      private
      logical :: disabled = .false.
      type(ESMF_Alarm) :: update_alarm
      type(ESMF_TimeInterval) :: offset
      logical :: single_shot = .false.
      contains
         procedure :: create_from_parameters
         procedure :: check_update
         procedure :: is_disabled
         procedure :: is_single_shot
         procedure :: disable
   end type

   contains

   subroutine create_from_parameters(this,update_time,update_freq,update_offset,time,clock,rc)
      class(ExtDataPointerUpdate), intent(inout) :: this
      character(len=*), intent(in) :: update_time
      character(len=*), intent(in) :: update_freq
      character(len=*), intent(in) :: update_offset
      type(ESMF_Time), intent(inout) :: time
      type(ESMF_Clock), intent(inout) :: clock
      integer, optional, intent(out) :: rc

      type(ESMF_Time) :: reference_time
      type(ESMF_TimeInterval) :: reference_freq
      integer :: status,int_time,year,month,day,hour,minute,second

      if (update_freq == "-") then
         this%single_shot = .true.
      else if (update_freq /= "PT0S") then
         int_time = string_to_integer_time(update_time)
         hour=int_time/10000
         minute=mod(int_time/100,100)
         second=mod(int_time,100)
         call ESMF_TimeGet(time,yy=year,mm=month,dd=day,__RC__)
         call ESMF_TimeSet(reference_time,yy=year,mm=month,dd=day,h=hour,m=minute,s=second,__RC__)
         reference_freq = string_to_esmf_timeinterval(update_freq,__RC__)
         this%update_alarm = ESMF_AlarmCreate(clock,ringTime=reference_time,ringInterval=reference_freq,sticky=.false.,__RC__)
      end if
      this%offset=string_to_esmf_timeinterval(update_offset,__RC__)
      _RETURN(_SUCCESS)

   end subroutine create_from_parameters

   subroutine check_update(this,do_update,working_time,current_time,first_time,rc)
      class(ExtDataPointerUpdate), intent(inout) :: this
      logical, intent(out) :: do_update
      type(ESMF_Time), intent(inout) :: working_time
      type(ESMF_Time), intent(inout) :: current_time
      logical, intent(in) :: first_time
      integer, optional, intent(out) :: rc
      type(ESMF_Time) :: previous_ring

      integer :: status

      if (this%disabled) then
         do_update = .false.
         _RETURN(_SUCCESS)
      end if
      if (ESMF_AlarmIsCreated(this%update_alarm)) then
         if (first_time) then
            call ESMF_AlarmGet(this%update_alarm,prevRingTime=previous_ring,__RC__)
            working_time =previous_ring+this%offset
            do_update = .true.
         else
            do_update = ESMF_AlarmIsRinging(this%update_alarm,__RC__)
            working_time = current_time+this%offset
         end if
      else
         do_update = .true.
         if (this%single_shot) this%disabled = .true.
         working_time = current_time+this%offset
      end if

   end subroutine check_update

   function is_disabled(this) result(disabled)
      class(ExtDataPointerUpdate), intent(in) :: this
      logical :: disabled
      disabled = this%disabled
   end function is_disabled

   function is_single_shot(this) result(single_shot)
      class(ExtDataPointerUpdate), intent(in) :: this
      logical :: single_shot
      single_shot = this%single_shot
   end function is_single_shot

   subroutine disable(this)
      class(ExtDataPointerUpdate), intent(inout) :: this
      this%disabled = .true.
   end subroutine

end module MAPL_ExtDataPointerUpdate
