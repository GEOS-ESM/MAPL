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
      type(ESMF_TimeInterval) :: update_freq
      type(ESMF_Time) :: last_ring
      type(ESMF_Time) :: reference_time
      logical :: simple_alarm_created = .false.
      type(ESMF_TIme) :: last_checked
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

      integer :: status,int_time,year,month,day,hour,minute,second

      this%last_checked = time
      if (update_freq == "-") then
         this%single_shot = .true.
      else if (update_freq /= "PT0S") then
         this%simple_alarm_created = .true.
         int_time = string_to_integer_time(update_time)
         hour=int_time/10000
         minute=mod(int_time/100,100)
         second=mod(int_time,100)
         call ESMF_TimeGet(time,yy=year,mm=month,dd=day,__RC__)
         call ESMF_TimeSet(this%reference_time,yy=year,mm=month,dd=day,h=hour,m=minute,s=second,__RC__)
         this%last_ring = this%reference_time
         this%update_freq = string_to_esmf_timeinterval(update_freq,__RC__)
      end if
      this%offset=string_to_esmf_timeinterval(update_offset,__RC__)
      _RETURN(_SUCCESS)

   end subroutine create_from_parameters

   subroutine check_update(this,do_update,use_time,current_time,first_time,rc)
      class(ExtDataPointerUpdate), intent(inout) :: this
      logical, intent(out) :: do_update
      type(ESMF_Time), intent(inout) :: use_time
      type(ESMF_Time), intent(inout) :: current_time
      logical, intent(in) :: first_time
      integer, optional, intent(out) :: rc
      type(ESMF_Time) :: next_ring

      integer :: status

      if (this%disabled) then
         do_update = .false.
         _RETURN(_SUCCESS)
      end if
      !if (ESMF_AlarmIsCreated(this%update_alarm)) then
      if (this%simple_alarm_created) then
         use_time = current_time+this%offset
         if (first_time) then
            do_update = .true.
            if (mapl_am_I_root()) write(*,*)"bmaa first time "
         else
            ! normal flow
            next_ring = this%last_ring
            if (current_time > this%last_checked) then
               if (mapl_am_i_root()) write(*,*)"bmaa normal flow!"
               do while (next_ring < current_time)
                  next_ring=next_ring+this%update_freq
               enddo
               if (current_time == next_ring) then
                  do_update = .true.
                  this%last_ring = next_ring
                  if (mapl_am_I_root()) write(*,*)"bmaa update "
               end if
            ! if clock went backwards, so we must update, set ringtime to previous ring from working time
            else if (current_time < this%last_checked) then
               if (mapl_am_i_root()) write(*,*)"bmaa clock went back!"

               next_ring = this%last_ring
               if (this%last_ring > current_time) then
                  do while(next_ring >= current_time)
                     next_ring=next_ring-this%update_freq
                  enddo
               end if
               do_update = .true.
               this%last_ring = next_ring
            end if
         end if
      else
         do_update = .true.
         if (this%single_shot) this%disabled = .true.
         use_time = current_time+this%offset
      end if
      this%last_checked = current_time

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
