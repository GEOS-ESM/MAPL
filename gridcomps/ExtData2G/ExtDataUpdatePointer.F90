#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"

module MAPL_ExtDataPointerUpdate
   use ESMF
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   use MAPL_TimeStringConversion
   use MAPL_CommsMod
   implicit none
   private

   public :: ExtDataPointerUpdate

   type :: ExtDataPointerUpdate
      private
      logical :: disabled = .false.
      logical :: first_time_updated = .true.
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
         procedure :: get_adjusted_time
   end type

   contains

   function get_adjusted_time(this,time,rc) result(adjusted_time)
      type(ESMF_Time) :: adjusted_time
      class(ExtDataPointerUpdate), intent(inout) :: this
      type(ESMF_Time), intent(in) :: time
      integer, optional, intent(out) :: rc

      adjusted_time = time+this%offset

      _RETURN(_SUCCESS)
   end function

   subroutine create_from_parameters(this,update_time,update_freq,update_offset,time,clock,rc)
      class(ExtDataPointerUpdate), intent(inout) :: this
      character(len=*), intent(in) :: update_time
      character(len=*), intent(in) :: update_freq
      character(len=*), intent(in) :: update_offset
      type(ESMF_Time), intent(inout) :: time
      type(ESMF_Clock), intent(inout) :: clock
      integer, optional, intent(out) :: rc

      integer :: status,int_time,year,month,day,hour,minute,second,neg_index
      logical :: negative_offset

      this%last_checked = time
      if (update_freq == "-") then
         this%single_shot = .true.
      else if (update_freq /= "PT0S") then
         this%simple_alarm_created = .true.
         int_time = string_to_integer_time(update_time)
         hour=int_time/10000
         minute=mod(int_time/100,100)
         second=mod(int_time,100)
         call ESMF_TimeGet(time,yy=year,mm=month,dd=day,_RC)
         call ESMF_TimeSet(this%reference_time,yy=year,mm=month,dd=day,h=hour,m=minute,s=second,_RC)
         this%last_ring = this%reference_time
         this%update_freq = string_to_esmf_timeinterval(update_freq,_RC)
      end if
      negative_offset = .false.
      if (index(update_offset,"-") > 0) then
         negative_offset = .true.
         neg_index = index(update_offset,"-")
      end if
      if (negative_offset) then
         this%offset=string_to_esmf_timeinterval(update_offset(neg_index+1:),_RC)
         this%offset = -this%offset
      else
         this%offset=string_to_esmf_timeinterval(update_offset,_RC)
      end if
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(clock)

   end subroutine create_from_parameters

   subroutine check_update(this,do_update,use_time,current_time,first_time,rc)
      class(ExtDataPointerUpdate), intent(inout) :: this
      logical, intent(out) :: do_update
      type(ESMF_Time), intent(inout) :: use_time
      type(ESMF_Time), intent(inout) :: current_time
      logical, intent(in) :: first_time
      integer, optional, intent(out) :: rc
      type(ESMF_Time) :: next_ring

      if (this%disabled) then
         do_update = .false.
         _RETURN(_SUCCESS)
      end if
      if (this%simple_alarm_created) then
         use_time = current_time+this%offset
         if (first_time) then
            do_update = .true.
            this%first_time_updated = .true.
            use_time = this%last_ring + this%offset
         else
            ! normal flow
            next_ring = this%last_ring
            if (current_time > this%last_checked) then
               do while (next_ring < current_time)
                  next_ring=next_ring+this%update_freq
               enddo
               if (current_time == next_ring) then
                  do_update = .true.
                  this%last_ring = next_ring
                  this%first_time_updated = .false.
               end if
            ! if clock went backwards, so we must update, set ringtime to previous ring from working time
            else if (current_time < this%last_checked) then
               next_ring = this%last_ring
               ! the clock must have rewound past last ring
               if (this%last_ring > current_time) then
                  do while(next_ring >= current_time)
                     next_ring=next_ring-this%update_freq
                  enddo
                  use_time = next_ring+this%offset
                  this%last_ring = next_ring
                  do_update = .true.
               ! alarm never rang during the previous advance, only update the previous update was the first time
               else if (this%last_ring < current_time) then
                    if (this%first_time_updated) then
                       do_update=.true.
                       this%first_time_updated = .false.
                       use_time = this%last_ring + this%offset
                    end if
               ! otherwise we land on a time when the alarm would ring and we would update
               else if (this%last_ring == current_time) then
                  do_update =.true.
                  this%first_time_updated = .false.
                  use_time = current_time+this%offset
               end if
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
