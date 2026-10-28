#include "MAPL.h"
module mapl_ExtDataPointerUpdate_mod
   use ESMF
   use MAPL
   use mapl_ExtDataSample_mod
   implicit none
   private

   public :: ExtDataPointerUpdate

   ! Decides WHEN an export is updated and WHAT time is used when it is.
   !   update_frequency      "PT0S" (default): update every run
   !                         "-": update once, then never again
   !                         otherwise: update at reference_time + n*frequency
   !   update_reference_time time of day (hhmmss as an integer) of a ring on
   !                         the date of the first call
   !   update_offset         added to the time used to select data; it does
   !                         not change when updates happen
   type :: ExtDataPointerUpdate
      private
      logical :: disabled = .false.
      logical :: single_shot = .false.
      logical :: alarm_created = .false.
      logical :: first_call = .true.
      logical :: first_update_pending = .true.
      type(ESMF_TimeInterval) :: offset
      type(ESMF_TimeInterval) :: update_freq
      integer :: ref_hour = 0
      integer :: ref_minute = 0
      integer :: ref_second = 0
      type(ESMF_Time) :: last_ring
      type(ESMF_Time) :: last_checked
   contains
      procedure :: create
      procedure :: check_update
      procedure :: is_single_shot
   end type ExtDataPointerUpdate

contains

   subroutine create(this, sample, time_step, unusable, rc)
      class(ExtDataPointerUpdate), intent(inout) :: this
      type(ExtDataSample), intent(in) :: sample
      type(ESMF_TimeInterval), intent(in) :: time_step
      class(mapl_KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status, int_time, ios

      _UNUSED_DUMMY(unusable)

      this%offset = sample%get_update_offset(time_step, _RC)

      if (sample%refresh_frequency == "-") then
         this%single_shot = .true.
      else if (sample%refresh_frequency /= "PT0S") then
         this%alarm_created = .true.
         call ESMF_TimeIntervalSet(this%update_freq, sample%refresh_frequency, _RC)
         read(sample%refresh_time, *, iostat=ios) int_time
         _ASSERT(ios == 0, "update_reference_time must be an integer hhmmss: "//sample%refresh_time)
         this%ref_hour = int_time/10000
         this%ref_minute = mod(int_time/100, 100)
         this%ref_second = mod(int_time, 100)
      end if

      _RETURN(_SUCCESS)
   end subroutine create

   subroutine check_update(this, do_update, use_time, current_time, rc)
      class(ExtDataPointerUpdate), intent(inout) :: this
      logical, intent(out) :: do_update
      type(ESMF_Time), intent(out) :: use_time
      type(ESMF_Time), intent(in) :: current_time
      integer, optional, intent(out) :: rc

      integer :: status, year, month, day
      type(ESMF_Time) :: next_ring

      do_update = .false.
      use_time = current_time + this%offset

      if (this%disabled) then
         _RETURN(_SUCCESS)
      end if

      if (this%alarm_created) then
         if (this%first_call) then
            call ESMF_TimeGet(current_time, yy=year, mm=month, dd=day, _RC)
            call ESMF_TimeSet(this%last_ring, yy=year, mm=month, dd=day, &
                 h=this%ref_hour, m=this%ref_minute, s=this%ref_second, _RC)
            do_update = .true.
            this%first_update_pending = .true.
            use_time = this%last_ring + this%offset
         else if (current_time > this%last_checked) then
            next_ring = this%last_ring
            do while (next_ring < current_time)
               next_ring = next_ring + this%update_freq
            end do
            if (current_time == next_ring) then
               do_update = .true.
               this%last_ring = next_ring
               this%first_update_pending = .false.
            end if
         else if (current_time < this%last_checked) then
            ! the clock went backwards
            if (this%last_ring > current_time) then
               next_ring = this%last_ring
               do while (next_ring >= current_time)
                  next_ring = next_ring - this%update_freq
               end do
               use_time = next_ring + this%offset
               this%last_ring = next_ring
               do_update = .true.
            else if (this%last_ring < current_time) then
               if (this%first_update_pending) then
                  do_update = .true.
                  this%first_update_pending = .false.
                  use_time = this%last_ring + this%offset
               end if
            else
               do_update = .true.
               this%first_update_pending = .false.
            end if
         end if
      else
         do_update = .true.
         if (this%single_shot) this%disabled = .true.
      end if

      this%first_call = .false.
      this%last_checked = current_time

      _RETURN(_SUCCESS)
   end subroutine check_update

   logical function is_single_shot(this)
      class(ExtDataPointerUpdate), intent(in) :: this
      is_single_shot = this%single_shot
   end function is_single_shot

end module mapl_ExtDataPointerUpdate_mod
