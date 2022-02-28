#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
module MAPL_ExtdataClimFileHandler
   use ESMF
   use MAPL_ExtDataAbstractFileHandler
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   use MAPL_ExtDataFileStream
   use MAPL_ExtDataFileStreamMap
   use MAPL_DataCollectionMod
   use MAPL_CollectionVectorMod
   use MAPL_DataCollectionManagerMod
   use MAPL_FileMetadataUtilsMod
   use MAPL_TimeStringConversion
   use MAPL_StringTemplate
   use MAPL_ExtDataBracket
   use MAPL_ExtDataConstants
   implicit none
   private
   public ExtDataClimFileHandler

   integer, parameter :: CLIM_NULL = -1
   type, extends(ExtDataAbstractFileHandler) :: ExtDataClimFileHandler
      integer :: clim_year = CLIM_NULL
      contains
         procedure :: get_file_bracket
         procedure :: get_file
   end type

contains

   subroutine get_file_bracket(this, input_time, source_time, bracket, rc)
      class(ExtdataClimFileHandler), intent(inout) :: this
      type(ESMF_Time), intent(in) :: input_time
      type(ESMF_Time), intent(in) :: source_time(:)
      type(ExtDataBracket), intent(inout) :: bracket
      integer, optional, intent(out) :: rc

      type(ESMF_Time) :: time
      integer :: time_index
      character(len=ESMF_MAXPATHLEN) :: file
      integer :: status
      type(ESMF_TimeInterval) :: zero
      type(ESMF_Time) :: target_time

      integer :: target_year, original_year,clim_shift,valid_years(2)
      integer, allocatable :: source_years(:)
     
      
      if (bracket%time_in_bracket(input_time)) then
         _RETURN(_SUCCESS)
      end if

      target_time=input_time
      _ASSERT(size(this%valid_range) == 2, 'Valid time is not defined so can not do any extrapolation or climatology')
      call ESMF_TimeGet(this%valid_range(1),yy=valid_years(1),__RC__)
      call ESMF_TimeGet(this%valid_range(2),yy=valid_years(2),__RC__)
      if (size(source_time)==2) then
         allocate(source_years(2))
         call ESMF_TimeGet(source_time(1),yy=source_years(1),__RC__)
         call ESMF_TimeGet(source_time(2),yy=source_years(2),__RC__)
         _ASSERT(source_years(1) >= valid_years(1),'source time outide valid range')
         _ASSERT(source_years(1) <=  valid_years(2),'source time outide valid range')
         _ASSERT(source_years(2) >=  valid_years(1),'source time outide valid range')
         _ASSERT(source_years(2) <= valid_years(2),'source time outide valid range')
      end if

      ! shift target year to request source time if specified
      ! is TS1 < TM < TS2, if not then extrapolate beyond that
      call ESMF_TimeGet(target_time,yy=target_year,__RC__)
      original_year=target_year

      if (size(source_years)>0) then
         if (target_year < source_years(1)) then
            target_year = source_years(1)
            this%clim_year = target_year 
         else if (target_year >= source_years(2)) then
            target_year = source_years(2)
            this%clim_year = target_year 
         end if
         call swap_year(target_time,target_year,__RC__)
      else
         if (target_year < valid_years(1)) then
            target_year = valid_years(1)
            this%clim_year = target_year
            call swap_year(target_time,target_year,__RC__)
         else if (target_year >= valid_years(2)) then
            target_year = valid_years(2)
            this%clim_year = target_year
            call swap_year(target_time,target_year,__RC__)
         end if 
      end if

      ! the target time is contained in the dataset and we are not extrapolating outside of source time selection based on available data
      if (this%clim_year == CLIM_NULL) then

         call ESMF_TimeIntervalSet(zero,__RC__)      
         if (this%frequency == zero) then
            file = this%file_template
            call this%get_time_on_file(file,input_time,'L',time_index,time,__RC__)
            _ASSERT(time_index/=time_not_found,"Time not found on file")
            call bracket%set_node('L',file=file,time_index=time_index,time=time,__RC__)
            if (bracket%left_node == bracket%right_node) then
               call bracket%swap_node_fields(rc=status)
               _VERIFY(status)
            else
               bracket%new_file_left=.true.
            end if
            call this%get_time_on_file(file,input_time,'R',time_index,time,__RC__)
            _ASSERT(time_index/=time_not_found,"Time not found on file")
            call bracket%set_node('R',file=file,time_index=time_index,time=time,__RC__)
            bracket%new_file_right=.true.
         else
            call this%get_file(file,target_time,0,__RC__)
            call this%get_time_on_file(file,target_time,'L',time_index,time,rc=status)
            if (time_index == time_not_found) then
               call this%get_file(file,target_time,-1,__RC__)
               call this%get_time_on_file(file,target_time,'L',time_index,time,__RC__)
               _ASSERT(time_index/=time_not_found,"Time not found on file")
            end if
            call bracket%set_node('L',file=file,time_index=time_index,time=time,__RC__)
            if (bracket%left_node == bracket%right_node) then
               call bracket%swap_node_fields(rc=status)
               _VERIFY(status)
            else
               bracket%new_file_left=.true.
            end if

            call this%get_file(file,target_time,0,__RC__)
            call this%get_time_on_file(file,target_time,'R',time_index,time,rc=status)
            if (time_index == time_not_found) then
               call this%get_file(file,target_time,1,__RC__)
               call this%get_time_on_file(file,target_time,'R',time_index,time,__RC__)
               _ASSERT(time_index/=time_not_found,"Time not found on file")
            end if
            call bracket%set_node('R',file=file,time_index=time_index,time=time,__RC__)
            bracket%new_file_right=.true.
         end if

      ! the target time has been specified to be a climatology for the year; either we 
      ! are outside the dataset or we have requested a source time range and are on
      ! or outside either end
      else

         call ESMF_TimeIntervalSet(zero,__RC__)      
         if (this%frequency == zero) then
            file = this%file_template
            clim_shift=0
            call this%get_time_on_file(file,target_time,'L',time_index,time,wrap=clim_shift,__RC__)
            _ASSERT(time_index/=time_not_found,"Time not found on file")
            call swap_year(time,original_year+clim_shift,__RC__)
            call bracket%set_node('L',file=file,time_index=time_index,time=time,__RC__)
            if (bracket%left_node == bracket%right_node) then
               call bracket%swap_node_fields(rc=status)
               _VERIFY(status)
            else
               bracket%new_file_left=.true.
            end if

            clim_shift=0
            call this%get_time_on_file(file,target_time,'R',time_index,time,wrap=clim_shift,__RC__)
            _ASSERT(time_index/=time_not_found,"Time not found on file")
            call swap_year(time,original_year+clim_shift,__RC__)
            call bracket%set_node('R',file=file,time_index=time_index,time=time,__RC__)
            bracket%new_file_right=.true.

         else

            call this%get_file(file,target_time,0,__RC__)
            call this%get_time_on_file(file,target_time,'L',time_index,time,rc=status)
            if (time_index == time_not_found) then
               call this%get_file(file,target_time,-1,__RC__)
               call this%get_time_on_file(file,target_time,'L',time_index,time,__RC__)
               _ASSERT(time_index/=time_not_found,"Time not found on file")
               call ESMF_TimeGet(target_time,yy=target_year,__RC__)
               if (target_year > this%clim_year) then
                  call swap_year(time,original_year-1,__RC__)
               else 
                  call swap_year(time,original_year,__RC__)
               end if         
            else
               call swap_year(time,original_year,__RC__)
            end if
            if (bracket%left_node == bracket%right_node) then
               call bracket%swap_node_fields(rc=status)
               _VERIFY(status)
            else
               bracket%new_file_left=.true.
            end if
            call bracket%set_node('L',file=file,time_index=time_index,time=time,__RC__)

            call this%get_file(file,target_time,0,__RC__)
            call this%get_time_on_file(file,target_time,'R',time_index,time,rc=status)
            if (time_index == time_not_found) then
               call this%get_file(file,target_time,1,__RC__)
               call this%get_time_on_file(file,target_time,'R',time_index,time,__RC__)
               _ASSERT(time_index/=time_not_found,"Time not found on file")
               call ESMF_TimeGet(target_time,yy=target_year,__RC__)
               if (target_year < this%clim_year) then
                  call swap_year(time,original_year+1,__RC__)
               else 
                  call swap_year(time,original_year,__RC__)
               end if         
            else
               call swap_year(time,original_year,__RC__)
            end if
            call bracket%set_node('R',file=file,time_index=time_index,time=time,__RC__)
            bracket%new_file_right=.true.

         end if

      end if

      _RETURN(_SUCCESS)
   
   end subroutine get_file_bracket

   subroutine get_file(this,filename,target_time,shift,rc)
      class(ExtdataClimFileHandler), intent(inout) :: this
      character(len=*), intent(out) :: filename
      type(ESMF_Time) :: target_time
      integer, intent(in) :: shift
      integer, intent(out), optional :: rc

      type(ESMF_Time) :: ftime
      integer :: n,status
      logical :: file_found
      integer :: new_year
      integer(ESMF_KIND_I8) :: interval_seconds


      call ESMF_TimeIntervalGet(this%frequency,s_i8=interval_seconds)
      if (interval_seconds==0) then
         ! time is not representable as absolute time interval (month, year etc...) do this 
         ! brute force way. Not good but ESMF leaves no choice
         ftime=this%reff_time
         do while (ftime < target_time)
            ftime = ftime + this%frequency
         enddo
         ftime=ftime -this%frequency + shift*this%frequency
      else
         n = (target_time-this%reff_time)/this%frequency 
         ftime = this%reff_time+(n+shift)*this%frequency
      end if
      if (this%clim_year /= CLIM_NULL) then
         call ESMF_TimeGet(ftime,yy=new_year,__RC__)
         if (new_year/=this%clim_year) then
            call swap_year(ftime,this%clim_year,__RC__)
            if (shift > 0) then
               call swap_year(target_time,this%clim_year-shift)
            else if (shift < 0) then
               call swap_year(target_time,this%clim_year+shift)
            end if
         end if
      end if
      call fill_grads_template(filename,this%file_template,time=ftime,__RC__)
      inquire(file=trim(filename),exist=file_found)
      _ASSERT(file_found,"get_file did not file a file using: "//trim(this%file_template))
      _RETURN(_SUCCESS)

   end subroutine get_file

   subroutine swap_year(time,year,rc)
      type(ESMF_Time), intent(inout) :: time
      integer, intent(in) :: year
      integer, optional, intent(out) :: rc
      logical :: is_leap_year
      type(ESMF_Calendar) :: calendar
      integer :: status, month, day, hour, minute, second
 
      is_leap_year=.false. 
      call ESMF_TimeGet(time,mm=month,dd=day,h=hour,m=minute,s=second,calendar=calendar,__RC__)
      if (day==29 .and. month==2) then
         is_leap_year = ESMF_CalendarIsLeapYear(calendar,year,__RC__)
         if (.not.is_leap_year) day=28
      end if
      call ESMF_TimeSet(time,yy=year,mm=month,dd=day,h=hour,m=minute,s=second,__RC__)
      _RETURN(_SUCCESS)
   end subroutine

end module MAPL_ExtdataClimFileHandler
