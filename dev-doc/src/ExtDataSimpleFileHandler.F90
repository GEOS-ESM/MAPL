#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
module MAPL_ExtdataSimpleFileHandler
   use ESMF
   use MAPL_ExtDataAbstractFileHandler
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
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
   public ExtDataSimpleFileHandler

   type, extends(ExtDataAbstractFileHandler) :: ExtDataSimpleFileHandler
      contains
         procedure :: get_file_bracket
         procedure :: get_file
   end type

contains

   subroutine get_file_bracket(this, input_time, source_time, bracket, fail_on_missing_file, rc)
      class(ExtdataSimpleFileHandler), intent(inout) :: this
      type(ESMF_Time), intent(in) :: input_time
      type(ESMF_Time), intent(in) :: source_time(:)
      type(ExtDataBracket), intent(inout) :: bracket
      logical, intent(in) :: fail_on_missing_file
      integer, optional, intent(out) :: rc
      integer :: status
      type(ESMF_TimeInterval) :: zero

      type(ESMF_Time) :: time
      integer :: time_index
      character(len=ESMF_MAXPATHLEN) :: current_file
      logical :: get_left, get_right,in_range,left_was_set,right_was_set
      type(ESMF_Time) :: target_time,ghost_time
      logical :: allow_missing_file

      get_left=.true.
      get_right=.true.
      in_range=.true.
      target_time=input_time

      allow_missing_file = .not.fail_on_missing_file

      call bracket%get_node('L',was_set=left_was_set)
      call bracket%get_node('R',was_set=right_was_set)

      call bracket%set_parameters(intermittent_disable=.false.)
      if (this%persist_closest) then
         if (input_time <= this%valid_range(1)) then
            target_time = this%valid_range(1)
            get_right = .false.
            in_range = .false.
            if (left_was_set) get_left=.false.
            call bracket%set_parameters(intermittent_disable=.true.)
         else if (input_time >= this%valid_range(2)) then
            target_time = this%valid_range(2)
            get_right = .false.
            in_range = .false.
            if (left_was_set) get_left=.false.
            call bracket%set_parameters(intermittent_disable=.true.)
         end if
      else
         _ASSERT(left_was_set.eqv.right_was_set,"You should not be here")
      end if
      if (in_range) then
         if (bracket%time_in_bracket(target_time)) then
            _RETURN(_SUCCESS)
         end if
      end if

      call ESMF_TimeIntervalSet(zero,_RC)
      if (this%frequency == zero) then
         current_file = this%file_template
         if (get_left) then
            call this%get_time_on_file(current_file,target_time,'L',time_index,time,_RC)
            _ASSERT(time_index/=time_not_found,"Time not found in file")
            call bracket%set_node('L',file=current_file,time_index=time_index,time=time,was_set=.true.,_RC)
            if (in_range .and. (bracket%left_node == bracket%right_node)) then
               call bracket%swap_node_fields(rc=status)
               _VERIFY(status)
            else
               bracket%new_file_left=.true.
            end if
         end if
         if (get_right) then
            call this%get_time_on_file(current_file,target_time,'R',time_index,time,_RC)
            _ASSERT(time_index/=time_not_found,"Time not found in file")
            call bracket%set_node('R',file=current_file,time_index=time_index,time=time,was_set=.true.,_RC)
            bracket%new_file_right=.true.
         end if
      else
         if (get_left) then
            call this%get_file(current_file,target_time,0,allow_missing_file,ghost_time=ghost_time,_RC)
            call this%get_time_on_file(current_file,target_time,'L',time_index,time,_RC)
            if (current_file == file_not_found) time=ghost_time

            call bracket%set_node('L',file=current_file,time_index=time_index,time=time,was_set=.true.,_RC)
            if (in_range .and. (bracket%left_node == bracket%right_node)) then
               if (.not. (current_file == file_not_found)) then
                  call bracket%swap_node_fields(_RC)
                  bracket%new_file_left = .false.
               end if
            else
               if (time_index == time_not_found ) then
                  call this%get_file(current_file,target_time,-1,allow_missing_file,_RC)
                  call this%get_time_on_file(current_file,target_time,'L',time_index,time,_RC)
                  if (time_index == time_not_found) then
                     if (allow_missing_file) then
                        time = ghost_time
                     else
                        _FAIL("Time not found in file")
                     end if
                  end if
               end if
               call bracket%set_node('L',file=current_file,time_index=time_index,time=time,was_set=.true.,_RC)
               bracket%new_file_left=.true.
            end if
         end if

         if (get_right) then
            call this%get_file(current_file,target_time,0,allow_missing_file,_RC)
            call this%get_time_on_file(current_file,target_time,'R',time_index,time,_RC)
            if (time_index == time_not_found) then
               call this%get_file(current_file,target_time,1,allow_missing_file,ghost_time,_RC)
               call this%get_time_on_file(current_file,target_time,'R',time_index,time,_RC)
               if (time_index == time_not_found) then
                  if (allow_missing_file) then
                     time = ghost_time
                  else
                     _FAIL("Time not found in file")
                  end if
               end if
            end if
            call bracket%set_node('R',file=current_file,time_index=time_index,time=time,was_set=.true.,_RC)
            bracket%new_file_right=.true.
         end if

      end if

      _RETURN(_SUCCESS)

      _UNUSED_DUMMY(source_time)

   end subroutine get_file_bracket

   subroutine get_file(this,filename,input_time,shift,allow_missing_file,ghost_time,rc)
      class(ExtdataSimpleFileHandler), intent(inout) :: this
      character(len=*), intent(out) :: filename
      type(ESMF_Time) :: input_time
      integer, intent(in) :: shift
      logical, intent(in) :: allow_missing_file
      type(ESMF_Time), intent(out), optional :: ghost_time
      integer, intent(out), optional :: rc

      type(ESMF_Time) :: ftime
      integer :: n,status
      logical :: file_found
      integer(ESMF_KIND_I8) :: interval_seconds

      call ESMF_TimeIntervalGet(this%frequency,s_i8=interval_seconds)
      if (interval_seconds==0) then
         ! time is not representable as absolute time interval (month, year etc...) do this
         ! brute force way. Not good but ESMF leaves no choice
         ftime=this%reff_time
         do while (ftime <= input_time)
            ftime = ftime + this%frequency
         enddo
         ftime=ftime -this%frequency + shift*this%frequency
      else
         n = (input_time-this%reff_time)/this%frequency
         ftime = this%reff_time+(n+shift)*this%frequency
      end if
      call fill_grads_template(filename,this%file_template,time=ftime,_RC)
      inquire(file=trim(filename),exist=file_found)
      if (.not.file_found) then
         if (allow_Missing_file) then
            filename = file_not_found
            if (present(ghost_time)) ghost_time = ftime
         else
            _FAIL("get_file did not file a file using: "//trim(this%file_template))
         end if
      end if
      _RETURN(_SUCCESS)

   end subroutine get_file

end module MAPL_ExtdataSimpleFileHandler
