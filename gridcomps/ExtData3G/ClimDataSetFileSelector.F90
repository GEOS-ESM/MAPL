#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
module mapl3g_ClimDataSetFileSelector
   use ESMF
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   use mapl3g_DataSetBracket
   use mapl3g_DataSetNode
   use mapl3g_AbstractDataSetFileSelector
   use mapl3g_ExtdataUtilities
   use mapl_StringTemplate
   use mapl3g_geomio
   use mapl3g_FieldBundle_API
   use MAPL_FieldUtils
   implicit none
   private

   public ClimDataSetFileSelector

   integer, parameter :: CLIM_NULL = -100000 
   type, extends(AbstractDataSetFileSelector):: ClimDataSetFileSelector
      type(ESMF_Time), allocatable :: source_time(:)
      integer :: clim_year = CLIM_NULL
      contains
         procedure :: update_file_bracket
         procedure :: in_valid_range
         procedure :: update_node
         procedure :: update_both_brackets
         procedure :: update_bracket_in_range
         procedure :: update_bracket_out_of_range
    end type

    interface ClimDataSetFileSelector
       procedure new_ClimDataSetFileSelector
    end interface
       
    contains

    function new_ClimDataSetFileSelector(file_template, valid_range, file_frequency, ref_time, timeStep, source_time, rc) result(file_handler)
       type(ClimDataSetFileSelector) :: file_handler
       character(len=*), intent(in) :: file_template
       type(ESMF_Time), optional, intent(in) :: valid_range(:)
       type(ESMF_TimeInterval), intent(in), optional :: file_frequency
       type(ESMF_Time), intent(in), optional :: ref_time 
       type(ESMF_TimeInterval), intent(in), optional :: timeStep
       type(ESMF_Time), intent(in), optional :: source_time(:)
       integer, intent(out), optional :: rc

       integer :: status

       file_handler%file_template = file_template
       if ( index(file_handler%file_template,'%') == 0 ) file_handler%single_file = .true.
       file_handler%collection_id = mapl3g_AddDataCollection(file_handler%file_template)

       if (present(valid_range)) then
          _ASSERT(size(valid_range)==2,"Valid range must be 2")
          file_handler%valid_range = valid_range
       else
          call file_handler%get_valid_range_single_file(_RC)
       end if

       if (present(file_frequency)) file_handler%file_frequency = file_frequency
       if (present(ref_time)) file_handler%ref_time = ref_time

       if (present(timeStep)) then
          file_handler%timeStep = timeStep
       end if

       if (present(source_time)) then
          _ASSERT(size(source_time) == 2, 'Source time must be of size 2')
          file_handler%source_time = source_time 
       end if
       
       _RETURN(_SUCCESS) 
    end function

    subroutine update_file_bracket(this, bundle, current_time, bracket, rc)
       class(ClimDataSetFileSelector), intent(inout) :: this
       type(ESMF_FieldBundle), intent(inout) :: bundle
       type(ESMF_Time), intent(in) :: current_time
       type(DataSetBracket), intent(inout) :: bracket
       integer, optional, intent(out) :: rc

       type(ESMF_Time) :: target_time
       integer :: status, node_side, valid_years(2)
       type(DataSetNode) :: left_node, right_node, test_node
       logical :: node_is_valid, both_valid, time_jumped, both_invalid

       _HERE,'bmaa '
       target_time = current_time
       _ASSERT(size(this%valid_range) == 2, 'Valid range must be of size 2 to do climatological extrpolation')
       call ESMF_TimeGet(this%valid_range(1),yy=valid_years(1),_RC)
       call ESMF_TimeGet(this%valid_range(2),yy=valid_years(2),_RC)
       if (size(this%source_time)==2) then
          _ASSERT(this%source_time(1) >= this%valid_range(1),'source time outside valid range')
          _ASSERT(this%source_time(1) <=  this%valid_range(2),'source time outside valid range')
          _ASSERT(this%source_time(2) >=  this%valid_range(1),'source time outside valid range')
          _ASSERT(this%source_time(2) <= this%valid_range(2),'source time outside valid range')
       end if

       if (target_time <= this%valid_range(1)) then
          this%clim_year = valid_years(1)
          call swap_year(target_time, this%clim_year, _RC)
       else if (target_time >= this%valid_range(2)) then
          this%clim_year = valid_years(2)
          call swap_year(target_time, this%clim_year, _RC)
       end if

       call ESMF_TimePrint(target_time, options='string', prestring='bmaa target time: ')
       _HERE,'bmaa '
       if (this%clim_year == CLIM_NULL) then
          call this%update_bracket_in_range(bundle, target_time, bracket, _RC)
       else
          call this%update_bracket_out_of_range(bundle, target_time, bracket, _RC)
       end if
       call this%set_last_update(current_time, _RC)
       _RETURN(_SUCCESS)
    end subroutine update_file_bracket

    subroutine update_bracket_in_range(this, bundle, target_time, bracket, rc)
       class(ClimDataSetFileSelector), intent(inout) :: this
       type(ESMF_FieldBundle), intent(inout) :: bundle
       type(ESMF_Time), intent(in) :: target_time 
       type(DataSetBracket), intent(inout) :: bracket
       integer, optional, intent(out) :: rc

       integer :: status, node_side
       logical :: establish_both
       type(DataSetNode) :: left_node, right_node, test_node
       logical :: node_is_valid, both_valid, time_jumped, both_invalid

       _HERE,'bmaa '
       left_node = bracket%get_left_node(_RC)
       right_node = bracket%get_right_node(_RC)
       both_valid = left_node%validate(target_time) .and. right_node%validate(target_time) 
       time_jumped = this%detect_time_flow(target_time)
       both_invalid = (left_node%validate(target_time) .eqv. .false.) .and. &
                      (right_node%validate(target_time) .eqv. .false.)

       if (time_jumped .or. both_invalid) then ! if time moved more than 1 clock dt, force update
          call this%update_both_brackets(bracket, target_time, _RC)
       else if (both_valid) then ! else if it did not, both still valid, don't update
          call left_node%set_update(.false.)
          call right_node%set_update(.false.)
          call bracket%set_parameters(left_node=left_node)
          call bracket%set_parameters(right_node=right_node)
       else ! finally need to update one of them, try swapping right to left and update left
          test_node = right_node
          call test_node%set_node_side(NODE_LEFT)
          node_is_valid = test_node%validate(target_time)
          if (node_is_valid) then
             left_node = test_node 
             call left_node%set_update(.false.)
             call bracket%set_parameters(left_node=left_node)
             call this%update_node(target_time, right_node, _RC)
             call bracket%set_parameters(right_node=right_node)
             call swap_bracket_fields(bundle, _RC)
          else 
             call this%update_both_brackets(bracket, target_time, _RC)
          end if
       end if

       _RETURN(_SUCCESS)

    end subroutine update_bracket_in_range

    subroutine update_bracket_out_of_range(this, bundle, target_time, bracket, rc)
       class(ClimDataSetFileSelector), intent(inout) :: this
       type(ESMF_FieldBundle), intent(inout) :: bundle
       type(ESMF_Time), intent(in) :: target_time 
       type(DataSetBracket), intent(inout) :: bracket
       integer, optional, intent(out) :: rc

       integer :: status, node_side
       logical :: establish_both
       type(DataSetNode) :: left_node, right_node, test_node
       logical :: node_is_valid, both_valid, time_jumped, both_invalid

       _HERE,'bmaa '
       left_node = bracket%get_left_node(_RC)
       right_node = bracket%get_right_node(_RC)
       both_valid = left_node%validate(target_time) .and. right_node%validate(target_time) 
       time_jumped = this%detect_time_flow(target_time)
       both_invalid = (left_node%validate(target_time) .eqv. .false.) .and. &
                      (right_node%validate(target_time) .eqv. .false.)

       if (time_jumped .or. both_invalid) then ! if time moved more than 1 clock dt, force update
          call this%update_both_brackets(bracket, target_time, _RC)
       else if (both_valid) then ! else if it did not, both still valid, don't update
          call left_node%set_update(.false.)
          call right_node%set_update(.false.)
          call bracket%set_parameters(left_node=left_node)
          call bracket%set_parameters(right_node=right_node)
       else ! finally need to update one of them, try swapping right to left and update left
          test_node = right_node
          call test_node%set_node_side(NODE_LEFT)
          node_is_valid = test_node%validate(target_time)
          if (node_is_valid) then
             left_node = test_node 
             call left_node%set_update(.false.)
             call bracket%set_parameters(left_node=left_node)
             call this%update_node(target_time, right_node, _RC)
             call bracket%set_parameters(right_node=right_node)
             call swap_bracket_fields(bundle, _RC)
          else 
             call this%update_both_brackets(bracket, target_time, _RC)
          end if
       end if

       _RETURN(_SUCCESS)

    end subroutine update_bracket_out_of_range

    subroutine update_both_brackets(this, bracket, target_time, rc)
       class(ClimDataSetFileSelector), intent(inout) :: this
       type(DataSetBracket), intent(inout) :: bracket
       type(ESMF_Time), intent(in) :: target_time
       integer, optional, intent(out) :: rc

       type(DataSetNode) :: left_node, right_node
       integer :: status

       left_node = bracket%get_left_node(_RC)
       right_node = bracket%get_right_node(_RC)
       call this%update_node(target_time, left_node, _RC)
       call bracket%set_parameters(left_node=left_node)
       call this%update_node(target_time, right_node, _RC)
       call bracket%set_parameters(right_node=right_node)
       _RETURN(_SUCCESS)
    end subroutine update_both_brackets

    subroutine update_node(this, current_time, node, rc)
       class(ClimDataSetFileSelector), intent(inout) :: this
       type(ESMF_Time), intent(in) :: current_time
       type(DataSetNode), intent(inout) :: node
       integer, optional, intent(out) :: rc
       
       integer :: status, local_search_stop, step,  node_side, i
       type(ESMF_Time) :: trial_time
       character(len=ESMF_MAXPATHLEN) :: trial_file
       logical :: file_found, valid_node

       node_side = node%get_node_side()
       select case(node_side)
       case (NODE_LEFT)
            local_search_stop = -NUM_SEARCH_TRIES
            step = -1
       case (NODE_RIGHT)
            local_search_stop = NUM_SEARCH_TRIES
            step = 1
       end select
       valid_node = .false.
       do i=0,local_search_stop,step
          trial_time = this%compute_trial_time(current_time, i, _RC)
          call fill_grads_template(trial_file, this%file_template, time=trial_time, _RC)
          inquire(file=trial_file, exist=file_found)
          if (file_found) then 
             call node%invalidate()
             call node%update_node_from_file(trial_file, current_time, _RC)
             valid_node = node%validate(current_time, _RC)
             _RETURN_IF(valid_node)
          end if
       enddo
       _FAIL("Could not find a valid node")
    end subroutine update_node

    function in_valid_range(this, target_time) result(target_in_valid_range)
       logical :: target_in_valid_range
       class(ClimDataSetFileSelector), intent(inout) :: this
       type(ESMF_Time), intent(in) :: target_time
 
       target_in_valid_range = (this%valid_range(1) < target_time) .and. (target_time < this%valid_range(2)) 
    end function
  
    subroutine swap_bracket_fields(bundle, rc)
       type(ESMF_FieldBundle), intent(inout) :: bundle
       integer, optional, intent(out) :: rc

       integer :: status
       type(ESMF_Field), allocatable :: field_list(:)
 
       call MAPL_FieldBundleGet(bundle, fieldList=field_list, _RC)
       call FieldCopy(field_list(2), field_list(1), _RC)

       _RETURN(_SUCCESS)
    end subroutine swap_bracket_fields

    subroutine swap_year(time,year,rc)
       type(ESMF_Time), intent(inout) :: time
       integer, intent(in) :: year
       integer, optional, intent(out) :: rc
       logical :: is_leap_year
       type(ESMF_Calendar) :: calendar
       integer :: status, month, day, hour, minute, second

       is_leap_year=.false.
       call ESMF_TimeGet(time,mm=month,dd=day,h=hour,m=minute,s=second,calendar=calendar,_RC)
       if (day==29 .and. month==2) then
          is_leap_year = ESMF_CalendarIsLeapYear(calendar,year,_RC)
          if (.not.is_leap_year) day=28
       end if
       call ESMF_TimeSet(time,yy=year,mm=month,dd=day,h=hour,m=minute,s=second,_RC)
       _RETURN(_SUCCESS)
   end subroutine

end module mapl3g_ClimDataSetFileSelector
   
