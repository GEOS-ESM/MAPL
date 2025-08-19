#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
module mapl3g_NonClimDataSetFileSelector
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

   public NonClimDataSetFileSelector
 
   type, extends(AbstractDataSetFileSelector):: NonClimDataSetFileSelector
      logical :: persist_closest = .false.
      contains
         procedure :: update_file_bracket
         procedure :: in_valid_range
         procedure :: update_node
         procedure :: update_both_brackets
         procedure :: update_half_bracket
    end type

    interface NonClimDataSetFileSelector
       procedure new_NonClimDataSetFileSelector
    end interface
       
    contains

    function new_NonClimDataSetFileSelector(file_template, file_frequency, ref_time, timeStep, valid_range, persist_closest, rc) result(file_handler)
       type(NonClimDataSetFileSelector) :: file_handler
       character(len=*), intent(in) :: file_template
       type(ESMF_TimeInterval), intent(in), optional :: file_frequency
       type(ESMF_Time), intent(in), optional :: ref_time 
       type(ESMF_Time), intent(in), optional :: valid_range(:)
       type(ESMF_TimeInterval), intent(in), optional :: timeStep
       logical, intent(in), optional :: persist_closest
       integer, intent(out), optional :: rc

       integer :: status

       file_handler%file_template = file_template
       if ( index(file_handler%file_template,'%') == 0 ) file_handler%single_file = .true.
       file_handler%collection_id = mapl3g_AddDataCollection(file_handler%file_template)
       if (present(file_frequency)) file_handler%file_frequency = file_frequency
       if (present(ref_time)) file_handler%ref_time = ref_time
       if (present(valid_range)) then
          _ASSERT(size(valid_range)==2,"Valid range must be 2")
          file_handler%valid_range = valid_range
       end if
       if (present(persist_closest)) file_handler%persist_closest = persist_closest

       if (file_handler%persist_closest) then
          ! see if we can determine it if using didn't provide
          if ( (.not.allocated(file_handler%valid_range)) .and. file_handler%single_file) then
             call file_handler%get_valid_range_single_file(_RC)
          end if
          _ASSERT(allocated(file_handler%valid_range),'Asking for persistence but out of range')
       end if

       if (present(timeStep)) then
         file_handler%timeStep = timeStep
       end if

       
       _RETURN(_SUCCESS) 
    end function

    subroutine update_file_bracket(this, bundle, current_time, bracket, rc)
       class(NonClimDataSetFileSelector), intent(inout) :: this
       type(ESMF_FieldBundle), intent(inout) :: bundle
       type(ESMF_Time), intent(in) :: current_time
       type(DataSetBracket), intent(inout) :: bracket
       integer, optional, intent(out) :: rc

       type(ESMF_Time) :: target_time
       integer :: status, node_side
       logical :: establish_both, establish_single
       type(DataSetNode) :: left_node, right_node, test_node
       logical :: node_is_valid, both_valid, time_jumped, both_invalid

       establish_both = .true. 
       establish_single = .false.
       target_time = current_time
       if (this%persist_closest) then
          _ASSERT(allocated(this%valid_range), 'using persistence but not in range')
          if (.not. this%in_valid_range(target_time)) then
             establish_both = .false.
             if (current_time < this%valid_range(1)) then
                establish_single = .true.
                node_side = NODE_LEFT 
                target_time = this%valid_range(1)   
             else if (current_time >= this%valid_range(2)) then
                establish_single = .true.
                node_side = NODE_LEFT
                target_time = this%valid_range(2)       
             end if
          end if
       end if

       if (establish_single) then
          call this%update_half_bracket(bracket, target_time, current_time, node_side, _RC)
          _RETURN(_SUCCESS)
       end if

       _RETURN_UNLESS(establish_both)

       left_node = bracket%get_left_node(_RC)
       right_node = bracket%get_right_node(_RC)
       both_valid = left_node%validate(target_time) .and. right_node%validate(target_time) 
       time_jumped = this%detect_time_flow(current_time)
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

       call this%set_last_update(current_time, _RC)
       _RETURN(_SUCCESS)
    end subroutine update_file_bracket

    subroutine update_half_bracket(this, bracket, target_time, current_time, node_side, rc)
       class(NonClimDataSetFileSelector), intent(inout) :: this
       type(DataSetBracket), intent(inout) :: bracket
       type(ESMF_Time), intent(in) :: target_time
       type(ESMF_Time), intent(in) :: current_time 
       integer, intent(in) :: node_side
       integer, optional, intent(out) :: rc

       type(DataSetNode) :: active_node, inactive_node
       integer :: status
       logical :: node_is_valid
   
       select case(node_side)
       case(NODE_LEFT)
          active_node = bracket%get_left_node(_RC)
          inactive_node = bracket%get_right_node(_RC)
       case(NODE_RIGHT)
          inactive_node = bracket%get_left_node(_RC)
          active_node = bracket%get_right_node(_RC)
       end select

       call inactive_node%set_enabled(.false.)
       call inactive_node%set_update(.false.)
       call active_node%set_update(.false.)
       node_is_valid = active_node%validate(target_time)
       if (.not.node_is_valid) then
          call this%update_node(target_time, active_node, _RC)
       end if

       select case(node_side)
       case(NODE_LEFT)
          call bracket%set_parameters(left_node=active_node)
          call bracket%set_parameters(right_node=inactive_node)
       case(NODE_RIGHT)
          call bracket%set_parameters(left_node=inactive_node)
          call bracket%set_parameters(right_node=active_node)
       end select
       call this%set_last_update(current_time, _RC)

       _RETURN(_SUCCESS)
    end subroutine update_half_bracket

    subroutine update_both_brackets(this, bracket, target_time, rc)
       class(NonClimDataSetFileSelector), intent(inout) :: this
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
       class(NonClimDataSetFileSelector), intent(inout) :: this
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
       class(NonClimDataSetFileSelector), intent(inout) :: this
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

end module mapl3g_NonClimDataSetFileSelector
   
