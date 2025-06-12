#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
module mapl3g_SimpleFileHandler
   use ESMF
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   use mapl3g_ExtDataBracket
   use mapl3g_ExtDataNode
   use mapl3g_AbstractFileHandler
   use mapl3g_ExtdataUtilities
   use mapl_StringTemplate
   implicit none
   private

   public SimpleFileHandler
 
   type, extends(AbstractFileHandler):: SimpleFileHandler
      logical :: persist_closest = .false.
      contains
         procedure :: update_file_bracket
         procedure :: not_in_range
         procedure :: update_node
    end type

    interface SimpleFileHandler
       procedure new_SimpleFileHandler
    end interface
       
    contains

    function new_SimpleFileHandler(file_template, frequency, ref_time, valid_range, persist_closest, enable_interpolation, rc) result(file_handler)
       type(SimpleFileHandler) :: file_handler
       character(len=*), intent(in) :: file_template
       type(ESMF_TimeInterval), intent(in), optional :: frequency
       type(ESMF_Time), intent(in), optional :: ref_time 
       type(ESMF_Time), intent(in), optional :: valid_range(:)
       logical, intent(in), optional :: persist_closest
       logical, intent(in), optional :: enable_interpolation
       integer, intent(out), optional :: rc

       integer :: status
       file_handler%file_template = file_template
       if (present(frequency)) file_handler%frequency = frequency
       if (present(ref_time)) file_handler%ref_time = ref_time
       if (present(valid_range)) then
          _ASSERT(size(valid_range)==2,"Valid range must be 2")
          allocate(file_handler%valid_range, source=valid_range, _STAT)
       end if
       if (present(persist_closest)) file_handler%persist_closest = persist_closest

       if (file_handler%persist_closest) then
          _ASSERT(allocated(file_handler%valid_range),'Asking for persistence but out of range')
       end if
       
       _RETURN(_SUCCESS) 
    end function

    subroutine update_file_bracket(this, current_time, bracket,  rc)
       class(SimpleFileHandler), intent(inout) :: this
       type(ESMF_Time), intent(in) :: current_time
       type(ExtDataBracket), intent(inout) :: bracket
       integer, optional, intent(out) :: rc

       type(ESMF_Time) :: target_time
       integer :: status
       logical :: establish_both, establish_left, establish_right
       type(ExtDataNode) :: left_node, right_node
       logical :: node_is_valid
       character(len=:), allocatable :: node_file

       establish_both = .true. 
       establish_left = .false.
       establish_right = .false.
       target_time = current_time
       if (this%persist_closest .and. this%not_in_range(target_time)) then
          establish_both = .false.
          if (current_time < this%valid_range(1)) then
             establish_right = .true.
             target_time = this%valid_range(1)       
          else if (current_time > this%valid_range(2)) then
             establish_left = .true.
             target_time = this%valid_range(2)       
          end if
       end if

       if (establish_left) then
          right_node = bracket%get_right_node(_RC)
          call right_node%set_enabled(.false.)
          call bracket%set_parameters(right_node=right_node)
          left_node = bracket%get_left_node(_RC)
          node_is_valid = left_node%validate(current_time)
          if (.not.node_is_valid) then
             call this%update_node(current_time, left_node, _RC)
          end if
          node_is_valid = left_node%validate(current_time)
          _ASSERT(node_is_valid, "left node not updated")
          call bracket%set_parameters(left_node=left_node)
       end if

       if (establish_right) then
          left_node = bracket%get_left_node(_RC)
          call left_node%set_enabled(.false.)
          call bracket%set_parameters(left_node=left_node)
          right_node = bracket%get_right_node(_RC)
          node_is_valid = right_node%validate(current_time)
          if (.not.node_is_valid) then
             call this%update_node(current_time, right_node, _RC)
          end if
          node_is_valid = right_node%validate(current_time)
          _ASSERT(node_is_valid, "right node not updated")
          call bracket%set_parameters(right_node=right_node)
       end if

       _RETURN(_SUCCESS)

    end subroutine update_file_bracket

    subroutine update_node(this, current_time, node, rc)
       class(SimpleFileHandler), intent(inout) :: this
       type(ESMF_Time), intent(in) :: current_time
       type(ExtDataNode), intent(inout) :: node
       integer, optional, intent(out) :: rc
       
       integer :: status, local_search_stop, step,  node_side, i
       type(ESMF_Time) :: trial_time
       character(len=:), allocatable :: trial_file
       logical :: file_found, valid_node

       node_side = node%get_node_side()
       select case(node_side)
       case (left_node)
            local_search_stop = -NUM_SEARCH_TRIES
            step = -1
       case (right_node)
            local_search_stop = NUM_SEARCH_TRIES
            step = 1
       end select
       do i=0,local_search_stop,step
          trial_time = this%compute_trial_time(current_time, i, _RC)
          call fill_grads_template(trial_file, this%file_template, time=trial_time, _RC)
          inquire(file=trial_file, exist=file_found)
          if (file_found) then 
             call node%invalidate()
             call this%update_node_from_file(trial_file, current_time, node, _RC)
             valid_node = node%validate(current_time, _RC)
             if (valid_node) exit 
          end if
       enddo

       _RETURN(_SUCCESS)
    end subroutine update_node

    function not_in_range(this, target_time) result(target_in_range)
       logical :: target_in_range
       class(SimpleFileHandler), intent(inout) :: this
       type(ESMF_Time), intent(in) :: target_time
  
       target_in_range = ((target_time < this%valid_range(1)) .or. (this%valid_range(2) < target_time))
    end function
       

end module mapl3g_SimpleFileHandler
   
