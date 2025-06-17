#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
module mapl3g_AbstractDataSetFileSelector
   use ESMF
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   use mapl3g_DataSetBracket
   use mapl_StringTemplate
   implicit none
   private

   public AbstractDataSetFileSelector
   public file_not_found
   public NUM_SEARCH_TRIES

   integer, parameter :: MAX_TRIALS = 10
   integer, parameter :: NUM_SEARCH_TRIES = 1
   character(len=*), parameter :: file_not_found = "NONE"
 
   type, abstract :: AbstractDataSetFileSelector
      character(:), allocatable :: file_template
      type(ESMF_TimeInterval)  :: file_frequency
      type(ESMF_Time) :: ref_time
      type(ESMF_Time), allocatable :: valid_range(:)
      type(ESMF_Time), allocatable :: last_updated
      type(ESMF_TimeInterval), allocatable :: timeStep 
      contains
         procedure :: find_any_file
         procedure :: compute_trial_time
         procedure :: set_last_update
         procedure :: detect_time_flow
         procedure(I_update_file_bracket), deferred :: update_file_bracket
    end type

    abstract interface
       subroutine I_update_file_bracket(this, current_time, bracket, rc)
          use ESMF, only: ESMF_Time
          use mapl3g_DataSetBracket
          import AbstractDataSetFileSelector
          class(AbstractDataSetFileSelector), intent(inout) :: this
          type(ESMF_Time), intent(in) :: current_time
          type(DataSetBracket), intent(inout) :: bracket
          integer, optional, intent(out) :: rc
       end subroutine I_update_file_bracket
    end interface

    contains

    function find_any_file(this, rc) result(filename)
       character(len=:), allocatable :: filename
       class(AbstractDataSetFileSelector), intent(inout) :: this
       integer, optional, intent(out) :: rc


       integer :: status, i
       type(ESMF_Time) :: useable_time
       character(len=ESMF_MAXPATHLEN) :: trial_file
       logical :: file_found

       filename = file_not_found
       useable_time = this%ref_time
       call fill_grads_template(trial_file, this%file_template, time=useable_time, _RC)
       inquire(file=trim(trial_file),exist=file_found)
       if (file_found) then
          filename = trial_file
          _RETURN(_SUCCESS)
       end if
       do i=1, MAX_TRIALS
          useable_time = useable_time + this%file_frequency
          call fill_grads_template(trial_file, this%file_template, time=useable_time, _RC)
          inquire(file=trim(trial_file),exist=file_found)
          if (file_found) then
             filename = trial_file
             exit
          end if
       enddo
       _RETURN(_SUCCESS)

    end function find_any_file

    function compute_trial_time(this, target_time, shift, rc) result(trial_time)
       type(ESMF_Time) :: trial_time
       class(AbstractDataSetFileSelector), intent(inout) :: this
       type(ESMF_Time), intent(in) :: target_time
       integer, intent(in) :: shift
       integer, optional, intent(out) :: rc

       integer :: status, n
       integer(ESMF_KIND_I8) :: int_sec
       
       call ESMF_TimeIntervalGet(this%file_frequency, s_i8=int_sec, _RC)
       if (int_sec == 0) then
          trial_time = this%ref_time
          do while(trial_time <= target_time)
             trial_time = trial_time + this%file_frequency
          enddo
          trial_time = trial_time - this%file_frequency + shift*this%file_frequency
       else
          n = (target_time-this%ref_time)/this%file_frequency
          trial_time = this%ref_time+(n+shift)*this%file_frequency
       end if
       _RETURN(_SUCCESS)
       
    end function compute_trial_time 

    subroutine set_last_update(this, update_time, rc)
       class(AbstractDataSetFileSelector), intent(inout) :: this
       type(ESMF_Time), intent(in) :: update_time
       integer, optional, intent(out) :: rc

       integer :: status
       this%last_updated = update_time
       _RETURN(_SUCCESS)
    end subroutine

    function detect_time_flow(this, current_time, rc) result(time_jumped)
       logical :: time_jumped
       class(AbstractDataSetFileSelector), intent(inout) :: this
       type(ESMF_Time), intent(in) :: current_time
       integer, optional, intent(inout) :: rc

       integer :: status
       type(ESMF_TimeInterval) :: time_interval
       integer(ESMF_KIND_I8) :: f1, f2

       time_jumped = .false.
       _RETURN_UNLESS(allocated(this%last_updated) .and. allocated(this%timeStep))
       time_interval = current_time - this%last_updated
       call ESMF_TimeIntervalGet(time_interval, s_i8=f1, _RC)
       call ESMF_TimeIntervalGet(this%timeStep, s_i8=f2, _RC)
       time_jumped = abs(f1) > f2
       _RETURN(_SUCCESS)
    end function 

end module mapl3g_AbstractDataSetFileSelector
   
