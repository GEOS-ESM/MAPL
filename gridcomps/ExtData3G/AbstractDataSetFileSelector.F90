#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
module mapl3g_AbstractDataSetFileSelector
   use ESMF
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   use mapl3g_DataSetBracket
   use mapl_StringTemplate
   use mapl_FileMetadataUtilsMod
   use mapl3g_geomio
   use mapl3g_ExtDataConstants
   implicit none
   private

   public AbstractDataSetFileSelector
   public NUM_SEARCH_TRIES

   integer, parameter :: MAX_TRIALS = 10
   integer, parameter :: NUM_SEARCH_TRIES = 1
 
   type, abstract :: AbstractDataSetFileSelector
      character(:), allocatable :: file_template
      type(ESMF_TimeInterval)  :: file_frequency
      type(ESMF_Time) :: ref_time
      type(ESMF_Time), allocatable :: valid_range(:)
      type(ESMF_Time), allocatable :: last_updated
      type(ESMF_TimeInterval), allocatable :: timeStep 
      integer :: collection_id
      logical :: single_file = .false.
      contains
         procedure :: find_any_file
         procedure :: compute_trial_time
         procedure :: set_last_update
         procedure :: detect_time_flow
         procedure :: get_dataset_metadata
         procedure :: get_file_template
         procedure :: get_valid_range_single_file
         procedure(I_update_file_bracket), deferred :: update_file_bracket
    end type

    abstract interface
       subroutine I_update_file_bracket(this, bundle, current_time, bracket, rc)
          use ESMF, only: ESMF_Time, ESMF_FieldBundle
          use mapl3g_DataSetBracket
          import AbstractDataSetFileSelector
          class(AbstractDataSetFileSelector), intent(inout) :: this
          type(ESMF_FieldBundle), intent(inout) :: bundle
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
             _RETURN(_SUCCESS)
          end if
       enddo
       _FAIL("could not find a file") 

    end function find_any_file

    function get_dataset_metadata(this, rc) result(metadata)
       type(FileMetadataUtils), pointer :: metadata
       class(AbstractDataSetFileSelector), intent(inout) :: this
       integer, optional, intent(out) :: rc

       character(len=:), allocatable :: filename
       integer :: status 
       type(DataCollection), pointer :: collection
      
       filename = this%find_any_file(_RC) 
       collection => DataCollections%at(this%collection_id)
       metadata => collection%find(filename, _RC)
       _RETURN(_SUCCESS)
    end function

    function compute_trial_time(this, target_time, shift, rc) result(trial_time)
       type(ESMF_Time) :: trial_time
       class(AbstractDataSetFileSelector), intent(inout) :: this
       type(ESMF_Time), intent(in) :: target_time
       integer, intent(in) :: shift
       integer, optional, intent(out) :: rc

       integer :: status, n
       integer(ESMF_KIND_I8) :: int_sec
      
       if (this%single_file) then
          trial_time = target_time
          _RETURN(_SUCCESS)
       end if
 
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

    subroutine get_file_template(this, file_template)
       class(AbstractDataSetFileSelector), intent(in) :: this
       character(len=:), allocatable :: file_template
 
       if (allocated(this%file_template)) file_template = this%file_template
    end subroutine get_file_template

    subroutine get_valid_range_single_file(this, rc)
       class(AbstractDataSetFileSelector), intent(inout) :: this
       integer, intent(out), optional :: rc

       type(DataCollection), pointer :: collection
       type(FileMetadataUtils), pointer :: metadata
       type(ESMF_Time), allocatable :: time_series(:)
       integer :: status

       allocate(this%valid_range(2), _STAT)
       collection => DataCollections%at(this%collection_id)
       metadata => collection%find(this%file_template)
       call metadata%get_time_info(timeVector=time_series, _RC) 
       this%valid_range(1)=time_series(1)
       this%valid_range(2)=time_series(size(time_series))
  
       _RETURN(_SUCCESS)

    end subroutine get_valid_range_single_file 

end module mapl3g_AbstractDataSetFileSelector
   
