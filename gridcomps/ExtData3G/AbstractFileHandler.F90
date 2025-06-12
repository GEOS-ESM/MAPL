#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
module mapl3g_AbstractFileHandler
   use ESMF
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   use mapl3g_ExtDataBracket
   use mapl3g_ExtDataNode
   use mapl_StringTemplate
   use pFIO
   use MAPL_FileMetadataUtilsMod
   implicit none
   private

   public AbstractFileHandler
   public file_not_found
   public NUM_SEARCH_TRIES

   integer, parameter :: MAX_TRIALS = 10
   integer, parameter :: NUM_SEARCH_TRIES = 1
   character(len=*), parameter :: file_not_found = "NONE"
 
   type, abstract :: AbstractFileHandler
      character(:), allocatable :: file_template
      type(ESMF_TimeInterval)  :: frequency
      type(ESMF_Time) :: ref_time
      type(ESMF_Time), allocatable :: valid_range(:)
      logical :: enable_interpolation = .true.
      contains
         procedure :: find_any_file
         procedure :: compute_trial_time
         procedure :: update_node_from_file
         procedure(I_update_file_bracket), deferred :: update_file_bracket
    end type

    abstract interface
       subroutine I_update_file_bracket(this, current_time, bracket, rc)
          use ESMF, only: ESMF_Time
          use mapl3g_ExtDataBracket
          import AbstractFileHandler
          class(AbstractFileHandler), intent(inout) :: this
          type(ESMF_Time), intent(in) :: current_time
          type(ExtDataBracket), intent(inout) :: bracket
          integer, optional, intent(out) :: rc
       end subroutine I_update_file_bracket
    end interface

    contains

    function find_any_file(this, rc) result(filename)
       character(len=:), allocatable :: filename
       class(AbstractFileHandler), intent(inout) :: this
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
          useable_time = useable_time + this%frequency
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
       class(AbstractFileHandler), intent(inout) :: this
       type(ESMF_Time), intent(in) :: target_time
       integer, intent(in) :: shift
       integer, optional, intent(out) :: rc

       integer :: status, n
       integer(ESMF_KIND_I8) :: int_sec
       call ESMF_TimeIntervalGet(this%frequency, s_i8=int_sec, _RC)
       if (int_sec == 0) then
          trial_time = this%ref_time
          do while(trial_time <= target_time)
             trial_time = trial_time + this%frequency
        
          enddo
          trial_time = trial_time - this%frequency + shift*this%frequency
       else
          n = (target_time-this%ref_time)/this%frequency
          trial_time = this%ref_time+shift*this%frequency 
       end if
       _RETURN(_SUCCESS)
       
    end function compute_trial_time 

    subroutine update_node_from_file(this, filename, target_time, node, rc)
       class(AbstractFileHandler), intent(inout) :: this
       character(len=*), intent(in) :: filename
       type(ESMF_Time), intent(in) :: target_time
       type(ExtDataNode), intent(inout) :: node
       integer, optional, intent(out) :: rc
      
       integer :: status, node_side, i
       type(FileMetaDataUtils) :: metadata
       type(FileMetadata) :: basic_metadata
       type(NetCDF4_FileFormatter) :: formatter
       type(ESMF_Time), allocatable :: time_vector(:)

       node_side = node%get_node_side()
       _ASSERT(node_side/=unknown_node, "node does not have a side")
       call formatter%open(filename, pFIO_READ, _RC)
       basic_metadata = formatter%read(_RC)
       call formatter%close()
       metadata = FileMetadataUtils(basic_metadata, filename)

       call metadata%get_time_info(timeVector=time_vector, _RC)
       select case(node_side)
       case (left_node)
          do i=size(time_vector),1,-1
             if (target_time >= time_vector(i)) then
                call node%set_file(filename)
                call node%set_interp_time(time_vector(i))
                call node%set_file_time(time_vector(i))
                call node%set_time_index(i)
                call node%set_enabled(.true.)
                call node%set_update(.true.)
                exit
             end if 
          enddo
       case (right_node)
          do i=1,size(time_vector)
             if (target_time < time_vector(i)) then
                call node%set_file(filename)
                call node%set_interp_time(time_vector(i))
                call node%set_file_time(time_vector(i))
                call node%set_time_index(i)
                call node%set_enabled(.true.)
                call node%set_update(.true.)
                exit
             end if
          enddo
       end select

       _RETURN(_SUCCESS)
    end subroutine

end module mapl3g_AbstractFileHandler
   
