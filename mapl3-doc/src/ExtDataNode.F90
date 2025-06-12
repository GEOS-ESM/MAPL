#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
module mapl3g_ExtDataNode
   use ESMF
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   implicit none
   private

   public :: ExtDataNode

   type :: ExtDataNode
      type(ESMF_Time)  :: interp_time
      type(ESMF_Time)  :: file_time
      character(len=:), allocatable :: file
      integer :: time_index 
      contains
         procedure :: set_file_time
         procedure :: set_interp_time
         procedure :: set_time_index
         procedure :: set_file
         procedure :: get_file_time
         procedure :: get_interp_time
         procedure :: get_time_index
         procedure :: get_file
         procedure :: equals
         generic :: operator(==) => equals
   end type

   interface ExtDataNode
      procedure new_ExtDataNode
   end interface

contains

   function new_ExtDataNode(file, time_index, file_time, interp_time) result(node)
      type(ExtDataNode) :: node
      character(len=*), intent(in) :: file
      integer, intent(in) :: time_index
      type(ESMF_Time), intent(in) :: file_time
      type(ESMF_Time), intent(in) :: interp_time

      node%file_time = file_time
      node%interp_time = interp_time
      node%file = trim(file)
      node%time_index = time_index
      
   end function new_ExtDataNode

   subroutine set_file_time(this, file_time)
      class(ExtDataNode), intent(inout) :: this
      type(ESMF_Time), intent(in) :: file_time
      this%file_time=file_time
   end subroutine

   subroutine set_interp_time(this, interp_time)
      class(ExtDataNode), intent(inout) :: this
      type(ESMF_Time), intent(in) :: interp_time
      this%interp_time=interp_time
   end subroutine

   subroutine set_file(this, file)
      class(ExtDataNode), intent(inout) :: this
      character(len=*), intent(in) :: file
      this%file=file
   end subroutine

   subroutine set_time_index(this, time_index)
      class(ExtDataNode), intent(inout) :: this
      integer, intent(in) :: time_index
      this%time_index=time_index
   end subroutine

   function get_file_time(this) result(file_time)
      type(ESMF_Time) :: file_time
      class(ExtDataNode), intent(inout) :: this
      file_time=this%file_time
   end function

   function get_interp_time(this) result(interp_time)
      type(ESMF_Time) :: interp_time
      class(ExtDataNode), intent(inout) :: this
      interp_time=this%interp_time
   end function

   function get_file(this) result(file)
      character(len=:), allocatable :: file
      class(ExtDataNode), intent(inout) :: this
      file=this%file
   end function

   function get_time_index(this) result(time_index)
      integer :: time_index
      class(ExtDataNode), intent(inout) :: this
      time_index=this%time_index
   end function

   logical function equals(a,b)
      class(ExtDataNode), intent(in) :: a
      class(ExtDataNode), intent(in) :: b

      equals = (trim(a%file)==trim(b%file)) .and. (a%file_time==b%file_time) .and. (a%time_index==b%time_index) .and. (a%interp_time==b%interp_time)
   end function equals

end module mapl3g_ExtDataNode
