#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
module mapl3g_ExtDataNode
   use ESMF
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   use pFIO
   use MAPL_FileMetadataUtilsMod
   implicit none
   private

   public :: ExtDataNode
   public :: left_node
   public :: right_node
   public :: unknown_node
 
   enum, bind(c)
      enumerator :: left_node
      enumerator :: right_node
      enumerator :: unknown_node
   end enum      

   type :: ExtDataNode
      logical :: update = .false.
      integer :: node_side
      logical :: enabled = .false.
      type(ESMF_Time) :: interp_time
      type(ESMF_Time) :: file_time
      character(len=:), allocatable :: file
      integer :: time_index 
      contains
         procedure :: set_file_time
         procedure :: set_interp_time
         procedure :: set_time_index
         procedure :: set_file
         procedure :: set_node_side
         procedure :: set_update
         procedure :: set_enabled
         procedure :: get_file_time 
         procedure :: get_interp_time
         procedure :: get_time_index
         procedure :: get_file
         procedure :: get_node_side
         procedure :: get_update
         procedure :: get_enabled
         procedure :: equals
         procedure :: validate
         procedure :: invalidate
         procedure :: update_node_from_file
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

   subroutine set_node_side(this, node_side)
      class(ExtDataNode), intent(inout) :: this
      integer, intent(in) :: node_side
      this%node_side = node_side
   end subroutine

   subroutine set_enabled(this, enabled)
      class(ExtDataNode), intent(inout) :: this
      logical, intent(in) :: enabled
      this%enabled = enabled 
   end subroutine

   subroutine set_update(this, update)
      class(ExtDataNode), intent(inout) :: this
      logical, intent(in) :: update
      this%update = update 
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

   function get_node_side(this) result(node_side)
      integer :: node_side
      class(ExtDataNode), intent(inout) :: this
      node_side=this%node_side
   end function

   function get_update(this) result(update)
      logical :: update
      class(ExtDataNode), intent(inout) :: this
      update=this%update
   end function

   function get_enabled(this) result(enabled)
      logical :: enabled
      class(ExtDataNode), intent(inout) :: this
      enabled=this%enabled
   end function

   logical function equals(a,b)
      class(ExtDataNode), intent(in) :: a
      class(ExtDataNode), intent(in) :: b

      equals = (trim(a%file)==trim(b%file)) .and. (a%file_time==b%file_time) .and. (a%time_index==b%time_index) .and. (a%interp_time==b%interp_time)
   end function equals

   subroutine reset(this)
      class(ExtDataNode), intent(inout) :: this
      deallocate(this%file)
      this%enabled = .false.
      this%update = .false.
   end subroutine

   function validate(this, current_time, rc) result(node_is_valid)
      logical :: node_is_valid
      class(ExtDataNode), intent(inout) :: this
      type(ESMF_Time), intent(in) :: current_time
      integer, intent(out), optional :: rc

      integer :: status
      if (.not.allocated(this%file)) then
         node_is_valid = .false.
         _RETURN(_SUCCESS)
      end if
      if (this%node_side == unknown_node ) then
         node_is_valid = .false.
         _RETURN(_SUCCESS)
      end if
      if (this%node_side == left_node) then
         node_is_valid = (current_time >= this%file_time)
      else if (this%node_side == right_node) then
         node_is_valid = (current_time < this%file_time)
      end if
      _RETURN(_SUCCESS)
   end function

   subroutine invalidate(this)
      class(ExtDataNode), intent(inout) :: this
      if (allocated(this%file)) then
         deallocate(this%file) 
      end if
      this%enabled = .false.
      this%update = .false.
   end subroutine

   subroutine update_node_from_file(this, filename, target_time, rc)
      class(ExtDataNode), intent(inout) :: this
      character(len=*), intent(in) :: filename
      type(ESMF_Time), intent(in) :: target_time
      integer, optional, intent(out) :: rc

      integer :: status, i
      type(FileMetaDataUtils) :: metadata
      type(FileMetadata) :: basic_metadata
      type(NetCDF4_FileFormatter) :: formatter
      type(ESMF_Time), allocatable :: time_vector(:)

      _ASSERT(this%node_side/=unknown_node, "node does not have a side")
      call formatter%open(filename, pFIO_READ, _RC)
      basic_metadata = formatter%read(_RC)
      call formatter%close()
      metadata = FileMetadataUtils(basic_metadata, filename)

      call metadata%get_time_info(timeVector=time_vector, _RC)
      select case(this%node_side)
      case (left_node)
         do i=size(time_vector),1,-1
            if (target_time >= time_vector(i)) then
               this%file = filename
               this%interp_time = time_vector(i)
               this%file_time = time_vector(i)
               this%time_index = i
               this%enabled = .true.
               this%update = .true.
               exit
            end if
         enddo
      case (right_node)
         do i=1,size(time_vector)
            if (target_time < time_vector(i)) then
               this%file = filename
               this%interp_time = time_vector(i)
               this%file_time = time_vector(i)
               this%time_index = i
               this%enabled = .true.
               this%update = .true.
               exit
            end if
         enddo
      end select

      _RETURN(_SUCCESS)
   end subroutine

end module mapl3g_ExtDataNode
