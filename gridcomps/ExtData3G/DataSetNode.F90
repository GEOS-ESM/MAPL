#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
module mapl3g_DataSetNode
   use ESMF
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   use pFIO
   use MAPL_FileMetadataUtilsMod
   implicit none
   private

   public :: DataSetNode
   public :: NODE_LEFT
   public :: NODE_RIGHT
   public :: NODE_UNKNOWN
 
   enum, bind(c)
      enumerator :: NODE_LEFT
      enumerator :: NODE_RIGHT
      enumerator :: NODE_UNKNOWN
   end enum      

   type :: DataSetNode
      integer :: node_side
      logical :: update = .false.
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
         procedure :: write_node
         procedure :: file_allocated
         generic :: operator(==) => equals
   end type

   interface DataSetNode
      procedure new_DataSetNode
   end interface

contains

   function new_DataSetNode(file, time_index, file_time, interp_time) result(node)
      type(DataSetNode) :: node
      character(len=*), intent(in) :: file
      integer, intent(in) :: time_index
      type(ESMF_Time), intent(in) :: file_time
      type(ESMF_Time), intent(in) :: interp_time

      node%file_time = file_time
      node%interp_time = interp_time
      node%file = trim(file)
      node%time_index = time_index
      
   end function new_DataSetNode

   subroutine set_file_time(this, file_time)
      class(DataSetNode), intent(inout) :: this
      type(ESMF_Time), intent(in) :: file_time
      this%file_time=file_time
   end subroutine

   subroutine set_interp_time(this, interp_time)
      class(DataSetNode), intent(inout) :: this
      type(ESMF_Time), intent(in) :: interp_time
      this%interp_time=interp_time
   end subroutine

   subroutine set_file(this, file)
      class(DataSetNode), intent(inout) :: this
      character(len=*), intent(in) :: file
      this%file=file
   end subroutine

   subroutine set_time_index(this, time_index)
      class(DataSetNode), intent(inout) :: this
      integer, intent(in) :: time_index
      this%time_index=time_index
   end subroutine

   subroutine set_node_side(this, node_side)
      class(DataSetNode), intent(inout) :: this
      integer, intent(in) :: node_side
      this%node_side = node_side
   end subroutine

   subroutine set_enabled(this, enabled)
      class(DataSetNode), intent(inout) :: this
      logical, intent(in) :: enabled
      this%enabled = enabled 
   end subroutine

   subroutine set_update(this, update)
      class(DataSetNode), intent(inout) :: this
      logical, intent(in) :: update
      this%update = update 
   end subroutine

   function get_file_time(this) result(file_time)
      type(ESMF_Time) :: file_time
      class(DataSetNode), intent(inout) :: this
      file_time=this%file_time
   end function

   function get_interp_time(this) result(interp_time)
      type(ESMF_Time) :: interp_time
      class(DataSetNode), intent(inout) :: this
      interp_time=this%interp_time
   end function

   function get_file(this) result(file)
      character(len=:), allocatable :: file
      class(DataSetNode), intent(inout) :: this
      if (allocated(this%file)) file=this%file
      !if (allocfile=this%file
   end function

   function get_time_index(this) result(time_index)
      integer :: time_index
      class(DataSetNode), intent(inout) :: this
      time_index=this%time_index
   end function

   function get_node_side(this) result(node_side)
      integer :: node_side
      class(DataSetNode), intent(inout) :: this
      node_side=this%node_side
   end function

   function get_update(this) result(update)
      logical :: update
      class(DataSetNode), intent(inout) :: this
      update=this%update
   end function

   function get_enabled(this) result(enabled)
      logical :: enabled
      class(DataSetNode), intent(inout) :: this
      enabled=this%enabled
   end function

   logical function equals(a,b)
      class(DataSetNode), intent(in) :: a
      class(DataSetNode), intent(in) :: b

      equals = (trim(a%file)==trim(b%file)) .and. (a%file_time==b%file_time) .and. (a%time_index==b%time_index) .and. (a%interp_time==b%interp_time)
   end function equals

   subroutine reset(this)
      class(DataSetNode), intent(inout) :: this
      deallocate(this%file)
      this%enabled = .false.
      this%update = .false.
   end subroutine

   function validate(this, current_time, rc) result(node_is_valid)
      logical :: node_is_valid
      class(DataSetNode), intent(inout) :: this
      type(ESMF_Time), intent(in) :: current_time
      integer, intent(out), optional :: rc

      integer :: status
      if (.not.allocated(this%file)) then
         node_is_valid = .false.
         _RETURN(_SUCCESS)
      end if
      if (this%node_side == NODE_UNKNOWN ) then
         node_is_valid = .false.
         _RETURN(_SUCCESS)
      end if
      if (this%node_side == NODE_LEFT) then
         node_is_valid = (current_time >= this%file_time)
      else if (this%node_side == NODE_RIGHT) then
         node_is_valid = (current_time < this%file_time)
      end if
      _RETURN(_SUCCESS)
   end function

   subroutine invalidate(this)
      class(DataSetNode), intent(inout) :: this
      if (allocated(this%file)) then
         deallocate(this%file) 
      end if
      this%enabled = .false.
      this%update = .false.
   end subroutine

   subroutine update_node_from_file(this, filename, target_time, rc)
      class(DataSetNode), intent(inout) :: this
      character(len=*), intent(in) :: filename
      type(ESMF_Time), intent(in) :: target_time
      integer, optional, intent(out) :: rc

      integer :: status, i
      type(FileMetaDataUtils) :: metadata
      type(FileMetadata) :: basic_metadata
      type(NetCDF4_FileFormatter) :: formatter
      type(ESMF_Time), allocatable :: time_vector(:)

      _ASSERT(this%node_side/=NODE_UNKNOWN, "node does not have a side")
      call formatter%open(filename, pFIO_READ, _RC)
      basic_metadata = formatter%read(_RC)
      call formatter%close()
      metadata = FileMetadataUtils(basic_metadata, filename)

      call metadata%get_time_info(timeVector=time_vector, _RC)
      select case(this%node_side)
      case (NODE_LEFT)
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
      case (NODE_RIGHT)
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

   function file_allocated(this) result(is_allocated)
      logical :: is_allocated
      class(DataSetNode), intent(inout) :: this
      is_allocated = allocated(this%file)
   end function
 
   subroutine write_node(this, pre_string)
      class(DataSetNode), intent(inout) :: this
      character(len=*), optional, intent(in) :: pre_string
      if (present(pre_string)) then
         print*,pre_string//'writing node '
      else
         print*,'writing node '
      end if
      print*,'node_side: ',this%node_side
      print*,'update: ',this%update
      print*,'enabled: ',this%enabled
      if (allocated(this%file)) then
         print*,'file: ',trim(this%file)
      else
         print*,'file not allocated'
      end if
      print*,'time_index ',this%time_index
      call ESMF_TimePrint(this%interp_time, options='string', prestring='interp time: ')
      call ESMF_TimePrint(this%file_time, options='string', prestring='file time: ')
  end subroutine
end module mapl3g_DataSetNode
