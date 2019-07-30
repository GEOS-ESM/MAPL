#include "pFIO_ErrLog.h"
#include "unused_dummy.H"

module pFIO_AbstractCollectiveDataMessageMod
   use pFIO_ErrorHandlingMod
   use pFIO_AbstractMessageMod
   use pFIO_AbstractDataMessageMod
   use pFIO_UtilitiesMod
   use pFIO_ArrayReferenceMod
   use pFIO_KeywordEnforcerMod
   implicit none
   private

   public :: AbstractCollectiveDataMessage

   type, extends(AbstractDataMessage),abstract :: AbstractCollectiveDataMessage
      integer, allocatable :: global_start(:)
      integer, allocatable :: global_count(:)
   contains
      procedure :: initCollective
      procedure :: get_length
      procedure :: serialize
      procedure :: deserialize
   end type AbstractCollectiveDataMessage

contains

   subroutine initCollective( message, &
        & request_id, collection_id, file_name, var_name, &
        & data_reference, unusable, start,global_start,global_count, rc)
      class (AbstractCollectiveDataMessage) :: message
      integer, intent(in) :: request_id
      integer, intent(in) :: collection_id
      character(len=*), intent(in) :: file_name
      character(len=*), intent(in) :: var_name
      type (ArrayReference), intent(in) :: data_reference
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: start(:)
      integer, optional, intent(in) :: global_start(:)
      integer, optional, intent(in) :: global_count(:)
      integer, optional, intent(out) :: rc

      integer, allocatable :: st(:)

      integer :: i,k, status


      if (present(global_start)) then
         message%global_start = global_start
      else
         message%global_start = [(1,i=1,size(data_reference%shape))]
      end if

      if (present(global_count)) then
         message%global_count = global_count
      else
         message%global_count = data_reference%shape
      end if

      _ASSERT(size(message%global_start) == size(message%global_count),"global count and global start should mactch")

      if(present(start)) then
         k = size(message%global_start) - size(start)
         st = [start,[(1,i=1,k)]]
      else
         st = message%global_start
      endif 
              
      call message%init(request_id,collection_id, &
          file_name,var_name,data_reference,unusable=unusable,start=st, rc=status)
      _VERIFY(status)
      _RETURN(_SUCCESS)
   end subroutine initCollective
 
   integer function get_length(this) result(length)
      class (AbstractCollectiveDataMessage), intent(in) :: this

      length = &
           & serialize_buffer_length(this%request_id) + &
           & serialize_buffer_length(this%collection_id) + &
           & serialize_buffer_length(this%file_name) + &
           & serialize_buffer_length(this%var_name) + &
           & serialize_buffer_length(this%type_kind) + &
           & serialize_buffer_length(this%start) + &
           & serialize_buffer_length(this%count) + &
           & serialize_buffer_length(this%global_start) + &
           & serialize_buffer_length(this%global_count) + &
           & this%data_reference%get_length()

   end function get_length

   subroutine serialize(this, buffer, rc)
      class (AbstractCollectiveDataMessage), intent(in) :: this
      integer, intent(inout) :: buffer(:)
      integer, optional, intent(out) :: rc

      integer, allocatable :: data_buf(:)
      integer :: status

      call this%data_reference%serialize(data_buf, status)
      _VERIFY(status)

      buffer = [ &
           & serialize_intrinsic(this%request_id), &
           & serialize_intrinsic(this%collection_id), &
           & serialize_intrinsic(this%file_name), &
           & serialize_intrinsic(this%var_name), &
           & serialize_intrinsic(this%type_kind), &
           & serialize_intrinsic(this%start), &
           & serialize_intrinsic(this%count), &
           & serialize_intrinsic(this%global_start), &
           & serialize_intrinsic(this%global_count), &
           & data_buf ]
       _RETURN(_SUCCESS)
   end subroutine serialize

   subroutine deserialize(this, buffer, rc)
      class (AbstractCollectiveDataMessage), intent(inout) :: this
      integer, intent(in) :: buffer(:)
      integer, optional, intent(out) :: rc

      integer :: n, status

      n = 1
      call deserialize_intrinsic(buffer(n:), this%request_id)
      n = n + serialize_buffer_length(this%request_id)
      call deserialize_intrinsic(buffer(n:), this%collection_id)
      n = n + serialize_buffer_length(this%collection_id)
      call deserialize_intrinsic(buffer(n:), this%file_name)
      n = n + serialize_buffer_length(this%file_name)
      call deserialize_intrinsic(buffer(n:),this%var_name)
      n = n + serialize_buffer_length(this%var_name)
      call deserialize_intrinsic(buffer(n:), this%type_kind)
      n = n + serialize_buffer_length(this%type_kind)
      call deserialize_intrinsic(buffer(n:), this%start)
      n = n + serialize_buffer_length(this%start)
      call deserialize_intrinsic(buffer(n:), this%count)
      n = n + serialize_buffer_length(this%count)
      call deserialize_intrinsic(buffer(n:), this%global_start)
      n = n + serialize_buffer_length(this%global_start)
      call deserialize_intrinsic(buffer(n:), this%global_count)
      n = n + serialize_buffer_length(this%global_count)
      call this%data_reference%deserialize(buffer(n:), rc=status)
      _VERIFY(status)
      _RETURN(_SUCCESS)
   end subroutine deserialize
   
end module pFIO_AbstractCollectiveDataMessageMod
