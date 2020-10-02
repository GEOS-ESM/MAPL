#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module pFIO_ForwardDataMessageMod
   use mpi
   use MAPL_ExceptionHandling
   use pFIO_AbstractMessageMod
   use pFIO_UtilitiesMod
   use pFIO_AbstractDataReferenceMod
   use pFIO_KeywordEnforcerMod
   use pFIO_AbstractDataMessageMod
   implicit none
   private

   public :: ForwardDataMessage

   type, extends(AbstractMessage) :: ForwardDataMessage
      integer :: request_id
      integer :: collection_id
      character(len=:), allocatable :: file_name
      character(len=:), allocatable :: var_name
      integer :: type_kind
      integer, allocatable :: count(:)
      integer(kind=MPI_ADDRESS_KIND) :: offset 
   contains
      procedure, nopass :: get_type_id
      procedure :: get_length
      procedure :: serialize
      procedure :: deserialize
   end type ForwardDataMessage

   interface ForwardDataMessage
      module procedure new_ForwardDataMessage
   end interface ForwardDataMessage

contains

   integer function get_type_id() result(type_id)
      type_id = ForwardData_ID
   end function get_type_id

   function new_ForwardDataMessage( &
        & request_id, collection_id, file_name, var_name, &
        & type_kind, count, offset) result(message)
      type (ForwardDataMessage) :: message
      integer, intent(in) :: request_id
      integer, intent(in) :: collection_id
      character(len=*), intent(in) :: file_name
      character(len=*), intent(in) :: var_name
      integer :: type_kind
      integer, intent(in) :: count(:)
      integer(kind=MPI_ADDRESS_KIND), intent(in) :: offset 

      message%request_id    = request_id
      message%collection_id = collection_id
      message%file_name = file_name
      message%var_name  = var_name
      message%type_kind = type_kind

      message%count = count
      message%offset = offset

   end function new_ForwardDataMessage

   integer function get_length(this) result(length)
      class (ForwardDataMessage), intent(in) :: this

      length = &
           & serialize_buffer_length(this%request_id) + &
           & serialize_buffer_length(this%collection_id) + &
           & serialize_buffer_length(this%file_name) + &
           & serialize_buffer_length(this%var_name) + &
           & serialize_buffer_length(this%type_kind) + &
           & serialize_buffer_length(this%count) + &
           & serialize_buffer_length(this%offset)
   end function get_length

   subroutine serialize(this, buffer, rc)
      class (ForwardDataMessage), intent(in) :: this
      integer, intent(inout) :: buffer(:)
      integer, optional, intent(out) :: rc

      buffer = [ &
           & serialize_intrinsic(this%request_id), &
           & serialize_intrinsic(this%collection_id), &
           & serialize_intrinsic(this%file_name), &
           & serialize_intrinsic(this%var_name), &
           & serialize_intrinsic(this%type_kind), &
           & serialize_intrinsic(this%count), &
           & serialize_intrinsic(this%offset)]
      _RETURN(_SUCCESS)
   end subroutine serialize

   subroutine deserialize(this, buffer, rc)
      class (ForwardDataMessage), intent(inout) :: this
      integer, intent(in) :: buffer(:)
      integer, optional, intent(out) :: rc

      integer :: n

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
      call deserialize_intrinsic(buffer(n:), this%count)
      n = n + serialize_buffer_length(this%count)
      call deserialize_intrinsic(buffer(n:), this%offset)
      n = n + serialize_buffer_length(this%offset)
      _RETURN(_SUCCESS)
   end subroutine deserialize

end module pFIO_ForwardDataMessageMod

