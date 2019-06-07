module pFIO_RequestIdMessageMod
   use pFIO_AbstractMessageMod
   use, intrinsic :: iso_fortran_env, only: INT32
   implicit none
   private

   public :: RequestIdMessage

   type, extends(AbstractMessage) :: RequestIdMessage
      integer :: request_id
   contains
      procedure, nopass :: get_type_id
      procedure :: get_length
      procedure :: serialize
      procedure :: deserialize
   end type RequestIdMessage

   interface RequestIdMessage
      module procedure new_RequestIdMessage
   end interface RequestIdMessage

contains

   function new_RequestIdMessage(request_id) result(message)
      type (RequestIdMessage) :: message
      integer, intent(in) :: request_id

      message%request_id = request_id

   end function new_RequestIdMessage
      

   integer function get_type_id() result(type_id)
      type_id = REQUESTID_ID
   end function get_type_id

   integer function get_length(this) result(length)
      class (RequestIdMessage), intent(in) :: this
      length = 1
   end function get_length

   subroutine serialize(this, buffer)
      class (RequestIdMessage), intent(in) :: this
      integer(kind=INT32), intent(inout) :: buffer(:) ! no-op
      buffer = [this%request_id]
   end subroutine serialize

   subroutine deserialize(this, buffer)
      class (RequestIdMessage), intent(inout) :: this
      integer(kind=INT32), intent(in) :: buffer(:)

      this%request_id = buffer(1)

   end subroutine deserialize
   
end module pFIO_RequestIdMessageMod

