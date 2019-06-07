module pFIO_AddCollectionMessageMod
   use pFIO_UtilitiesMod
   use pFIO_AbstractMessageMod
   implicit none
   private

   public :: AddCollectionMessage

   type, extends(AbstractMessage) :: AddCollectionMessage
      character(len=:), allocatable :: template
   contains
      procedure, nopass :: get_type_id
      procedure :: get_length
      procedure :: serialize
      procedure :: deserialize
   end type AddCollectionMessage

   interface AddCollectionMessage
      module procedure new_AddCollectionMessage
   end interface AddCollectionMessage


contains


   function new_AddCollectionMessage(template) result(message)
      type (AddCollectionMessage) :: message
      character(len=*), intent(in) :: template

      message%template = template
   end function new_AddCollectionMessage

   
   integer function get_type_id() result(type_id)
      type_id = ADDCOLLECTION_ID
   end function get_type_id


   integer function get_length(this) result(length)
      class (AddCollectionMessage), intent(in) :: this
      length = serialize_buffer_length(this%template)
   end function get_length


   subroutine serialize(this, buffer)
      class (AddCollectionMessage), intent(in) :: this
      integer, intent(inout) :: buffer(:) ! no-op
      buffer = serialize_intrinsic(this%template)
   end subroutine serialize


   subroutine deserialize(this, buffer)
      class (AddCollectionMessage), intent(inout) :: this
      integer, intent(in) :: buffer(:)

      call deserialize_intrinsic(buffer, this%template)

   end subroutine deserialize

end module pFIO_AddCollectionMessageMod
