module pFIO_CollectiveRequestDataMessageMod
   use pFIO_AbstractMessageMod
   use pFIO_RequestDataMessageMod
   use pFIO_UtilitiesMod
   use pFIO_ArrayReferenceMod
   use pFIO_KeywordEnforcerMod
   implicit none
   private

   public :: CollectiveRequestDataMessage

   type, extends(RequestDataMessage) :: CollectiveRequestDataMessage
      integer, allocatable :: global_start(:)
      integer, allocatable :: global_count(:)
   contains
      procedure, nopass :: get_type_id
      procedure :: get_length
      procedure :: serialize
      procedure :: deserialize
   end type CollectiveRequestDataMessage

   interface CollectiveRequestDataMessage
      module procedure new_CollectiveRequestDataMessage
   end interface CollectiveRequestDataMessage

contains


   function new_CollectiveRequestDataMessage( &
        & request_id, collection_id, file_name, var_name, &
        & data_reference, unusable, start,global_start,global_count) result(message)
      type (CollectiveRequestDataMessage) :: message
      integer, intent(in) :: request_id
      integer, intent(in) :: collection_id
      character(len=*), intent(in) :: file_name
      character(len=*), intent(in) :: var_name
      type (ArrayReference), intent(in) :: data_reference
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: start(:)
      integer, optional, intent(in) :: global_start(:)
      integer, optional, intent(in) :: global_count(:)


      integer :: i

      message%RequestDataMessage=RequestDataMessage(request_id,collection_id, &
          file_name,var_name,data_reference,start=start)

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

      if( size(message%global_start) /= size(message%global_count)) print*, "WARNING: global count and global start should mactch"
      if( size(message%global_start) /= size(message%start)) stop "global start and local start shoudl be in the same rank"

   end function new_CollectiveRequestDataMessage
 

   integer function get_type_id() result(type_id)
      type_id = CollectiveRequestData_ID
   end function get_type_id

   integer function get_length(this) result(length)
      class (CollectiveRequestDataMessage), intent(in) :: this

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

   subroutine serialize(this, buffer)
      class (CollectiveRequestDataMessage), intent(in) :: this
      integer, intent(inout) :: buffer(:) ! no-op
      integer, allocatable :: data_buf(:)

      call this%data_reference%serialize(data_buf)

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

   end subroutine serialize

   subroutine deserialize(this, buffer)
      class (CollectiveRequestDataMessage), intent(inout) :: this
      integer, intent(in) :: buffer(:)

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
      call deserialize_intrinsic(buffer(n:), this%start)
      n = n + serialize_buffer_length(this%start)
      call deserialize_intrinsic(buffer(n:), this%count)
      n = n + serialize_buffer_length(this%count)
      call deserialize_intrinsic(buffer(n:), this%global_start)
      n = n + serialize_buffer_length(this%global_start)
      call deserialize_intrinsic(buffer(n:), this%global_count)
      n = n + serialize_buffer_length(this%global_count)
      call this%data_reference%deserialize(buffer(n:))
   end subroutine deserialize
   
end module pFIO_CollectiveRequestDataMessageMod

