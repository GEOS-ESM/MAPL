module pFIO_CollectivePrefetchDataMessageMod
   use pFIO_AbstractMessageMod
   use pFIO_AbstractCollectiveDataMessageMod
   use pFIO_UtilitiesMod
   use pFIO_AbstractDataReferenceMod
   use mapl_KeywordEnforcer_mod
   implicit none
   private

   public :: CollectivePrefetchDataMessage

   type, extends(AbstractCollectiveDataMessage) :: CollectivePrefetchDataMessage
      logical :: cache_only = .false.
   contains
      procedure, nopass :: get_type_id
      procedure :: get_length
      procedure :: serialize
      procedure :: deserialize
   end type CollectivePrefetchDataMessage

   interface CollectivePrefetchDataMessage
      module procedure new_CollectivePrefetchDataMessage
   end interface CollectivePrefetchDataMessage

contains


    function new_CollectivePrefetchDataMessage( &
         & request_id, collection_id, file_name, var_name, &
         & data_reference, unusable, start,global_start,global_count, cache_only) result(message)
      type (CollectivePrefetchDataMessage) :: message
      integer, intent(in) :: request_id
      integer, intent(in) :: collection_id
      character(len=*), intent(in) :: file_name
      character(len=*), intent(in) :: var_name
      class (AbstractDataReference), intent(in) :: data_reference
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: start(:)
      integer, optional, intent(in) :: global_start(:)
      integer, optional, intent(in) :: global_count(:)
      logical, optional, intent(in) :: cache_only

      call message%initCollective(request_id,collection_id, &
           file_name,var_name,data_reference,unusable=unusable, &
           start=start,global_start = global_start,global_count=global_count)
      if (present(cache_only)) message%cache_only = cache_only

    end function new_CollectivePrefetchDataMessage

    integer function get_type_id() result(type_id)
       type_id = CollectivePrefetchData_ID
    end function get_type_id

    integer function get_length(this) result(length)
      class (CollectivePrefetchDataMessage), intent(in) :: this

      length = &
           & serialize_buffer_length(this%request_id) + &
           & serialize_buffer_length(this%collection_id) + &
           & serialize_buffer_length(this%file_name) + &
           & serialize_buffer_length(this%var_name) + &
           & serialize_buffer_length(this%type_kind) + &
           & serialize_buffer_length(this%start) + &
           & serialize_buffer_length(this%count) + &
           & serialize_buffer_length(this%cache_only) + &
           & serialize_buffer_length(this%global_start) + &
           & serialize_buffer_length(this%global_count)
    end function get_length

    subroutine serialize(this, buffer, rc)
      class (CollectivePrefetchDataMessage), intent(in) :: this
      integer, intent(inout) :: buffer(:)
      integer, optional, intent(out) :: rc

      buffer = [ &
           & serialize_intrinsic(this%request_id), &
           & serialize_intrinsic(this%collection_id), &
           & serialize_intrinsic(this%file_name), &
           & serialize_intrinsic(this%var_name), &
           & serialize_intrinsic(this%type_kind), &
           & serialize_intrinsic(this%start), &
           & serialize_intrinsic(this%count), &
           & serialize_intrinsic(this%cache_only), &
           & serialize_intrinsic(this%global_start), &
           & serialize_intrinsic(this%global_count)]
      if (present(rc)) rc = 0
    end subroutine serialize

    subroutine deserialize(this, buffer, rc)
      class (CollectivePrefetchDataMessage), intent(inout) :: this
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
      call deserialize_intrinsic(buffer(n:), this%var_name)
      n = n + serialize_buffer_length(this%var_name)
      call deserialize_intrinsic(buffer(n:), this%type_kind)
      n = n + serialize_buffer_length(this%type_kind)
      call deserialize_intrinsic(buffer(n:), this%start)
      n = n + serialize_buffer_length(this%start)
      call deserialize_intrinsic(buffer(n:), this%count)
      n = n + serialize_buffer_length(this%count)
      call deserialize_intrinsic(buffer(n:), this%cache_only)
      n = n + serialize_buffer_length(this%cache_only)
      call deserialize_intrinsic(buffer(n:), this%global_start)
      n = n + serialize_buffer_length(this%global_start)
      call deserialize_intrinsic(buffer(n:), this%global_count)
      if (present(rc)) rc = 0
    end subroutine deserialize

end module pFIO_CollectivePrefetchDataMessageMod
