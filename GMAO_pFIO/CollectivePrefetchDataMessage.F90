module pFIO_CollectivePrefetchDataMessageMod
   use pFIO_AbstractMessageMod
   use pFIO_AbstractCollectiveDataMessageMod
   use pFIO_UtilitiesMod
   use pFIO_ArrayReferenceMod
   use pFIO_KeywordEnforcerMod
   implicit none
   private

   public :: CollectivePrefetchDataMessage

   type, extends(AbstractCollectiveDataMessage) :: CollectivePrefetchDataMessage
   contains
      procedure, nopass :: get_type_id
   end type CollectivePrefetchDataMessage

   interface CollectivePrefetchDataMessage
      module procedure new_CollectivePrefetchDataMessage
   end interface CollectivePrefetchDataMessage

contains


   function new_CollectivePrefetchDataMessage( &
        & request_id, collection_id, file_name, var_name, &
        & data_reference, unusable, start,global_start,global_count) result(message)
      type (CollectivePrefetchDataMessage) :: message
      integer, intent(in) :: request_id
      integer, intent(in) :: collection_id
      character(len=*), intent(in) :: file_name
      character(len=*), intent(in) :: var_name
      type (ArrayReference), intent(in) :: data_reference
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: start(:)
      integer, optional, intent(in) :: global_start(:)
      integer, optional, intent(in) :: global_count(:)

      call message%initCollective(request_id,collection_id, &
          file_name,var_name,data_reference,unusable=unusable, &
          start=start,global_start = global_start,global_count=global_count)

   end function new_CollectivePrefetchDataMessage

   integer function get_type_id() result(type_id)
      type_id = CollectivePrefetchData_ID
   end function get_type_id

end module pFIO_CollectivePrefetchDataMessageMod

