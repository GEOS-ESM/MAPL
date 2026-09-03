module pFIO_NextCollectivePrefetchMessageMod
   use pFIO_AbstractMessageMod
   use pFIO_CollectivePrefetchDataMessageMod
   use pFIO_AbstractDataReferenceMod
   use mapl_KeywordEnforcer_mod
   implicit none
   private

   public :: NextCollectivePrefetchMessage

   type, extends(CollectivePrefetchDataMessage) :: NextCollectivePrefetchMessage
   contains
      procedure, nopass :: get_type_id
   end type NextCollectivePrefetchMessage

   interface NextCollectivePrefetchMessage
      module procedure new_NextCollectivePrefetchMessage
   end interface NextCollectivePrefetchMessage

contains

   function new_NextCollectivePrefetchMessage( &
        & request_id, collection_id, file_name, var_name, &
        & data_reference, unusable, start, global_start, global_count) result(message)
      type(NextCollectivePrefetchMessage) :: message
      integer, intent(in) :: request_id
      integer, intent(in) :: collection_id
      character(len=*), intent(in) :: file_name
      character(len=*), intent(in) :: var_name
      class(AbstractDataReference), intent(in) :: data_reference
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: start(:)
      integer, optional, intent(in) :: global_start(:)
      integer, optional, intent(in) :: global_count(:)

      call message%initCollective(request_id, collection_id, file_name, var_name, data_reference, &
           unusable=unusable, start=start, global_start=global_start, global_count=global_count)
      message%cache_only = .true.
   end function new_NextCollectivePrefetchMessage

   integer function get_type_id() result(type_id)
      type_id = COLLECTIVENextPrefetchData_ID
   end function get_type_id

end module pFIO_NextCollectivePrefetchMessageMod
