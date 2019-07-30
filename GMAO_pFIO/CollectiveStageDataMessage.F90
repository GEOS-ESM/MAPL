module pFIO_CollectiveStageDataMessageMod
   use pFIO_AbstractMessageMod
   use pFIO_AbstractCollectiveDataMessageMod
   use pFIO_UtilitiesMod
   use pFIO_ArrayReferenceMod
   use pFIO_KeywordEnforcerMod
   implicit none
   private

   public :: CollectiveStageDataMessage

   type, extends(AbstractCollectiveDataMessage) :: CollectiveStageDataMessage
   contains
      procedure, nopass :: get_type_id
   end type CollectiveStageDataMessage

   interface CollectiveStageDataMessage
      module procedure new_CollectiveStageDataMessage
   end interface CollectiveStageDataMessage

contains


   function new_CollectiveStageDataMessage( &
        & request_id, collection_id, file_name, var_name, &
        & data_reference, unusable, start,global_start,global_count) result(message)
      type (CollectiveStageDataMessage) :: message
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

   end function new_CollectiveStageDataMessage

   integer function get_type_id() result(type_id)
      type_id = CollectiveStageData_ID
   end function get_type_id

end module pFIO_CollectiveStageDataMessageMod

