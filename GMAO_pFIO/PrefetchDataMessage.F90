module pFIO_PrefetchDataMessageMod
   use pFIO_AbstractMessageMod
   use pFIO_UtilitiesMod
   use pFIO_ArrayReferenceMod
   use pFIO_KeywordEnforcerMod
   use pFIO_AbstractDataMessageMod
   implicit none
   private

   public :: PrefetchDataMessage

   type, extends(AbstractDataMessage) :: PrefetchDataMessage
   contains
     procedure, nopass :: get_type_id
   end type PrefetchDataMessage

   interface PrefetchDataMessage
      module procedure new_PrefetchDataMessage
   end interface PrefetchDataMessage

contains

   integer function get_type_id() result(type_id)
      type_id = PrefetchData_ID
   end function get_type_id

   function new_PrefetchDataMessage( &
        & request_id, collection_id, file_name, var_name, &
        & data_reference, unusable, start) result(message)
      type (PrefetchDataMessage) :: message
      integer, intent(in) :: request_id
      integer, intent(in) :: collection_id
      character(len=*), intent(in) :: file_name
      character(len=*), intent(in) :: var_name
      type (ArrayReference), intent(in) :: data_reference
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: start(:)

      call message%init( &
        & request_id, collection_id, file_name, var_name, &
        & data_reference, unusable=unusable, start=start)

   end function new_PrefetchDataMessage

end module pFIO_PrefetchDataMessageMod

