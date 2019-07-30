module pFIO_StageDataMessageMod
   use pFIO_AbstractMessageMod
   use pFIO_UtilitiesMod
   use pFIO_ArrayReferenceMod
   use pFIO_KeywordEnforcerMod
   use pFIO_AbstractDataMessageMod
   implicit none
   private

   public :: StageDataMessage

   type, extends(AbstractDataMessage) :: StageDataMessage
   contains
      procedure, nopass :: get_type_id
   end type StageDataMessage

   interface StageDataMessage
      module procedure new_StageDataMessage
   end interface StageDataMessage

contains

   integer function get_type_id() result(type_id)
      type_id = StageData_ID
   end function get_type_id

   function new_StageDataMessage( &
        & request_id, file_md_id, file_name, var_name, &
        & data_reference, unusable, start) result(message)
      type (StageDataMessage) :: message
      integer, intent(in) :: request_id
      integer, intent(in) :: file_md_id
      character(len=*), intent(in) :: file_name
      character(len=*), intent(in) :: var_name
      type (ArrayReference), intent(in) :: data_reference
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: start(:)

      call message%init( &
        & request_id, file_md_id, file_name, var_name, &
        & data_reference, unusable=unusable, start=start)

   end function new_StageDataMessage

end module pFIO_StageDataMessageMod

