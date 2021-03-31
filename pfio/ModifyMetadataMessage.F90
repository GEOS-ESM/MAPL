#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module pFIO_ModifyMetadataMessageMod
   use MAPL_ExceptionHandling
   use pFIO_UtilitiesMod
   use pFIO_AbstractMessageMod
   use pFIO_StringVariableMapMod
   use pFIO_StringVariableMapUtilMod
   use pFIO_KeywordEnforcerMod
   implicit none
   private

   public :: ModifyMetadataMessage

   type, extends(AbstractMessage) :: ModifyMetadataMessage
      integer :: collection_id
      type (StringVariableMap) :: var_map
   contains
      procedure, nopass :: get_type_id
      procedure :: get_length
      procedure :: serialize
      procedure :: deserialize
   end type ModifyMetadataMessage

   interface ModifyMetadataMessage
      module procedure new_ModifyMetadataMessage
   end interface

contains

   function new_ModifyMetadataMessage(collection_id, unusable, var_map, rc) result (message)
      type (ModifyMetadataMessage) :: message
      integer, intent(in) :: collection_id
      class (KeywordEnforcer), optional,  intent(in) :: unusable
      type (StringVariableMap), optional, intent(in) :: var_map
      integer, optional, intent(out) :: rc

      message%collection_id = collection_id
      if (present(var_map)) message%var_map = var_map
 
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function new_ModifyMetadataMessage

   integer function get_type_id() result(type_id)
      type_id = ModifyMetadata_ID
   end function get_type_id
 
   integer function get_length(this) result(length)
      class (ModifyMetadataMessage), intent(in) :: this

      length = &
           & serialize_buffer_length(this%collection_id) + &
           & StringVariableMap_get_length(this%var_map)

   end function get_length

   subroutine serialize(this, buffer, rc)
      class (ModifyMetadataMessage), intent(in) :: this
      integer, intent(inout) :: buffer(:)
      integer, optional, intent(out) :: rc
  
      integer, allocatable ::  map_buf(:)
      integer :: status

      call StringVariableMap_serialize(this%var_map, map_buf, rc=status)
      _VERIFY(status)
      buffer = [ &
           & serialize_intrinsic(this%collection_id), &
           & map_buf]
      _RETURN(_SUCCESS)
   end subroutine serialize

   subroutine deserialize(this, buffer, rc)
      class (ModifyMetadataMessage), intent(inout) :: this
      integer, intent(in) :: buffer(:)
      integer, optional, intent(out) :: rc

      integer :: n, status

      n = 1
      call deserialize_intrinsic(buffer(n:), this%collection_id)
      n = n + serialize_buffer_length(this%collection_id)

      call StringVariableMap_deserialize(buffer(n:), this%var_map, rc=status)
      _VERIFY(status)
      _RETURN(_SUCCESS)
   end subroutine deserialize

end module pFIO_ModifyMetadataMessageMod
