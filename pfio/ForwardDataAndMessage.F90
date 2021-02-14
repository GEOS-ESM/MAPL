#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module pFIO_ForwardDataAndMessageMod
   use mpi
   use MAPL_ExceptionHandling
   use pFIO_AbstractMessageMod
   use pFIO_UtilitiesMod
   use pFIO_AbstractDataReferenceMod
   use pFIO_KeywordEnforcerMod
   use pFIO_AbstractDataMessageMod
   use pFIO_FileMetaDataMod
   use pFIO_MessageVectorMod
   use pFIO_MessageVectorUtilMod

   implicit none
   private

   public :: ForwardDataAndMessage

   type :: ForwardDataAndMessage
      type (MessageVector) :: msg_vec
      integer, allocatable :: idata(:)
   contains
      procedure :: add_data_message
      procedure :: serialize
      procedure :: deserialize
   end type ForwardDataAndMessage

   interface ForwardDataAndMessage
      module procedure new_ForwardDataAndMessage
   end interface ForwardDataAndMessage

contains

   function new_ForwardDataAndMessage() result(message)
      type (ForwardDataAndMessage) :: message
      message%msg_vec = MessageVector()
   end function new_ForwardDataAndMessage

   subroutine serialize(this, buffer, rc)
      class (ForwardDataAndMessage), intent(in) :: this
      integer, allocatable, intent(inout) :: buffer(:)
      integer, optional, intent(out) :: rc
      integer :: i
      integer, allocatable :: buff_tmp(:)

      if (allocated(buffer)) deallocate(buffer)

      call serialize_message_vector(this%msg_vec, buff_tmp)
      if ( size(this%idata) > 0 ) then
         i = size(this%idata)+1
         buffer =[buff_tmp, i, this%idata]
      else
         buffer = buff_tmp
      endif

      _RETURN(_SUCCESS)

   end subroutine serialize

   subroutine deserialize(this, buffer, rc)
      class (ForwardDataAndMessage), intent(inout) :: this
      integer, intent(in) :: buffer(:)
      integer, optional, intent(out) :: rc

      integer :: n
      !integer :: k
      
      call deserialize_message_vector(buffer,this%msg_vec)
      n = 1 + buffer(1)
      !k = 0
      if (size(buffer) > n) then
         allocate(this%idata(buffer(n)-1))
         this%idata(:) = buffer(n+1:)
         !k = buffer(n)
      endif
      !_ASSERT(size(buffer) == buffer(1)+ k,"buffer size does not match")
      _RETURN(_SUCCESS)
   end subroutine deserialize

   subroutine add_data_message(this, msg, i_ptr, rc)
      class (ForwardDataAndMessage), intent(inout) :: this
      class (AbstractMessage) :: msg
      integer, intent(in) :: i_ptr(:)
      integer, optional, intent(out) :: rc
     
      call this%msg_vec%push_back(msg)
      if (size(i_ptr) ==0 ) then
         _RETURN(_SUCCESS)
      endif
      if (.not. allocated(this%idata)) then
         this%idata = [i_ptr]
      else
         this%idata = [this%idata, i_ptr]
      endif

      _RETURN(_SUCCESS)
   end subroutine

end module pFIO_ForwardDataAndMessageMod

