#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module pFIO_ForwardDataAndMessageMod
   use mpi
   use MAPL_ExceptionHandling
   use pFIO_AbstractMessageMod
   use pFIO_UtilitiesMod
   use pFIO_AbstractDataReferenceMod
   use mapl_KeywordEnforcerMod
   use pFIO_AbstractDataMessageMod
   use pFIO_FileMetaDataMod
   use pFIO_MessageVectorMod
   use pFIO_MessageVectorUtilMod
   use, intrinsic :: iso_fortran_env, only: INT64

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
      procedure :: destroy
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
      integer :: i,k
      integer, allocatable :: buff_tmp(:)


      if (allocated(buffer)) deallocate(buffer)
      k = 0
      if (allocated(this%idata)) k = size(this%idata)
      call serialize_message_vector(this%msg_vec, buff_tmp)

      if ( k > 0 ) then
         i = k + 1
         buffer =[buff_tmp, i, this%idata]
      else
         buffer = buff_tmp
      endif
      if ( size(buffer, kind=INT64) > huge(0)) then
        _FAIL("need to increase oserver's number of front cores (nfront)")
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

   subroutine destroy(this, rc)
      class (ForwardDataAndMessage), intent(inout) :: this
      integer, optional, intent(out) :: rc
      type (MessageVectorIterator) :: iter

      if (allocated(this%idata)) deallocate(this%idata)
      iter = this%msg_vec%begin()
      do while (iter /= this%msg_vec%end())
        call this%msg_vec%erase(iter)
        iter = this%msg_vec%begin()
     enddo
      _RETURN(_SUCCESS)
   end subroutine

end module pFIO_ForwardDataAndMessageMod

