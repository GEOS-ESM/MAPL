#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module pFIO_MockClientThreadMod
   use MAPL_ExceptionHandling
   use pFIO_AbstractMessageMod
   use pFIO_AbstractSocketMod
   use pFIO_AbstractRequestHandleMod
   use pFIO_IntegerRequestMapMod
   use pFIO_MessageVisitorMod
   use pFIO_BaseThreadMod
   use pFIO_AbstractDataReferenceMod
   use pFIO_KeywordEnforcerMod
   use pFIO_SimpleSocketMod
   use pFIO_FileMetadataMod

   use pFIO_TerminateMessageMod
   use pFIO_DoneMessageMod
   use pFIO_AddExtCollectionMessageMod
   use pFIO_AddHistCollectionMessageMod
   use pFIO_IdMessageMod
   use pFIO_PrefetchDataMessageMod
   use pFIO_StageDataMessageMod
   use pFIO_CollectivePrefetchDataMessageMod
   use pFIO_CollectiveStageDataMessageMod
   use pFIO_ModifyMetadataMessageMod
   use pFIO_StringVariableMapMod
   use pFIO_ClientThreadMod

   use, intrinsic :: iso_fortran_env, only: REAL32
   implicit none
   private

   public :: MockClientThread

   
   type, extends(ClientThread) :: MockClientThread
   contains
      procedure :: wait
   end type MockClientThread


   interface MockClientThread
      module procedure new_MockClientThread
   end interface MockClientThread

contains

   function new_MockClientThread(sckt) result(c)
      type (MockClientThread),target :: c
      class(AbstractSocket),optional,intent(in) :: sckt
      if(present(sckt)) call c%set_connection(sckt)
   end function new_MockClientThread

   subroutine wait(this, request_id)
      use pFIO_AbstractRequestHandleMod
      class (MockClientThread), target, intent(inout) :: this
      integer, intent(in) :: request_id
      class(AbstractRequestHandle), pointer :: handle

      handle => this%get_RequestHandle(request_id)
      call handle%wait()
      call this%erase_RequestHandle(request_id)

   end subroutine wait

end module pFIO_MockClientThreadMod
