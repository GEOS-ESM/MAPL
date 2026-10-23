#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module pFIO_NextCollectivePrefetchDoneMessageMod
   use pFIO_CollectivePrefetchDoneMessageMod
   implicit none
   private

   public :: NextCollectivePrefetchDoneMessage

   type, extends(CollectivePrefetchDoneMessage) :: NextCollectivePrefetchDoneMessage
   contains
      procedure, nopass :: get_type_id
   end type NextCollectivePrefetchDoneMessage

   interface NextCollectivePrefetchDoneMessage
      module procedure new_NextCollectivePrefetchDoneMessage
   end interface

contains

   function new_NextCollectivePrefetchDoneMessage() result(message)
      type(NextCollectivePrefetchDoneMessage) :: message
      return
      _UNUSED_DUMMY(message)
   end function new_NextCollectivePrefetchDoneMessage

   integer function get_type_id() result(type_id)
      use pFIO_AbstractMessageMod, only: NextCollectivePrefetchDone_ID
      type_id = NextCollectivePrefetchDone_ID
   end function get_type_id

end module pFIO_NextCollectivePrefetchDoneMessageMod
