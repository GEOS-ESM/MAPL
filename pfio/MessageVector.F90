#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module pFIO_MessageVectorMod
   use pFIO_AbstractMessageMod

#define _type class(AbstractMessage)
#define _allocatable
#define _vector MessageVector
#define _iterator MessageVectorIterator
#define _niterator MessageVectorRIterator
#include "templates/vector.inc"

end module pFIO_MessageVectorMod

module pFIO_MessageVectorUtilMod
   use MAPL_ExceptionHandling
   use pFIO_AbstractMessageMod
   use pFIO_MessageVectorMod
   use pFIO_ProtocolParserMod
   use pFIO_CollectiveStageDataMessageMod
   use, intrinsic :: iso_fortran_env, only: INT64
   implicit none
   private 

   public:: serialize_message_vector
   public:: deserialize_message_vector

contains

  subroutine serialize_message_vector(msgVec,buffer, rc)
     type (MessageVector),intent(in) :: msgVec
     integer, allocatable,intent(inout) :: buffer(:)
     integer, optional, intent(out) :: rc
     integer, allocatable :: tmp(:)
     class (AbstractMessage),pointer :: msg
     integer :: n, i
     type (ProtocolParser) :: parser

     n = msgVec%size()
     parser = ProtocolParser()
     allocate(tmp(0))
     do i = 1, n
        msg=>msgVec%at(i)
        tmp =[tmp, parser%encode(msg)]
     enddo

     if(size(tmp, kind=INT64) > huge(0)) then
       __FAIL("need to increase oserver's nfront")
     endif

     i = size(tmp)+1

     if (allocated(buffer)) deallocate(buffer)
     buffer =[i,tmp]

  end subroutine

  subroutine deserialize_message_vector(buffer, msgVec, rc)
     type (MessageVector),intent(inout) :: msgVec
     integer, intent(in) :: buffer(:)
     integer, optional, intent(out) :: rc

     class (AbstractMessage),allocatable:: msg

     integer :: n, length
     type (ProtocolParser) :: parser
    ! integer, allocatable :: buffer_test(:)

     parser = ProtocolParser()
     length = buffer(1)
     n=2
     msgVec = MessageVector()
     do while (n < length)
       allocate(msg, source = parser%decode(buffer(n:))) 
       call msgVec%push_back(msg)
       n = n + msg%get_length()+1 
       deallocate(msg)
     enddo
     __ASSERT(n-1 == length, "wrong length of message vector")

    ! lazy UNIT test! W.J notes: ifor passes, gfortran fails
    ! call serialize_message_vector(msgVec,buffer_test)
    ! __ASSERT(all(buffer(1:length) == buffer_test), "serialize-deserialize error")
    
     __RETURN(__SUCCESS)
  end subroutine

end module pFIO_MessageVectorUtilMod
