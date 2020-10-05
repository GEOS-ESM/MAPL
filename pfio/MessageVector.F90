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
   use pFIO_AbstractMessageMod
   use pFIO_MessageVectorMod
   use pFIO_ProtocolParserMod
   implicit none
   private 

   public:: serialize_message_vector
   public:: deserialize_message_vector

contains

  subroutine serialize_message_vector(msgVec,buffer)
     type (MessageVector),intent(in) :: msgVec
     integer, allocatable,intent(inout) :: buffer(:)
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
     buffer = tmp

  end subroutine

  subroutine deserialize_message_vector(buffer, msgVec)
     type (MessageVector),intent(inout) :: msgVec
     integer, intent(in) :: buffer(:)
     class (AbstractMessage),allocatable:: msg

     integer :: n, length
     type (ProtocolParser) :: parser

     parser = ProtocolParser()
     length = size(buffer)
     n=1
     msgVec = MessageVector()
     do while (n < length)
       allocate(msg, source = parser%decode(buffer(n:))) 
       call msgVec%push_back(msg)
       n= n+ msg%get_length()+1
       deallocate(msg)
     enddo
  end subroutine

end module pFIO_MessageVectorUtilMod
