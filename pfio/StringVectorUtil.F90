#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module pFIO_StringVectorUtilMod
   use pFIO_UtilitiesMod
   use pFIO_AttributeMod
   use gFTL_StringVector
   use MAPL_ExceptionHandling
   implicit none
   private
   public :: StringVector_serialize
   public :: StringVector_deserialize
   public :: string_in_stringVector

contains

   subroutine StringVector_serialize(strVec,buffer)
       type (StringVector) ,intent(in):: strVec
       integer, allocatable,intent(inout) :: buffer(:)
       type (StringVectorIterator) :: iter
       character(len=:),pointer :: str
       integer :: length
      
       if (allocated(buffer)) deallocate(buffer)
       allocate(buffer(0))
       iter = strVec%begin()
       do while (iter /= strVec%end())
          str => iter%get()
          buffer=[buffer,serialize_intrinsic(str)]
          call iter%next()
       enddo
       length = serialize_buffer_length(length) + size(buffer)
       buffer = [serialize_intrinsic(length),buffer]
    end subroutine StringVector_serialize

    subroutine StringVector_deserialize(buffer, strVec, rc)
       integer, intent(in) :: buffer(:)
       type (StringVector), intent(inout) :: strVec
       integer, optional, intent(out) :: rc

       character(len=:),allocatable :: str
       integer :: length,n,n1,n0

       n = 1
       call deserialize_intrinsic(buffer(n:),length)
       n0 =  serialize_buffer_length(length)
       n = n + n0
       length = length - n0
       strVec = StringVector() 
       do while (length > 0)
          call deserialize_intrinsic(buffer(n:),str)
          call strVec%push_back(str)
          n1 = serialize_buffer_length(str)
          n = n + n1
          length = length - n1
          deallocate(str)
       enddo
       _RETURN(_SUCCESS)
   end subroutine StringVector_deserialize

   function string_in_stringVector(target_string,string_vector) result(in_vector)
      logical :: in_vector
      character(len=*), intent(in) :: target_string
      type(StringVector), intent(in) :: string_vector

      type(StringVectorIterator) :: iter

      in_vector = .false.
      iter = string_vector%begin()
      do while(iter /= string_vector%end())
         if (trim(target_string) == iter%get()) in_vector = .true.
         call iter%next()
      enddo
   end function string_in_stringVector

end module pFIO_StringVectorUtilMod
