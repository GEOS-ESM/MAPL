#include "pFIO_ErrLog.h"
#include "unused_dummy.H"

module pFIO_AbstractDataReferenceMod
   use, intrinsic :: iso_c_binding, only: c_ptr
   use, intrinsic :: iso_c_binding, only: C_NULL_PTR
   use, intrinsic :: iso_c_binding, only: c_loc
   use, intrinsic :: iso_c_binding, only: c_f_pointer
   use, intrinsic :: iso_c_binding, only: c_associated
   use, intrinsic :: iso_fortran_env, only: INT32
   use, intrinsic :: iso_fortran_env, only: INT64
   use, intrinsic :: iso_fortran_env, only: REAL32
   use, intrinsic :: iso_fortran_env, only: REAL64
   use pFIO_UtilitiesMod, only: word_size
   use pFIO_ConstantsMod
   use pFIO_ErrorHandlingMod
   implicit none
   private

   public :: AbstractDataReference

   type,abstract :: AbstractDataReference
      type (c_ptr) :: base_address = C_NULL_PTR
      integer, allocatable :: shape(:)
      integer :: type_kind
   contains
      procedure :: fetch_data
      procedure(get_length),   deferred :: get_length
      procedure(serialize),   deferred :: serialize
      procedure(deserialize), deferred :: deserialize
      procedure :: get_length_base
      procedure :: serialize_base
      procedure :: deserialize_base
      procedure :: equal
      procedure :: allocate
      procedure :: deallocate
      procedure :: convert_addr
      generic :: operator(==) => equal
      procedure :: fence      
      procedure :: copy_data_to
   end type AbstractDataReference

   abstract interface

      integer function get_length(this) result(length)
         import AbstractDataReference
         class (AbstractDataReference), intent(in) :: this
      end function get_length

      subroutine serialize(this, buffer, rc)
         import AbstractDataReference
         class (AbstractDataReference), intent(in) :: this
         integer, allocatable :: buffer(:)
         integer, optional, intent(out) :: rc
      end subroutine serialize

      subroutine deserialize(this, buffer, rc)
         import AbstractDataReference
         class (AbstractDataReference), intent(inout) :: this
         integer, intent(in) :: buffer(:)
         integer, optional, intent(out) :: rc
      end subroutine deserialize

   end interface

contains

   subroutine fetch_data(this,offset_address,global_shape,offset_start, rc)
      class(AbstractDataReference),target,intent(in)    :: this
      type(c_ptr),intent(in)       :: offset_address
      integer,intent(in)           :: global_shape(:)
      integer,optional,intent(in)  :: offset_start(:)
      integer, optional, intent(out) :: rc
      

      integer :: s1,e1, s2,e2,s3,e3,s4,e4,s5,e5

      integer(kind=INT32), pointer :: values_int32_0d
      real(kind=REAL32),   pointer :: values_real32_0d
      integer(kind=INT32), pointer :: all_int32_0d
      real(kind=REAL32),   pointer :: all_real32_0d

      integer(kind=INT32), pointer :: values_int32_1d(:)
      real(kind=REAL32),   pointer :: values_real32_1d(:)
      integer(kind=INT32), pointer :: all_int32_1d(:)
      real(kind=REAL32),   pointer :: all_real32_1d(:)

      integer(kind=INT32), pointer :: values_int32_2d(:,:)
      real(kind=REAL32),   pointer :: values_real32_2d(:,:)
      integer(kind=INT32), pointer :: all_int32_2d(:,:)
      real(kind=REAL32),   pointer :: all_real32_2d(:,:)

      integer(kind=INT32), pointer :: values_int32_3d(:,:,:)
      real(kind=REAL32),   pointer :: values_real32_3d(:,:,:)
      integer(kind=INT32), pointer :: all_int32_3d(:,:,:)
      real(kind=REAL32),   pointer :: all_real32_3d(:,:,:)

      integer(kind=INT32), pointer :: values_int32_4d(:,:,:,:)
      real(kind=REAL32),   pointer :: values_real32_4d(:,:,:,:)
      integer(kind=INT32), pointer :: all_int32_4d(:,:,:,:)
      real(kind=REAL32),   pointer :: all_real32_4d(:,:,:,:)

      integer(kind=INT32), pointer :: values_int32_5d(:,:,:,:,:)
      real(kind=REAL32),   pointer :: values_real32_5d(:,:,:,:,:)
      integer(kind=INT32), pointer :: all_int32_5d(:,:,:,:,:)
      real(kind=REAL32),   pointer :: all_real32_5d(:,:,:,:,:)

      integer(kind=INT64), pointer :: values_int64_0d
      real(kind=REAL64),   pointer :: values_real64_0d
      integer(kind=INT64), pointer :: all_int64_0d
      real(kind=REAL64),   pointer :: all_real64_0d

      integer(kind=INT64), pointer :: values_int64_1d(:)
      real(kind=REAL64),   pointer :: values_real64_1d(:)
      integer(kind=INT64), pointer :: all_int64_1d(:)
      real(kind=REAL64),   pointer :: all_real64_1d(:)

      integer(kind=INT64), pointer :: values_int64_2d(:,:)
      real(kind=REAL64),   pointer :: values_real64_2d(:,:)
      integer(kind=INT64), pointer :: all_int64_2d(:,:)
      real(kind=REAL64),   pointer :: all_real64_2d(:,:)

      integer(kind=INT64), pointer :: values_int64_3d(:,:,:)
      real(kind=REAL64),   pointer :: values_real64_3d(:,:,:)
      integer(kind=INT64), pointer :: all_int64_3d(:,:,:)
      real(kind=REAL64),   pointer :: all_real64_3d(:,:,:)

      integer(kind=INT64), pointer :: values_int64_4d(:,:,:,:)
      real(kind=REAL64),   pointer :: values_real64_4d(:,:,:,:)
      integer(kind=INT64), pointer :: all_int64_4d(:,:,:,:)
      real(kind=REAL64),   pointer :: all_real64_4d(:,:,:,:)

      integer(kind=INT64), pointer :: values_int64_5d(:,:,:,:,:)
      real(kind=REAL64),   pointer :: values_real64_5d(:,:,:,:,:)
      integer(kind=INT64), pointer :: all_int64_5d(:,:,:,:,:)
      real(kind=REAL64),   pointer :: all_real64_5d(:,:,:,:,:)

      integer,allocatable :: count(:),start(:)
      integer :: full_rank

      full_rank = size(global_shape)
      if(size(this%shape) > full_rank) then
        _ASSERT(.false.,"ranks do not agree (probably fixable)")
      endif

      allocate(count(full_rank))
      allocate(start(full_rank))

      ! Pad any extra dimensions with 1's

      count(1:full_rank) = 1
      count(1:size(this%shape)) = this%shape
 
      start(1:full_rank) = 1
      if(present(offset_start)) then
         start(1:size(offset_start))=offset_start
      endif

      select case (size(global_shape))
         case(0) ! scalar
            select case (this%type_kind)
            case(pFIO_INT32)
               call c_f_pointer(this%base_address,values_int32_0d)
               call c_f_pointer(offset_address, all_int32_0d)
               values_int32_0d=all_int32_0d
            case(pFIO_REAL32)
               call c_f_pointer(this%base_address,values_real32_0d)
               call c_f_pointer(offset_address, all_real32_0d)
               values_real32_0d=all_real32_0d
            case(pFIO_INT64)
               call c_f_pointer(this%base_address,values_int64_0d)
               call c_f_pointer(offset_address, all_int64_0d)
               values_int64_0d=all_int64_0d
            case(pFIO_REAL64)
               call c_f_pointer(this%base_address,values_real64_0d)
               call c_f_pointer(offset_address, all_real64_0d)
               values_real64_0d=all_real64_0d
            case default
               _ASSERT(.false.,"type not supported yet")
            end select
         case(1)
            s1=start(1)
            e1=start(1)+count(1)-1
            select case (this%type_kind)
            case(pFIO_INT32)
               call c_f_pointer(this%base_address,values_int32_1d,count)
               call c_f_pointer(offset_address, all_int32_1d,   global_shape)
               values_int32_1d=all_int32_1d(s1:e1)
            case(pFIO_REAL32)
               call c_f_pointer(this%base_address,values_real32_1d,count)
               call c_f_pointer(offset_address, all_real32_1d,   global_shape)
               values_real32_1d=all_real32_1d(s1:e1)
            case(pFIO_INT64)
               call c_f_pointer(this%base_address,values_int64_1d,count)
               call c_f_pointer(offset_address, all_int64_1d,   global_shape)
               values_int64_1d=all_int64_1d(s1:e1)
            case(pFIO_REAL64)
               call c_f_pointer(this%base_address,values_real64_1d,count)
               call c_f_pointer(offset_address, all_real64_1d,   global_shape)
               values_real64_1d=all_real64_1d(s1:e1)
            case default
               _ASSERT(.false.,"type not supported yet")
            end select
        case(2)
            s1=start(1)
            e1=start(1)+count(1)-1
            s2=start(2)
            e2=start(2)+count(2)-1
            select case (this%type_kind)
            case(pFIO_INT32)
               call c_f_pointer(this%base_address,values_int32_2d,count)
               call c_f_pointer(offset_address, all_int32_2d,   global_shape)
               values_int32_2d=all_int32_2d(s1:e1,s2:e2)
            case(pFIO_REAL32)
               call c_f_pointer(this%base_address,values_real32_2d,count)
               call c_f_pointer(offset_address, all_real32_2d,   global_shape)
               values_real32_2d=all_real32_2d(s1:e1,s2:e2)
            case(pFIO_INT64)
               call c_f_pointer(this%base_address,values_int64_2d,count)
               call c_f_pointer(offset_address, all_int64_2d,   global_shape)
               values_int64_2d=all_int64_2d(s1:e1,s2:e2)
            case(pFIO_REAL64)
               call c_f_pointer(this%base_address,values_real64_2d,count)
               call c_f_pointer(offset_address, all_real64_2d,   global_shape)
               values_real64_2d=all_real64_2d(s1:e1,s2:e2)
            case default
               _ASSERT(.false.,"type not supported yet")
            end select
        case (3)
            s1=start(1)
            e1=start(1)+count(1)-1
            s2=start(2)
            e2=start(2)+count(2)-1
            s3=start(3)
            e3=start(3)+count(3)-1
            select case (this%type_kind)
            case(pFIO_INT32)
               call c_f_pointer(this%base_address,values_int32_3d,count)
               call c_f_pointer(offset_address, all_int32_3d,   global_shape)
               values_int32_3d=all_int32_3d(s1:e1,s2:e2,s3:e3)
            case(pFIO_REAL32)
               call c_f_pointer(this%base_address,values_real32_3d,count)
               call c_f_pointer(offset_address, all_real32_3d,   global_shape)
               values_real32_3d=all_real32_3d(s1:e1,s2:e2,s3:e3)
            case(pFIO_INT64)
               call c_f_pointer(this%base_address,values_int64_3d,count)
               call c_f_pointer(offset_address, all_int64_3d,   global_shape)
               values_int64_3d=all_int64_3d(s1:e1,s2:e2,s3:e3)
            case(pFIO_REAL64)
               call c_f_pointer(this%base_address,values_real64_3d,count)
               call c_f_pointer(offset_address, all_real64_3d,   global_shape)
               values_real64_3d=all_real64_3d(s1:e1,s2:e2,s3:e3)
            case default
               _ASSERT(.false.,"type not supported yet")
            end select
        case (4)
            s1=start(1)
            e1=start(1)+count(1)-1
            s2=start(2)
            e2=start(2)+count(2)-1
            s3=start(3)
            e3=start(3)+count(3)-1
            s4=start(4)
            e4=start(4)+count(4)-1
            select case (this%type_kind)
            case(pFIO_INT32)
               call c_f_pointer(this%base_address,values_int32_4d,count)
               call c_f_pointer(offset_address, all_int32_4d,   global_shape)
               values_int32_4d=all_int32_4d(s1:e1,s2:e2,s3:e3,s4:e4)
            case(pFIO_REAL32)
               call c_f_pointer(this%base_address,values_real32_4d,count)
               call c_f_pointer(offset_address, all_real32_4d,   global_shape)
               values_real32_4d=all_real32_4d(s1:e1,s2:e2,s3:e3,s4:e4)
            case(pFIO_INT64)
               call c_f_pointer(this%base_address,values_int64_4d,count)
               call c_f_pointer(offset_address, all_int64_4d,   global_shape)
               values_int64_4d=all_int64_4d(s1:e1,s2:e2,s3:e3,s4:e4)
            case(pFIO_REAL64)
               call c_f_pointer(this%base_address,values_real64_4d,count)
               call c_f_pointer(offset_address, all_real64_4d,   global_shape)
               values_real64_4d=all_real64_4d(s1:e1,s2:e2,s3:e3,s4:e4)
            case default
               _ASSERT(.false.,"type not supported yet")
            end select

        case (5)
            s1=start(1)
            e1=start(1)+count(1)-1
            s2=start(2)
            e2=start(2)+count(2)-1
            s3=start(3)
            e3=start(3)+count(3)-1
            s4=start(4)
            e4=start(4)+count(4)-1
            s5=start(5)
            e5=start(5)+count(5)-1
            select case (this%type_kind)
            case(pFIO_INT32)
               call c_f_pointer(this%base_address,values_int32_5d,count)
               call c_f_pointer(offset_address, all_int32_5d,   global_shape)
               values_int32_5d=all_int32_5d(s1:e1,s2:e2,s3:e3,s4:e4,s5:e5)
            case(pFIO_REAL32)
               call c_f_pointer(this%base_address,values_real32_5d,count)
               call c_f_pointer(offset_address, all_real32_5d,   global_shape)
               values_real32_5d=all_real32_5d(s1:e1,s2:e2,s3:e3,s4:e4,s5:e5)
            case(pFIO_INT64)
               call c_f_pointer(this%base_address,values_int64_5d,count)
               call c_f_pointer(offset_address, all_int64_5d,   global_shape)
               values_int64_5d=all_int64_5d(s1:e1,s2:e2,s3:e3,s4:e4,s5:e5)
            case(pFIO_REAL64)
               call c_f_pointer(this%base_address,values_real64_5d,count)
               call c_f_pointer(offset_address, all_real64_5d,   global_shape)
               values_real64_5d=all_real64_5d(s1:e1,s2:e2,s3:e3,s4:e4,s5:e5)
            case default
               _ASSERT(.false.,"type not supported yet")
            end select

         case default
            _ASSERT(.false.,"dimension not supported yet")
      end select

   end subroutine fetch_data

   integer function get_length_base(this) result(length)
      class (AbstractDataReference), intent(in) :: this
      integer :: n
      
      if( allocated(this%shape)) then
         n = size(this%shape)
      else
         n = 0
      endif

      length = size(transfer(this%base_address,[1])) +  &
               1 + 1 + & ! type_kind + rank 
               n  ! shape

   end function get_length_base


   subroutine serialize_base(this, buffer, rc)
      class (AbstractDataReference), intent(in) :: this
      integer, allocatable :: buffer(:)
      integer, optional, intent(out) :: rc

      integer :: n, rank

      if(allocated(buffer)) deallocate(buffer)
      allocate(buffer(this%get_length()))

      n = size(transfer(this%base_address,[1]))
    
      if( allocated(this%shape)) then
         rank = size(this%shape)
      else
         rank = 0
      endif

      buffer(1:n)  = transfer(this%base_address, [1])
      buffer(n+1)  = this%type_kind
      buffer(n+2)  = rank
      if (rank > 0) buffer(n+3:) = this%shape
      _RETURN(_SUCCESS)
   end subroutine serialize_base

   subroutine deserialize_base(this, buffer, rc)
      class (AbstractDataReference), intent(inout) :: this
      integer, intent(in) :: buffer(:)
      integer, optional, intent(out) :: rc

      integer :: n_base, n_rank
      
      n_base = size(transfer(this%base_address,[1]))
      _ASSERT( size(buffer) >= n_base+2, "buffer size is too small")

      this%base_address = transfer(buffer(1:n_base), this%base_address)
      this%type_kind = buffer(n_base+1)
      n_rank = buffer(n_base+2)
      allocate(this%shape(n_rank))
      this%shape = buffer(n_base+3:n_base+3+n_rank-1)
      _RETURN(_SUCCESS)
   end subroutine deserialize_base
      
   logical function equal(a, b)
      class (AbstractDataReference), intent(in) :: a
      class (AbstractDataReference), intent(in) :: b

      equal = (allocated(a%shape) .eqv. allocated(a%shape))
      if (.not. equal) return

      if (allocated(a%shape) .and. allocated(a%shape)) then
         equal = (size(a%shape) == size(b%shape))
         if (.not. equal) return

         equal = all(a%shape == b%shape)
         if (.not. equal) return
      endif      

      equal = (a%type_kind == b%type_kind)
      if (.not. equal) return

      equal = c_associated(a%base_address, b%base_address)
      
   end function equal


   subroutine allocate(this, rc)
      class (AbstractDataReference), intent(inout) :: this
      integer, optional, intent(out) :: rc
   end subroutine allocate

   subroutine deallocate(this, rc)
      class (AbstractDataReference), intent(inout) :: this
      integer, optional, intent(out) :: rc
   end subroutine deallocate

   function convert_addr(this) result(long)
     class(AbstractDataReference), target, intent(in) :: this
     integer(kind=INT64) :: long

     long = transfer(this%base_address, long)

   end function convert_addr

   subroutine fence(this, rc)
     class(AbstractDataReference),intent(inout) :: this
     integer, optional, intent(out) :: rc
     print*, "WARNNING: should fence in the sub class"
   end subroutine fence

   subroutine copy_data_to(this,to, rc)
     class(AbstractDataReference),intent(in) :: this
     class(AbstractDataReference),intent(inout) :: to
     integer, optional, intent(out) :: rc

     integer,pointer :: fromPtr(:)
     integer,pointer :: toPtr(:)
  
     integer(kind=INT64) :: n_words,n

     n_words = product(int(this%shape,INT64))*word_size(this%type_kind)
     n       = product(int(to%shape,INT64))*word_size(to%type_kind)
     
     _ASSERT(this%type_kind == to%type_kind,"copy type_kind not match")
     _ASSERT(n_words == n, "copy size does not match")  
     call c_f_pointer(this%base_address,fromPtr,[n])
     call c_f_pointer(to%base_address,toPtr,[n])
     toPtr(1:n) = fromPtr(1:n)
     _RETURN(_SUCCESS)
   end subroutine copy_data_to

end module pFIO_AbstractDataReferenceMod

module pFIO_AbstractDataReferenceVectorMod
   use pFIO_AbstractDataReferenceMod

#define _type class (AbstractDataReference)
#define _pointer
#define _vector AbstractDataReferenceVector
#define _iterator AbstractDataReferenceVectorIterator
#include "templates/vector.inc"

end module pFIO_AbstractDataReferenceVectorMod
