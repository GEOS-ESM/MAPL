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

   implicit none
   private

   public :: AbstractDataReference

   type,abstract :: AbstractDataReference
      type (c_ptr) :: base_address = C_NULL_PTR
      integer, allocatable :: shape(:)
      integer :: type_kind
   contains
      procedure :: fetch_data
      procedure :: get_length
      procedure :: serialize
      procedure :: deserialize
      procedure :: equal
      procedure :: allocate
      procedure :: deallocate
      procedure :: convert_addr
      generic :: operator(==) => equal
      procedure :: fence      
      procedure :: copy_data_to
   end type AbstractDataReference

contains

   subroutine fetch_data(this,offset_address,global_shape,offset_start)
      class(AbstractDataReference),target,intent(in)    :: this
      type(c_ptr),intent(in)       :: offset_address
      integer,intent(in)           :: global_shape(:)
      integer,optional,intent(in)  :: offset_start(:)

      integer :: s1,e1, s2,e2,s3,e3,s4,e4

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

      integer,allocatable :: count(:),start(:)
      integer :: full_rank

      full_rank = size(global_shape)
      if(size(this%shape) > full_rank) stop "ranks do not agree (probably fixable)"

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
            end select
         case default
            stop
      end select

   end subroutine fetch_data

   integer function get_length(this) result(length)
      class (AbstractDataReference), intent(in) :: this
      type (c_ptr) :: dummy

      integer :: size_c_ptr

      size_c_ptr = size(transfer(dummy,[1]))
      length = 2 + size_c_ptr + size(this%shape)

   end function get_length


   subroutine serialize(this, buffer)
      class (AbstractDataReference), intent(in) :: this
      integer, allocatable :: buffer(:)

      allocate(buffer(this%get_length()))

      buffer(1:2) = transfer(this%base_address, [1])
      buffer(3) = this%type_kind
      buffer(4) = size(this%shape)
      buffer(5:) = this%shape

   end subroutine serialize

   subroutine deserialize(this, buffer)
      class (AbstractDataReference), intent(inout) :: this
      integer, intent(in) :: buffer(:)

      integer :: n

      this%base_address = transfer(buffer(1:2), this%base_address)
      this%type_kind = buffer(3)
      n = buffer(4)
      this%shape = buffer(5:5+n-1)

   end subroutine deserialize
      

   logical function equal(a, b)
      class (AbstractDataReference), intent(in) :: a
      class (AbstractDataReference), intent(in) :: b

      equal = (size(a%shape) == size(b%shape))
      if (.not. equal) return

      equal = all(a%shape == b%shape)
      if (.not. equal) return
      
      equal = (a%type_kind == b%type_kind)
      if (.not. equal) return

      equal = c_associated(a%base_address, b%base_address)
      
   end function equal


   subroutine allocate(this)
      class (AbstractDataReference), intent(inout) :: this
   end subroutine allocate

   subroutine deallocate(this)
      class (AbstractDataReference), intent(inout) :: this
   end subroutine deallocate

   function convert_addr(this) result(long)
     class(AbstractDataReference), target, intent(in) :: this
     integer(kind=INT64) :: long

     long = transfer(this%base_address, long)

   end function convert_addr

   subroutine fence(this)
     class(AbstractDataReference),intent(inout) :: this
   end subroutine fence

   subroutine copy_data_to(this,to)
     class(AbstractDataReference),intent(in) :: this
     class(AbstractDataReference),intent(inout) :: to

     integer,pointer :: fromPtr(:)
     integer,pointer :: toPtr(:)
  
     integer:: n_words,n

     n_words = product(this%shape)*word_size(this%type_kind)
     n       = product(to%shape)*word_size(to%type_kind)
     
     if(this%type_kind /= to%type_kind) stop "type_kind not match"
     if(n_words /= n) stop "size not match"  
     call c_f_pointer(this%base_address,fromPtr,[n])
     call c_f_pointer(to%base_address,toPtr,[n])
     toPtr(1:n) = fromPtr(1:n)

   end subroutine copy_data_to

end module pFIO_AbstractDataReferenceMod
