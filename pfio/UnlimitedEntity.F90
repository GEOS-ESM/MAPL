#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module pFIO_UnlimitedEntityMod

!BOP
! !MODULE: pFIO_UnlimitedEntityMod - Encapsulates notion of variable UnlimitedEntitys
!
! !DESCRIPTION: 
!
!  {\tt CFI\_AtttributeMod} is a support layer for the CFIO package and
!  which implements encapsulates variable UnlimitedEntitys ala NetCDF.
!  An UnlimitedEntity can be any Fortan kind/type and can either be a scalar
!  or a 1-dimensional vector.   (Strings can only be scalars.)
!  
!  While some functionality exists for containing non-intrinsic types
!  (e.g., user defined types),  the primary intended use is for
!  establishing correspondence with intrinsic types stored in files.
!

! !USES:
!



   use pFIO_ConstantsMod
   use pFIO_UtilitiesMod
   use MAPL_ExceptionHandling
   use, intrinsic :: iso_fortran_env, only: INT32, INT64
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64

   implicit none
   private

   public :: UnlimitedEntity
   public :: UnlimitedEntity_deserialize
   public :: StringWrap

   type :: UnlimitedEntity
      private
      integer, allocatable :: shape(:)
      class (*), allocatable :: value
      class (*), allocatable :: values(:)
   contains
      procedure :: get_shape
      procedure :: get_rank
      procedure :: get_value
      procedure :: get_values
      generic :: operator(==) => equal
      generic :: operator(/=) => not_equal
      procedure :: equal
      procedure :: not_equal
      procedure :: set
      procedure :: serialize
      procedure :: get_string
      procedure :: is_empty
   end type UnlimitedEntity

   ! This derived type is a workaround for sporadic Intel Fortran
   ! issues when accessing strings through unlimited polymorphic
   ! entities.
   type :: StringWrap
     character(len=:), allocatable :: value
   end type StringWrap


   interface UnlimitedEntity
      module procedure new_UnlimitedEntity_empty
      module procedure new_UnlimitedEntity_0d ! scalar constructor
      module procedure new_UnlimitedEntity_1d ! vector constructor
      module procedure new_UnlimitedEntity_2d ! vector constructor
      module procedure new_UnlimitedEntity_3d ! vector constructor
      module procedure new_UnlimitedEntity_4d ! vector constructor
      module procedure new_UnlimitedEntity_5d ! vector constructor
   end interface UnlimitedEntity

   integer :: EMPTY(0)


contains

   function new_UnlimitedEntity_empty() result(attr)
      type (UnlimitedEntity) :: attr
   end function new_UnlimitedEntity_empty
   

   function new_UnlimitedEntity_0d(value, rc) result(attr)
      type (UnlimitedEntity) :: attr
      class (*), intent(in) :: value
      integer, optional, intent(out) :: rc
      type(StringWrap) :: w

      attr%shape = EMPTY
      select type (value)
      type is (character(len=*)) ! workaround for gfortran-6.2 and ifort-17.0.1
        w = StringWrap('')
        w%value = value
        allocate(attr%value,source=w)
      class default
        allocate(attr%value, source=value)
      end select
      _RETURN(_SUCCESS)
   end function new_UnlimitedEntity_0d
   
   function new_UnlimitedEntity_1d(values, rc) result(attr)
      type (UnlimitedEntity) :: attr
      class (*), intent(in) :: values(:)
      integer, optional, intent(out) :: rc

      select type (values)
      type is (character(len=*)) 
        _ASSERT(.false., 'unsupported unless shape is [1]')
      class default
         allocate(attr%values, source=values)
         attr%shape = shape(values)
      end select

      _RETURN(_SUCCESS)
   end function new_UnlimitedEntity_1d

   function new_UnlimitedEntity_2d(values, rc) result(attr)
      type (UnlimitedEntity) :: attr
      class (*), intent(in) :: values(:,:)
      integer, optional, intent(out) :: rc
      class (*), allocatable :: values1d(:)

      select type (values)
      type is (integer(INT32))
         allocate(values1d, source = reshape(values, [product(shape(values))]))
      type is (integer(INT64))
         allocate(values1d, source = reshape(values, [product(shape(values))]))
      type is (real(real32))
         allocate(values1d, source = reshape(values, [product(shape(values))]))
      type is (real(real64))
         allocate(values1d, source = reshape(values, [product(shape(values))]))
      type is (logical)
         allocate(values1d, source = reshape(values, [product(shape(values))]))
      class default
        _ASSERT(.false., 'not support type')
      end select

      attr = UnlimitedEntity(values1d)
      attr%shape = shape(values)

      deallocate(values1d)
      _RETURN(_SUCCESS)
   end function new_UnlimitedEntity_2d

   function new_UnlimitedEntity_3d(values, rc) result(attr)
      type (UnlimitedEntity) :: attr
      class (*), intent(in) :: values(:,:,:)
      integer, optional, intent(out) :: rc
      class (*), allocatable :: values1d(:)

      select type (values)
      type is (integer(INT32))
         allocate(values1d, source = reshape(values, [product(shape(values))]))
      type is (integer(INT64))
         allocate(values1d, source = reshape(values, [product(shape(values))]))
      type is (real(real32))
         allocate(values1d, source = reshape(values, [product(shape(values))]))
      type is (real(real64))
         allocate(values1d, source = reshape(values, [product(shape(values))]))
      type is (logical)
         allocate(values1d, source = reshape(values, [product(shape(values))]))
      class default
        _ASSERT(.false., 'not support type')
      end select

      attr = UnlimitedEntity(values1d)
      attr%shape = shape(values)

      deallocate(values1d)      
      _RETURN(_SUCCESS)
   end function new_UnlimitedEntity_3d

   function new_UnlimitedEntity_4d(values, rc) result(attr)
      type (UnlimitedEntity) :: attr
      class (*), intent(in) :: values(:,:,:,:)
      integer, optional, intent(out) :: rc
      class (*), allocatable :: values1d(:)

      select type (values)
      type is (integer(INT32))
         allocate(values1d, source = reshape(values, [product(shape(values))]))
      type is (integer(INT64))
         allocate(values1d, source = reshape(values, [product(shape(values))]))
      type is (real(real32))
         allocate(values1d, source = reshape(values, [product(shape(values))]))
      type is (real(real64))
         allocate(values1d, source = reshape(values, [product(shape(values))]))
      type is (logical)
         allocate(values1d, source = reshape(values, [product(shape(values))]))
      class default
        _ASSERT(.false., 'not support type')
      end select

      attr = UnlimitedEntity(values1d)
      attr%shape = shape(values)

      deallocate(values1d)      
      _RETURN(_SUCCESS)
   end function new_UnlimitedEntity_4d

   function new_UnlimitedEntity_5d(values, rc) result(attr)
      type (UnlimitedEntity) :: attr
      class (*), intent(in) :: values(:,:,:,:,:)
      integer, optional, intent(out) :: rc
      class (*), allocatable :: values1d(:)

      select type (values)
      type is (integer(INT32))
         allocate(values1d, source = reshape(values, [product(shape(values))]))
      type is (integer(INT64))
         allocate(values1d, source = reshape(values, [product(shape(values))]))
      type is (real(real32))
         allocate(values1d, source = reshape(values, [product(shape(values))]))
      type is (real(real64))
         allocate(values1d, source = reshape(values, [product(shape(values))]))
      type is (logical)
         allocate(values1d, source = reshape(values, [product(shape(values))]))
      class default
        _ASSERT(.false., 'not support type')
      end select

      attr = UnlimitedEntity(values1d)
      attr%shape = shape(values)

      deallocate(values1d)      
      _RETURN(_SUCCESS)
   end function new_UnlimitedEntity_5d

   ! set string or scalar
   subroutine set(this, value, rc)
      class (UnlimitedEntity), intent(inout) :: this
      class (*), intent(in) :: value
      integer, optional, intent(out) :: rc
      
      type (StringWrap) :: w

      if(allocated(this%value)) deallocate(this%value)

      select type (q => value)
      type is (character(len=*))
         w = StringWrap('') ! Intel compiler workaround
         w%value = q
         allocate(this%value, source=w)
      class default
         allocate(this%value, source=value)
      end select
      this%shape = EMPTY
      _RETURN(_SUCCESS)
   end subroutine set

   ! get string or scalar
   function get_value(this, rc) result(value)
      class (UnlimitedEntity), target, intent(in) :: this
      integer, optional, intent(out) :: rc
      class (*), pointer :: value

      if (allocated(this%value)) then
         select type (q => this%value)
         type is (StringWrap)
            value => q%value
         class default
            value => q
         end select
      else
         value => null()
      end if
      _RETURN(_SUCCESS)
   end function get_value

   ! get 1d , need get_shape to get back to original array
   function get_values(this, rc) result(values)
      class (UnlimitedEntity), target, intent(in) :: this
      integer, optional, intent(out) :: rc
      class (*), pointer :: values(:)

      if (allocated(this%values)) then
        values => this%values
      else
        values => null()
      end if
      _RETURN(_SUCCESS)
   end function get_values

   function get_string(this,rc) result(string)
      class(UnlimitedEntity), target, intent(in) :: this
      integer, optional, intent(out) :: rc
      character(len=:), allocatable :: string
      class (*), pointer :: value
      
      value => this%get_value()
      select type(value)
      type is (StringWrap)
         string = value%value
      type is (character(len=*))
         string = value
      class default
         _RETURN(_FAILURE)
      end select    
      _RETURN(_SUCCESS)
   end function get_string

   ! Simple accessor
   function get_shape(this, rc) result(shp)
      class (UnlimitedEntity), intent(in) :: this
      integer, optional, intent(out) :: rc
      integer, allocatable :: shp(:)
      
      if (.not. allocated(this%shape)) then
         shp = EMPTY
      else
         shp = this%shape
      endif
      _RETURN(_SUCCESS)
   end function get_shape

   function get_rank(this, rc) result(rank)
      class (UnlimitedEntity), intent(in) :: this
      integer, optional, intent(out) :: rc
      integer :: rank

      rank = size(this%get_shape())

      _RETURN(_SUCCESS)
   end function get_rank

   function is_empty(this, rc) result(yes)
      class (UnlimitedEntity), target, intent(in) :: this
      integer, optional, intent(out) :: rc
      logical :: yes

      class (*), pointer :: value

      ! not initialized
      yes = .not. allocated(this%shape)
      if (yes) then
        _RETURN(_SUCCESS)
      endif
      
      ! initialized with UnlimitedEnity('')
      value => this%get_value()
      if (associated(value)) then
         select type (q=>value) 
         type is (character(len=*))
             yes = (q == '')
         end select
      end if

      _RETURN(_SUCCESS)
   end function is_empty

   logical function equal(a, b)
!BOP
!
! !IROUTINE: equals() - returns true if-and-only-if a and b
! are the same type, kind, shape, and have the same values.
! !DESCRIPTION: 
!
! Only intrinsic data types are supported.
!
! Ugly nested SELECT TYPE is unfortunately necessary.
!
! !INTERFACE:
      class (UnlimitedEntity), target, intent(in) :: a
      type (UnlimitedEntity), target, intent(in) :: b

      integer, allocatable :: shape_a(:), shape_b(:)
      class (*), pointer :: value_a, value_b
      class (*), pointer :: values_a(:), values_b(:)
      integer :: rank

      ! check initialization
      equal = (.not. allocated(a%shape) .and. .not. allocated(b%shape))
      if (equal) return ! both are not initialized

      equal = allocated(a%shape) .and. allocated(b%shape)
      if (.not. equal) return ! one of them is not initialized
    
      ! check size
      shape_a = a%get_shape()
      shape_b = b%get_shape()
      equal = size(shape_a) == size(shape_b)
      if (.not. equal) return
      ! check shape
      equal = all(shape_a == shape_b)
      if (.not. equal) return

      rank = a%get_rank()

      if ( rank > 0) then ! at this point, both a%values and b%values must have been allocated
         ! check type
         values_a => a%get_values()
         values_b => b%get_values()
         equal = same_type_as(values_a, values_b)
         if (.not. equal) return

         select type (values_a)
         type is (integer(INT32))
            select type (values_b)
            type is (integer(INT32))
               equal = all(values_a == values_b)
            end select
         type is (integer(INT64))
            select type (values_b)
            type is (integer(INT64))
               equal = all(values_a == values_b)
            end select
         type is (real(real32))
            select type (values_b)
            type is (real(real32))
               equal = all(nearlyEqual(values_a, values_b))
            end select
         type is (real(real64))
            select type (values_b)
            type is (real(real64))
               equal = all(nearlyEqual(values_a, values_b))
            end select
         type is (logical)
            select type (values_b)
            type is (logical)
               equal = all(values_a .eqv. values_b)
            end select
         !W.J notes: it is illegal and not used
         !type is (character(len=*))
         !   select type (values_b)
         !   type is (character(len=*))
         !      equal = all(values_a == values_b)
         !   end select
         class default
            equal = .false.
         end select
      else 
         ! check type
         value_a => a%get_value()
         value_b => b%get_value()
         equal = same_type_as(value_a, value_b)
         if (.not. equal) return


         select type (value_a)
         type is (integer(INT32))
            select type (value_b)
            type is (integer(INT32))
               equal = (value_a == value_b)
            end select
         type is (integer(INT64))
            select type (value_b)
            type is (integer(INT64))
               equal = (value_a == value_b)
            end select
         type is (real(real32))
            select type (value_b)
            type is (real(real32))
               equal = (nearlyEqual(value_a,value_b))
            end select
         type is (real(real64))
            select type (value_b)
            type is (real(real64))
               equal = (nearlyEqual(value_a,value_b))
            end select
         type is (logical)
            select type (value_b)
            type is (logical)
               equal = (value_a .eqv. value_b)
            end select
         type is (character(len=*))
            select type (value_b)
            type is (character(len=*))
               equal = (value_a == value_b)
            end select
         ! W.J notes:  get_value will not return this type
         !type is (StringWrap)
         !   select type (value_b)
         !   type is (StringWrap)
         !      equal = (value_a%value == value_b%value)
         !   end select
         class default
            equal = .false.
         end select
      end if

   end function equal


   logical function not_equal(a, b)
      class (UnlimitedEntity), intent(in) :: a
      type (UnlimitedEntity), intent(in) :: b
      not_equal = .not. (a == b)
   end function not_equal

   subroutine serialize( this, buffer, rc)
      class (UnlimitedEntity),target, intent(in) :: this
      integer, allocatable,intent(inout) :: buffer(:)
      integer, optional, intent(out) :: rc
      integer :: type_kind
      integer :: length, rank

      if(allocated(buffer)) deallocate(buffer)

      buffer = EMPTY 
      rank = this%get_rank()

      select case (rank)
      case (0)
         if ( .not. allocated(this%value)) then
            type_kind = pFIO_UNSUPPORTED_TYPE
            buffer = [serialize_intrinsic(EMPTY), &
                      serialize_intrinsic(type_kind)]
         else         
         ! check type
         select type (value => this%value)
         type is (integer(INT32))
            type_kind = pFIO_INT32
            buffer = [serialize_intrinsic(this%shape), &
                      serialize_intrinsic(type_kind),  &
                      serialize_intrinsic(value)]
         type is (integer(INT64))
            type_kind = pFIO_INT64
            buffer = [serialize_intrinsic(this%shape), &
                      serialize_intrinsic(type_kind),  &
                      serialize_intrinsic(value)]
         type is (real(real32))
            type_kind = pFIO_REAL32
            buffer = [serialize_intrinsic(this%shape), &
                      serialize_intrinsic(type_kind),  &
                      serialize_intrinsic(value)]
         type is (real(real64))
            type_kind = pFIO_REAL64
            buffer = [serialize_intrinsic(this%shape), &
                      serialize_intrinsic(type_kind),  &
                      serialize_intrinsic(value)]
         type is (logical)
            type_kind = pFIO_LOGICAL
            buffer = [serialize_intrinsic(this%shape), &
                      serialize_intrinsic(type_kind),  &
                      serialize_intrinsic(value)]
         type is (StringWrap)
            type_kind = pFIO_STRING
            buffer = [serialize_intrinsic(this%shape), &
                      serialize_intrinsic(type_kind),  &
                      serialize_intrinsic(value%value)]
         class default
            _ASSERT(.false.," type is not supported")
         end select
         endif
      case (1:)
         ! check type
         select type (values=>this%values)
         type is (integer(INT32))
            type_kind = pFIO_INT32
            buffer = [serialize_intrinsic(this%shape), &
                      serialize_intrinsic(type_kind),  &
                      serialize_intrinsic(values)]
         type is (integer(INT64))
            type_kind = pFIO_INT64
            buffer = [serialize_intrinsic(this%shape), &
                      serialize_intrinsic(type_kind),  &
                      serialize_intrinsic(values)]
         type is (real(REAL32))
            type_kind = pFIO_REAL32
            buffer = [serialize_intrinsic(this%shape), &
                      serialize_intrinsic(type_kind),  &
                      serialize_intrinsic(values)]
         type is (real(REAL64))
            type_kind = pFIO_REAL64
            buffer = [serialize_intrinsic(this%shape), &
                      serialize_intrinsic(type_kind),  &
                      serialize_intrinsic(values)]
         type is (logical)
            type_kind = pFIO_LOGICAL
            buffer = [serialize_intrinsic(this%shape), &
                      serialize_intrinsic(type_kind),  &
                      serialize_intrinsic(values)]
        ! type is (character(len=*))
        !    type_kind = pFIO_STRING
        !    buffer = [serialize_intrinsic(this%shape), &
        !              serialize_intrinsic(type_kind),  &
        !              serialize_intrinsic(values)]
         class default
            _ASSERT(.false.," type is not supported")
         end select
      end select
      length =  serialize_buffer_length(length) + size(buffer)
      buffer = [serialize_intrinsic(length),buffer]
      _RETURN(_SUCCESS)
   end subroutine serialize 

   subroutine UnlimitedEntity_deserialize( buffer,this, rc)
      integer, intent(in) :: buffer(:)
      type (UnlimitedEntity),intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer :: status
      this = UnlimitedEntity() 
      call deserialize(this, buffer, rc=status)
      _VERIFY(status)
      _RETURN(_SUCCESS)
   contains

      subroutine deserialize( this, buffer, rc)
         class (UnlimitedEntity), target,intent(inout) :: this
         integer, intent(in) :: buffer(:)
         integer, optional, intent(out) :: rc
   
         integer :: n,type_kind,length
   
         integer(KIND=INT32) :: value_int32
         integer(KIND=INT64) :: value_int64
         real(KIND=REAL32)   :: value_real32
         real(KIND=REAL64)   :: value_real64
         logical :: value_logical
   
         integer(KIND=INT32), allocatable :: values_int32(:)
         integer(KIND=INT64), allocatable :: values_int64(:)
         real(KIND=REAL32), allocatable :: values_real32(:)
         real(KIND=REAL64), allocatable :: values_real64(:)
         logical, allocatable :: values_logical(:)
   
         character(len=:), allocatable :: value_char
         integer :: rank
   
         n = 1
         call deserialize_intrinsic(buffer(n:),length)
         _ASSERT(length == size(buffer),'length does not match')
   
         n = n + serialize_buffer_length(length)
         call deserialize_intrinsic(buffer(n:),this%shape)
         n = n + serialize_buffer_length(this%shape)
         call deserialize_intrinsic(buffer(n:),type_kind)
         n = n + serialize_buffer_length(type_kind)
   
         rank = this%get_rank()
         select case (rank)
         case (0)
            select case (type_kind)
            case (pFIO_INT32)
                call deserialize_intrinsic(buffer(n:),value_int32)
                call this%set(value_int32)
            case (pFIO_INT64)
                call deserialize_intrinsic(buffer(n:),value_int64)
                call this%set(value_int64)
            case (pFIO_REAL32)
                call deserialize_intrinsic(buffer(n:),value_real32)
                call this%set(value_real32)
            case (pFIO_REAL64)
                call deserialize_intrinsic(buffer(n:),value_real64)
                call this%set(value_real64)
            case (pFIO_LOGICAL)
                call deserialize_intrinsic(buffer(n:),value_logical)
                call this%set(value_logical)
            case (pFIO_STRING)
                call deserialize_intrinsic(buffer(n:),value_char)
                call this%set(value_char)
            case (pFIO_UNSUPPORTED_TYPE)
                ! this is uninitialized case, make sure shape is not allocated even it is empty
                 if (allocated(this%shape))deallocate(this%shape)
            case default
              _ASSERT(.false., "UnlimitedEntity deserialize not support")
            end select
         case (1:)
            select case (type_kind)
            case (pFIO_INT32)
                call deserialize_intrinsic(buffer(n:),values_int32)
                allocate(this%values, source =values_int32)
            case (pFIO_INT64)
                call deserialize_intrinsic(buffer(n:),values_int64)
                allocate(this%values, source =values_int64)
            case (pFIO_REAL32)
                call deserialize_intrinsic(buffer(n:),values_real32)
                allocate(this%values, source =values_real32)
            case (pFIO_REAL64)
                call deserialize_intrinsic(buffer(n:),values_real64)
                allocate(this%values, source =values_real64)
            case (pFIO_LOGICAL)
                call deserialize_intrinsic(buffer(n:),values_logical)
                allocate(this%values, source =values_logical)
            case default
              _ASSERT(.false., "UnlimitedEntity deserialize not support")
            end select
   
         end select
         _RETURN(_SUCCESS)
      end subroutine deserialize
   end subroutine UnlimitedEntity_deserialize

end module pFIO_UnlimitedEntityMod


! The following module defines an FTL map (associative array) with keys that are deferred
! length strings and values that are UnlimitedEntitys.

module pFIO_StringUnlimitedEntityMapMod
   use pFIO_UnlimitedEntityMod
   
#include "types/key_deferredLengthString.inc"   
#define _value type (UnlimitedEntity)
#define _value_equal_defined

#define _map StringUnlimitedEntityMap
#define _iterator StringUnlimitedEntityMapIterator

#define _alt
#include "templates/map.inc"
   
end module pFIO_StringUnlimitedEntityMapMod

module pFIO_StringUnlimitedEntityMapUtilMod
   use pFIO_UtilitiesMod
   use pFIO_UnlimitedEntityMod
   use pFIO_StringUnlimitedEntityMapMod
   use MAPL_ExceptionHandling
   implicit none
   private

   public :: StringUnlimitedEntityMap_serialize
   public :: StringUnlimitedEntityMap_deserialize

contains

    subroutine StringUnlimitedEntityMap_serialize(map,buffer)
       type (StringUnlimitedEntityMap) ,intent(in):: map
       integer, allocatable,intent(inout) :: buffer(:)
       type (StringUnlimitedEntityMapIterator) :: iter
       character(len=:),pointer :: key
       type(UnlimitedEntity),pointer :: attr_ptr
       integer :: length
       integer, allocatable :: tmp_buffer(:)

       if (allocated(buffer)) deallocate(buffer)
       allocate(buffer(0))
       iter = map%begin()
       do while (iter /= map%end())
          key => iter%key()
          buffer=[buffer,serialize_intrinsic(key)]
          attr_ptr => iter%value()
          call attr_ptr%serialize(tmp_buffer)
          buffer = [buffer, tmp_buffer]
          deallocate(tmp_buffer)
          call iter%next()
       enddo
       length = serialize_buffer_length(length)+size(buffer)
       buffer = [serialize_intrinsic(length),buffer]
    end subroutine StringUnlimitedEntityMap_serialize

    subroutine StringUnlimitedEntityMap_deserialize(buffer, map, rc) 
       integer, intent(in) :: buffer(:)
       type (StringUnlimitedEntityMap), intent(inout) :: map
       integer, optional, intent(out) :: rc

       character(len=:),allocatable :: key
       integer :: length,n,n0,n1,n2
       type (UnlimitedEntity) :: attr

       n = 1
       call deserialize_intrinsic(buffer(n:),length)
       n0 = serialize_buffer_length(length)
       n = n + n0
       length = length - n0

       map = StringUnlimitedEntityMap()
       do while (length > 0)
          call deserialize_intrinsic(buffer(n:),key)
          n1 = serialize_buffer_length(key)
          n = n + n1
          call deserialize_intrinsic(buffer(n:),n2)
          call UnlimitedEntity_deserialize(buffer(n:), attr)
          n = n + n2
          length = length - n1 - n2
          call map%insert(key,attr)
          deallocate(key)
       enddo
       _RETURN(_SUCCESS)
    end subroutine StringUnlimitedEntityMap_deserialize

end module pFIO_StringUnlimitedEntityMapUtilMod
