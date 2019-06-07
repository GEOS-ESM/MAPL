module pFIO_AttributeMod

!BOP
! !MODULE: pFIO_AttributeMod - Encapsulates notion of variable attributes
!
! !DESCRIPTION: 
!
!  {\tt CFI\_AtttributeMod} is a support layer for the CFIO package and
!  which implements encapsulates variable attributes ala NetCDF.
!  An attribute can be any Fortan kind/type and can either be a scalar
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
   use, intrinsic :: iso_fortran_env, only: INT32, INT64
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64
   implicit none
   private

   public :: Attribute
   public :: StringWrap

   type :: Attribute
      private
      integer, allocatable :: shape(:)
      class (*), allocatable :: value
      class (*), allocatable :: values(:)
   contains
      procedure :: get_shape
      procedure :: get_value
      procedure :: get_values
      generic :: operator(==) => equal
      generic :: operator(/=) => not_equal
      procedure :: equal
      procedure :: not_equal
      procedure :: set
      procedure :: serialize
      procedure :: deserialize
   end type Attribute

   ! This derived type is a workaround for sporadic Intel Fortran
   ! issues when accessing strings through unlimited polymorphic
   ! entities.
   type :: StringWrap
     character(len=:), allocatable :: value
   end type StringWrap


   interface Attribute
      module procedure new_Attribute_0d ! scalar constructor
      module procedure new_Attribute_1d ! vector constructor
   end interface Attribute

   integer :: EMPTY(0)


contains


   function new_Attribute_0d(value) result(attr)
      type (Attribute) :: attr
      class (*), intent(in) :: value
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

   end function new_Attribute_0d
   

   function new_Attribute_1d(values) result(attr)
     use pFIO_ThrowMod
      type (Attribute) :: attr
      class (*), intent(in) :: values(:)

      select type (values)
      type is (character(len=*)) ! unsupported unless shape is [1]
        call pFIO_throw_exception(__FILE__,__LINE__)
        return
      class default
         attr%shape = shape(values)
         allocate(attr%values, source=values)
      end select


   end function new_Attribute_1d


   subroutine set(this, value)
!BOP
!
! !IROUTINE: set() - change the contained value of an Attribute.
!
! !INTERFACE:
      class (Attribute), intent(inout) :: this
      class (*), intent(in) :: value
      
      type (StringWrap) :: w

      select type (q => value)
      type is (character(len=*))
         w = StringWrap('') ! Intel compiler workaround
         w%value = q
         allocate(this%value, source=w)
      class default
         allocate(this%value, source=value)
      end select
      this%shape = EMPTY

   end subroutine set



   function get_value(this) result(value)
!BOP
!
! !IROUTINE: get_value() - get a pointer to contained value if scalar.
!
! !INTERFACE:
      class (Attribute), target, intent(in) :: this
      class (*), pointer :: value

      if (allocated(this%value)) then
!$$         select type (q => this%value)
!$$         type is (StringWrap)
!$$            value => q%value
!$$         class default
!$$            value => q
!$$         end select
         value => this%value
      else
         value => null()
      end if

    end function get_value


   function get_values(this) result(values)
!BOP
!
! !IROUTINE: get_values() - get a pointer to contained value if vector.
!
! !INTERFACE:
      class (Attribute), target, intent(in) :: this
      class (*), pointer :: values(:)

      if (allocated(this%values)) then
        values => this%values
      else
        values => null()
      end if

   end function get_values


   ! Simple accessor
   function get_shape(this) result(shp)
      integer, allocatable :: shp(:)
      class (Attribute), intent(in) :: this

      shp = this%shape

   end function get_shape


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
      class (Attribute), target, intent(in) :: a
      class (Attribute), target, intent(in) :: b

      integer, allocatable :: shape_a(:), shape_b(:)
      class (*), pointer :: value_a, value_b
      class (*), pointer :: values_a(:), values_b(:)

      ! check size
      shape_a = a%get_shape()
      shape_b = b%get_shape()
      equal = size(shape_a) == size(shape_b)
      if (.not. equal) return

      ! check shape
      equal = all(shape_a == shape_b)
      if (.not. equal) return

      if (allocated(a%values)) then
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
               equal = all(values_a == values_b)
            end select
         type is (real(real64))
            select type (values_b)
            type is (real(real64))
               equal = all(values_a == values_b)
            end select
         type is (logical)
            select type (values_b)
            type is (logical)
               equal = all(values_a .eqv. values_b)
            end select
         type is (character(len=*))
            select type (values_b)
            type is (character(len=*))
               equal = all(values_a == values_b)
            end select
         class default
            equal = .false.
         end select
      else ! scalar case
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
               equal = (value_a == value_b)
            end select
         type is (real(real64))
            select type (value_b)
            type is (real(real64))
               equal = (value_a == value_b)
            end select
         type is (logical)
            select type (value_b)
            type is (logical)
               equal = (value_a .eqv. value_b)
            end select
         type is (StringWrap)
            select type (value_b)
            type is (StringWrap)
               equal = (value_a%value == value_b%value)
            end select
         class default
            equal = .false.
         end select
         
      end if

   end function equal


   logical function not_equal(a, b)
      class (Attribute), intent(in) :: a
      class (Attribute), intent(in) :: b

      not_equal = .not. (a == b)
   end function not_equal

   subroutine serialize( this, buffer)
      class (Attribute), intent(in) :: this
      integer, allocatable,intent(inout) :: buffer(:)
      integer :: type_kind
      integer :: length
 
      if(allocated(buffer)) deallocate(buffer)
      if (allocated(this%values)) then
         ! check type
         select type (values=>this%values)
         type is (integer(INT32))
            type_kind = pFIO_INT32
            buffer = [serialize_intrinsic(this%shape), &
                      serialize_intrinsic(type_kind),  &
                      serialize_intrinsic(values)]
         type is (real(REAL32))
            type_kind = pFIO_REAL32
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
            stop " type is not supported"
         end select
      else
         ! check type
         select type (value => this%value)
         type is (integer(INT32))
            type_kind = pFIO_INT32
            buffer = [serialize_intrinsic(this%shape), &
                      serialize_intrinsic(type_kind),  &
                      serialize_intrinsic(value)]
         type is (real(real32))
            type_kind = pFIO_REAL32
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
            stop " type is not supported"
         end select
      endif
      length =  serialize_buffer_length(length) + size(buffer)
      buffer = [serialize_intrinsic(length),buffer]

   end subroutine serialize 

   subroutine deserialize( this, buffer)
      class (Attribute), intent(inout) :: this
      integer, intent(in) :: buffer(:)
      integer :: n,type_kind,length

      integer(KIND=INT32), allocatable :: values_int32(:)
      integer(KIND=INT32) :: value_int32

      real(KIND=REAL32), allocatable :: values_real32(:)
      real(KIND=REAL32) :: value_real32

      logical, allocatable :: values_logical(:)
      logical :: value_logical

      character(len=:), allocatable :: value_char

      n = 1
      call deserialize_intrinsic(buffer(n:),length)
      n = n + serialize_buffer_length(length)
      call deserialize_intrinsic(buffer(n:),this%shape)
      n = n + serialize_buffer_length(this%shape)
      call deserialize_intrinsic(buffer(n:),type_kind)
      n = n + serialize_buffer_length(type_kind)

      if(size(this%shape) == 0 ) then
         select case (type_kind)
         case (pFIO_INT32)
             call deserialize_intrinsic(buffer(n:),value_int32)
             call this%set(value_int32)
         case (pFIO_REAL32)
             call deserialize_intrinsic(buffer(n:),value_real32)
             call this%set(value_real32)
         case (pFIO_LOGICAL)
             call deserialize_intrinsic(buffer(n:),value_logical)
             call this%set(value_logical)
         case (pFIO_STRING)
             call deserialize_intrinsic(buffer(n:),value_char)
             call this%set(value_char)
         case default
           stop "attribute deserialize not support"
         end select
      else
         select case (type_kind)
         case (pFIO_INT32)
             call deserialize_intrinsic(buffer(n:),values_int32)
             allocate(this%values, source =values_int32)
         case (pFIO_REAL32)
             call deserialize_intrinsic(buffer(n:),values_real32)
             allocate(this%values, source =values_real32)
         case (pFIO_LOGICAL)
             call deserialize_intrinsic(buffer(n:),values_logical)
             allocate(this%values, source =values_logical)
         case default
           stop "attribute deserialize not support"
         end select
      endif

   end subroutine deserialize

end module pFIO_AttributeMod


! The following module defines an FTL map (associative array) with keys that are deferred
! length strings and values that are Attributes.

module pFIO_StringAttributeMapMod
   use pFIO_ThrowMod
   use ESMF
   use pFIO_AttributeMod
   
#include "types/key_deferredLengthString.inc"   
#define _value type (Attribute)
#define _value_equal_defined

#define _map StringAttributeMap
#define _iterator StringAttributeMapIterator

#define _alt
#define _FTL_THROW pFIO_throw_exception

#include "templates/map.inc"
   
end module pFIO_StringAttributeMapMod

module pFIO_StringAttributeMapUtilMod
   use pFIO_UtilitiesMod
   use pFIO_AttributeMod
   use pFIO_StringAttributeMapMod
   implicit none
   private

   public :: StringAttributeMap_serialize
   public :: StringAttributeMap_deserialize

contains

    subroutine StringAttributeMap_serialize(map,buffer)
       type (StringAttributeMap) ,intent(in):: map
       integer, allocatable,intent(inout) :: buffer(:)
       type (StringAttributeMapIterator) :: iter
       character(len=:),pointer :: key
       type(Attribute),pointer :: attr_ptr
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
    end subroutine StringAttributeMap_serialize

    function StringAttributeMap_deserialize(buffer) result(map)
       type (StringAttributeMap) :: map
       integer, intent(in) :: buffer(:)

       character(len=:),allocatable :: key
       integer :: length,n,n0,n1,n2
       type (Attribute), allocatable :: attr

       n = 1
       call deserialize_intrinsic(buffer(n:),length)
       n0 = serialize_buffer_length(length)
       n = n + n0
       length = length - n0

       do while (length > 0)
          call deserialize_intrinsic(buffer(n:),key)
          n1 = serialize_buffer_length(key)
          n = n + n1
          allocate(attr)
          call attr%deserialize(buffer(n:))
          call deserialize_intrinsic(buffer(n:),n2)
          n = n + n2
          length = length - n1 - n2
          call map%insert(key,attr)
          deallocate(key)
          deallocate(attr)
       enddo
    end function StringAttributeMap_deserialize

end module pFIO_StringAttributeMapUtilMod
