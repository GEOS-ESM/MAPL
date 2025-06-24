#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module pFIO_VariableMod
   use pFIO_UtilitiesMod
   use MAPL_ExceptionHandling
   use gFTL_StringVector
   use pFIO_StringVectorUtilMod
   use mapl_KeywordEnforcerMod
   use pFIO_ConstantsMod
   use pFIO_UnlimitedEntityMod
   use pFIO_AttributeMod
   use pFIO_StringAttributeMapMod
   use pFIO_StringAttributeMapUtilMod
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64, INT32, INT64
   implicit none
   private

   public :: Variable
   public :: Variable_SERIALIZE_TYPE
   public :: Variable_deserialize

   integer, parameter :: Variable_SERIALIZE_TYPE = 100

   type :: Variable
      private
      integer :: type = -1
      type (StringVector) :: dimensions
      type (StringAttributeMap) :: attributes
      type (UnlimitedEntity) :: const_value
      integer :: deflation = 0 ! default no compression
      integer :: quantize_algorithm = 0 ! default no quantization
      integer :: quantize_level = 0 ! default no quantize_level
      integer :: zstandard_level = 0 ! default no zstandard
      integer, allocatable :: chunksizes(:)
   contains
      procedure :: get_type
      procedure :: get_ith_dimension
      procedure :: get_dimensions
      procedure :: get_attributes
      procedure :: get_const_value

      procedure :: get_attribute
      procedure :: get_attribute_string
      procedure :: get_attribute_int32
      procedure :: get_attribute_int64
      procedure :: get_attribute_real32
      procedure :: get_attribute_real64
      generic :: add_attribute => add_attribute_0d
      generic :: add_attribute => add_attribute_1d
      procedure :: add_attribute_0d
      procedure :: add_attribute_1d
      procedure :: remove_attribute
      procedure :: add_const_value

      procedure :: get_chunksizes
      procedure :: get_deflation
      procedure :: set_deflation
      procedure :: get_quantize_algorithm
      procedure :: get_quantize_level
      procedure :: get_zstandard_level
      procedure :: is_attribute_present
      generic :: operator(==) => equal
      generic :: operator(/=) => not_equal
      procedure :: equal
      procedure :: not_equal

      procedure :: serialize

   end type Variable


   interface Variable
      module procedure new_Variable
   end interface Variable

contains


   function new_Variable(unusable, type, dimensions, chunksizes,const_value, deflation, quantize_algorithm, quantize_level, zstandard_level, rc) result(var)
      type (Variable) :: var
      integer, optional, intent(in) :: type
      class (KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), optional, intent(in) :: dimensions
      integer, optional, intent(in) :: chunksizes(:)
      type (UnlimitedEntity), optional, intent(in) :: const_value
      integer, optional, intent(in) :: deflation
      integer, optional, intent(in) :: quantize_algorithm
      integer, optional, intent(in) :: quantize_level
      integer, optional, intent(in) :: zstandard_level
      integer, optional, intent(out) :: rc

      integer:: empty(0)

      var%type = -1
      var%deflation = 0
      var%quantize_algorithm = 0
      var%quantize_level = 0
      var%zstandard_level = 0
      var%chunksizes = empty
      var%dimensions = StringVector()
      var%attributes = StringAttributeMap()
      var%const_value = UnlimitedEntity()

      if (present(type)) then
         var%type = type
      endif

      if (present(dimensions)) then
         call parse_dimensions()
      end if

      if (present(chunksizes)) then
         var%chunksizes = chunksizes
      end if

      if (present(const_value)) then
         var%const_value = const_value
      endif

      if (present(deflation)) then
         var%deflation = deflation
      endif

      if (present(quantize_algorithm)) then
         var%quantize_algorithm = quantize_algorithm
      endif

      if (present(quantize_level)) then
         var%quantize_level = quantize_level
      endif

      if (present(zstandard_level)) then
         var%zstandard_level = zstandard_level
      endif

      __RETURN(__SUCCESS)
      __UNUSED_DUMMY(unusable)
   contains

      subroutine parse_dimensions()
         character(len=:), allocatable :: dim_string
         integer :: idx

         dim_string = dimensions // pFIO_DIMENSION_SEPARATOR
         do
            idx = index(dim_string, pFIO_DIMENSION_SEPARATOR)
            if (idx == 0) exit
            if (idx == 1) exit
            call var%dimensions%push_back(dim_string(1:idx-1))
            dim_string = dim_string(idx+1:) ! do not forget to skip separator !
         end do
      end subroutine parse_dimensions


   end function new_Variable


   integer function get_type(this) result(type)
      class (Variable), intent(in) :: this

      type = this%type
   end function get_type


   function get_ith_dimension(this, i, unusable, rc) result(dimension_name)
      character(len=:), pointer :: dimension_name
      class (Variable), target, intent(in) :: this
      integer, intent(in) :: i
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      if (i <= 0 .or. i > this%dimensions%size()) then
         dimension_name => null()
         __RETURN(pFIO_ILLEGAL_DIMENSION_INDEX)
      else
         dimension_name => this%dimensions%at(i)
         __RETURN(__SUCCESS)
      end if
      __UNUSED_DUMMY(unusable)

   end function get_ith_dimension

   function get_dimensions(this) result(dimensions)
      class (Variable), target, intent(in) :: this
      type (StringVector), pointer :: dimensions

      dimensions => this%dimensions

   end function get_dimensions


   function get_attributes(this) result(attributes)
      class (Variable), target, intent(in) :: this
      type (StringAttributeMap), pointer :: attributes

      attributes => this%attributes

   end function get_attributes

   subroutine remove_attribute(this,attr_name,rc)
      class (Variable), target, intent(inout) :: this
      character(len=*), intent(in) :: attr_name
      integer, optional, intent(out) :: rc
      type(StringAttributeMapIterator) :: iter
      integer :: status

      iter = this%attributes%find(attr_name)
      call this%attributes%erase(iter)
      __RETURN(__SUCCESS)
   end subroutine

   subroutine add_attribute_0d(this, attr_name, attr_value, rc)
      class (Variable), target, intent(inout) :: this
      character(len=*), intent(in) :: attr_name
      class (*), intent(in) :: attr_value
      integer, optional, intent(out) :: rc

      type (Attribute) :: attr

      select type (q => attr_value)
      class is (Attribute)
         call this%attributes%insert(attr_name, q)
      class default
         call attr%set(q)
         call this%attributes%insert(attr_name, attr)
      end select

      __RETURN(__SUCCESS)
   end subroutine add_attribute_0d

   subroutine add_attribute_1d(this, attr_name, attr_values, rc)
      class (Variable), target, intent(inout) :: this
      character(len=*), intent(in) :: attr_name
      class (*), intent(in) :: attr_values(:)
      integer, optional, intent(out) :: rc

      call this%attributes%insert(attr_name, Attribute(attr_values))
      __RETURN(__SUCCESS)
   end subroutine add_attribute_1d


   function is_attribute_present(this, attr_name, rc) result(isPresent)
      type (Attribute), pointer :: attr
      class (Variable), target, intent(in) :: this
      character(len=*), intent(in) :: attr_name
      integer, optional, intent(out) :: rc
      logical :: isPresent

      attr => this%attributes%at(attr_name)
      isPresent = associated(attr)
      __RETURN(__SUCCESS)

   end function is_attribute_present

   function get_attribute(this, attr_name, rc) result(attr)
      type (Attribute), pointer :: attr
      class (Variable), target, intent(in) :: this
      character(len=*), intent(in) :: attr_name
      integer, optional, intent(out) :: rc

      attr => this%attributes%at(attr_name)
      __ASSERT(associated(attr), "no such attribute : " // trim(attr_name))
      __RETURN(__SUCCESS)
   end function get_attribute

   function get_attribute_string(this, attr_name, rc) result(attr_string)
      character(len=:), allocatable :: attr_string
      class (Variable), target, intent(in) :: this
      character(len=*), intent(in) :: attr_name
      integer, optional, intent(out) :: rc

      integer :: status
      type(Attribute), pointer :: attr
      class(*), pointer :: attr_val

      attr => this%get_attribute(attr_name,__RC)
      __ASSERT(associated(attr),"no such attribute "//attr_name)
      attr_val => attr%get_value()
      select type(attr_val)
      type is(character(*))
         attr_string = attr_val
      class default
         __FAIL('unsupported subclass (not string) of attribute named '//attr_name)
      end select

      __RETURN(__SUCCESS)
   end function get_attribute_string

   function get_attribute_real32(this,attr_name,rc) result(attr_real32)
      real(REAL32) :: attr_real32
      class(Variable), intent(inout) :: this
      character(len=*), intent(in) :: attr_name
      integer, optional, intent(out) :: rc

      real(REAL32) :: tmp(1)
      real(REAL64) :: tmpd(1)
      integer :: status
      type(Attribute), pointer :: attr
      class(*), pointer :: attr_val(:)

      attr => this%get_attribute(attr_name,__RC)
      __ASSERT(associated(attr),"no attribute named "//attr_name)
      attr_val => attr%get_values()
      select type(attr_val)
      type is(real(kind=REAL32))
         tmp = attr_val
         attr_real32 = tmp(1)
      type is(real(kind=REAL64))
         tmpd = attr_val
         attr_real32 = REAL(tmpd(1))
      class default
         __FAIL('unsupported subclass (not real32) for units of attribute named '//attr_name)
      end select

      __RETURN(__SUCCESS)
   end function get_attribute_real32

   function get_attribute_real64(this,attr_name,rc) result(attr_real64)
      real(REAL64) :: attr_real64
      class(Variable), intent(inout) :: this
      character(len=*), intent(in) :: attr_name
      integer, optional, intent(out) :: rc

      real(REAL64) :: tmp(1)
      integer :: status
      type(Attribute), pointer :: attr
      class(*), pointer :: attr_val(:)

      attr => this%get_attribute(attr_name,__RC)
      __ASSERT(associated(attr),"no such attribute "//attr_name)
      attr_val => attr%get_values()
      select type(attr_val)
      type is(real(kind=REAL64))
         tmp = attr_val
         attr_real64 = tmp(1)
      class default
         __FAIL('unsupported subclass (not real64) for units of attribute named '//attr_name)
      end select

      __RETURN(__SUCCESS)
   end function get_attribute_real64

   function get_attribute_int32(this,attr_name,rc) result(attr_int32)
      integer(INT32) :: attr_int32
      class(Variable), intent(inout) :: this
      character(len=*), intent(in) :: attr_name
      integer, optional, intent(out) :: rc

      integer(INT32) :: tmp(1)
      integer :: status
      type(Attribute), pointer :: attr
      class(*), pointer :: attr_val(:)

      attr => this%get_attribute(attr_name,__RC)
      __ASSERT(associated(attr),"no attribute named "//attr_name)
      attr_val => attr%get_values()
      select type(attr_val)
      type is(integer(kind=INT32))
         tmp = attr_val
         attr_int32 = tmp(1)
      class default
         __FAIL('unsupported subclass (not int32) for units of attribute named '//attr_name)
      end select

      __RETURN(__SUCCESS)
   end function get_attribute_int32

   function get_attribute_int64(this,attr_name,rc) result(attr_int64)
      integer(INT64) :: attr_int64
      class(Variable), intent(inout) :: this
      character(len=*), intent(in) :: attr_name
      integer, optional, intent(out) :: rc

      integer(INT64) :: tmp(1)
      integer :: status
      type(Attribute), pointer :: attr
      class(*), pointer :: attr_val(:)

      attr => this%get_attribute(attr_name,__RC)
      __ASSERT(associated(attr),"no attribute named "//attr_name)
      attr_val => attr%get_values()
      select type(attr_val)
      type is(integer(kind=INT64))
         tmp = attr_val
         attr_int64 = tmp(1)
      class default
         __FAIL('unsupported subclass (not int64) for units of attribute named '//attr_name)
      end select

      __RETURN(__SUCCESS)
   end function get_attribute_int64

   subroutine add_const_value(this, const_value, rc)
      class (Variable), target, intent(inout) :: this
      type (UnlimitedEntity), intent(in) :: const_value
      integer, optional, intent(out) :: rc
      integer :: rank, dims
      !integer,allocatable :: shp(:), dims(:)

      rank = const_value%get_rank()
      dims = this%dimensions%size()

      __ASSERT( dims == rank, "dimensions and rank don't match.  Add dimension first")

      this%const_value = const_value
      __RETURN(__SUCCESS)
   end subroutine add_const_value

   function get_const_value(this) result(const_value)
      class (Variable), target, intent(in) :: this
      type (UnlimitedEntity), pointer :: const_value

      const_value =>this%const_value

   end function get_const_value

   function get_chunksizes(this) result(chunksizes)
      class (Variable), target, intent(in) :: this
      integer, pointer :: chunksizes(:)

      if (allocated(this%chunksizes)) then
         chunksizes => this%chunksizes
      else
         nullify(chunksizes)
      end if

   end function get_chunksizes

   function get_deflation(this) result(deflateLevel)
      class (Variable), target, intent(In) :: this
      integer :: deflateLevel

      deflateLevel=this%deflation
   end function get_deflation

   subroutine set_deflation(this,deflate_level)
      class (Variable), target, intent(inout) :: this
      integer, intent(in) :: deflate_level
      this%deflation = deflate_level
   end subroutine

   function get_quantize_algorithm(this) result(quantizeAlgorithm)
      class (Variable), target, intent(In) :: this
      integer :: quantizeAlgorithm

      quantizeAlgorithm=this%quantize_algorithm
   end function get_quantize_algorithm

   function get_quantize_level(this) result(quantizeLevel)
      class (Variable), target, intent(In) :: this
      integer :: quantizeLevel

      quantizeLevel=this%quantize_level
   end function get_quantize_level

   function get_zstandard_level(this) result(zstandardLevel)
      class (Variable), target, intent(In) :: this
      integer :: zstandardLevel

      zstandardLevel=this%zstandard_level
   end function get_zstandard_level

   logical function equal(a, b)
      class (Variable), target, intent(in) :: a
      type (Variable), target, intent(in) :: b

      type (StringAttributeMapIterator) :: iter
      type (Attribute), pointer :: attr_a, attr_b
      character(len=:), pointer :: attr_name

      ! special case : both are empty
      equal = (a%const_value == b%const_value)
      if (.not. equal) return

      equal = ( a%dimensions%size() == 0 .and. &
                b%dimensions%size() == 0 .and. &
                a%attributes%size() == 0 .and. &
                b%attributes%size() == 0 )

      if (equal) return

      equal = (a%type == b%type)
      if (.not. equal) return

      equal = (a%dimensions == b%dimensions)
      if (.not. equal) return

      equal = (a%attributes%size() == b%attributes%size())
      if (.not. equal) return

      iter = a%attributes%begin()
      do while (iter /= a%attributes%end())

         attr_name => iter%key()
         attr_b => b%attributes%at(attr_name)
         equal = (associated(attr_b))
         if (.not. equal) return

         attr_a => iter%value()

         equal = (attr_a == attr_b)
         if (.not. equal) return

         call iter%next()
      end do


   end function equal

   logical function not_equal(a, b)
      class (Variable), intent(in) :: a
      type (Variable), intent(in) :: b

      not_equal = .not. (a == b)
   end function not_equal

   subroutine serialize(this, buffer, rc)
      class (Variable), intent(in) :: this
      integer, allocatable, intent(inout) :: buffer(:)
      integer, optional, intent(out) :: rc
      integer, allocatable :: tmp_buffer(:)
      integer :: length
      integer :: status

      if(allocated(buffer)) deallocate(buffer)

      call StringVector_serialize(this%dimensions, tmp_buffer)
      buffer =[serialize_intrinsic(this%type), tmp_buffer]
      call StringAttributeMap_serialize(this%attributes, tmp_buffer, status)
      __VERIFY(status)
      buffer = [buffer, tmp_buffer]
      call this%const_value%serialize(tmp_buffer, status)
      __VERIFY(status)
      buffer = [buffer, tmp_buffer]
      buffer = [buffer, serialize_intrinsic(this%deflation)]
      buffer = [buffer, serialize_intrinsic(this%quantize_algorithm)]
      buffer = [buffer, serialize_intrinsic(this%quantize_level)]
      buffer = [buffer, serialize_intrinsic(this%zstandard_level)]

      if( .not. allocated(this%chunksizes)) then
        buffer =[buffer,[1]]
      else
        buffer =[buffer,serialize_intrinsic(this%chunksizes)]
      endif

      length = serialize_buffer_length(length) + serialize_buffer_length(Variable_SERIALIZE_TYPE) + size(buffer)
      buffer = [serialize_intrinsic(length), serialize_intrinsic(Variable_SERIALIZE_TYPE), buffer]
      __RETURN(__SUCCESS)
   end subroutine

   subroutine Variable_deserialize(buffer, var, rc)
      integer, intent(in) :: buffer(:)
      type (Variable), intent(inout) :: var
      integer, optional, intent(out) :: rc
      integer :: status
      var = Variable()
      call deserialize(var, buffer, rc=status)
      __VERIFY(status)
      __RETURN(__SUCCESS)
   contains
      subroutine deserialize(this, buffer, rc)
         class (Variable), intent(inout) :: this
         integer,intent(in) :: buffer(:)
         integer, optional, intent(out) :: rc
         integer :: n,length, v_type
         integer :: status

         n = 1
         call deserialize_intrinsic(buffer(n:),length)
         __ASSERT(length == size(buffer), "length does not match")

         length = serialize_buffer_length(length)
         n = n+length
         call deserialize_intrinsic(buffer(n:),v_type)
         length = serialize_buffer_length(v_type)
         n = n+length
         call deserialize_intrinsic(buffer(n:),this%type)
         length = serialize_buffer_length(this%type)
         n = n+length
         call StringVector_deserialize(buffer(n:), this%dimensions, status)
         __VERIFY(status)
         call deserialize_intrinsic(buffer(n:),length)
         n = n + length
         call deserialize_intrinsic(buffer(n:),length)
         call StringAttributeMap_deserialize(buffer(n:n+length-1),this%attributes, status)
         __VERIFY(status)

         n = n + length

         call deserialize_intrinsic(buffer(n:),length)
         call UnlimitedEntity_deserialize(buffer(n:(n+length-1)), this%const_value, status)
         __VERIFY(status)

         n = n + length
         call deserialize_intrinsic(buffer(n:),this%deflation)
         length = serialize_buffer_length(this%deflation)
         n = n + length
         call deserialize_intrinsic(buffer(n:),this%quantize_algorithm)
         length = serialize_buffer_length(this%quantize_algorithm)
         n = n + length
         call deserialize_intrinsic(buffer(n:),this%quantize_level)
         length = serialize_buffer_length(this%quantize_level)
         n = n + length
         call deserialize_intrinsic(buffer(n:),this%zstandard_level)
         length = serialize_buffer_length(this%zstandard_level)
         n = n + length
         call deserialize_intrinsic(buffer(n:),this%chunksizes)
         __RETURN(__SUCCESS)
      end subroutine deserialize
  end subroutine Variable_deserialize

end module pFIO_VariableMod
