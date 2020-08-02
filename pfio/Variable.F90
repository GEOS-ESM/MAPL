#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module pFIO_VariableMod
   use pFIO_UtilitiesMod
   use MAPL_ExceptionHandling
   use gFTL_StringVector
   use pFIO_StringVectorUtilMod
   use pFIO_KeywordEnforcerMod
   use pFIO_ConstantsMod
   use pFIO_UnlimitedEntityMod
   use pFIO_AttributeMod
   use pFIO_StringAttributeMapMod
   use pFIO_StringAttributeMapUtilMod
   use, intrinsic :: iso_fortran_env, only: INT32, INT64
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64
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
      integer, allocatable :: chunksizes(:)
   contains
      procedure :: get_type
      procedure :: get_ith_dimension
      procedure :: get_dimensions
      procedure :: get_attributes
      procedure :: get_const_value

      procedure :: get_attribute
      generic :: add_attribute => add_attribute_0d
      generic :: add_attribute => add_attribute_1d
      procedure :: add_attribute_0d
      procedure :: add_attribute_1d
      procedure :: add_const_value

      procedure :: get_chunksizes
      procedure :: get_deflation
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


   function new_Variable(unusable, type, dimensions, chunksizes,const_value, deflation, rc) result(var)
      type (Variable) :: var
      integer, optional, intent(in) :: type
      class (KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), optional, intent(in) :: dimensions
      integer, optional, intent(in) :: chunksizes(:)
      type (UnlimitedEntity), optional, intent(in) :: const_value
      integer, optional, intent(in) :: deflation
      integer, optional, intent(out) :: rc

      integer:: empty(0)

      var%type = -1
      var%deflation = 0
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
 
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
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
         _RETURN(pFIO_ILLEGAL_DIMENSION_INDEX)
      else
         dimension_name => this%dimensions%at(i)
         _RETURN(_SUCCESS)
      end if
      _UNUSED_DUMMY(unusable)

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
         
      _RETURN(_SUCCESS)
   end subroutine add_attribute_0d

   subroutine add_attribute_1d(this, attr_name, attr_values, rc)
      class (Variable), target, intent(inout) :: this
      character(len=*), intent(in) :: attr_name
      class (*), intent(in) :: attr_values(:)
      integer, optional, intent(out) :: rc

      call this%attributes%insert(attr_name, Attribute(attr_values))
      _RETURN(_SUCCESS)
   end subroutine add_attribute_1d


   function is_attribute_present(this, attr_name, rc) result(isPresent)
      type (Attribute), pointer :: attr
      class (Variable), target, intent(in) :: this
      character(len=*), intent(in) :: attr_name
      integer, optional, intent(out) :: rc
      logical :: isPresent

      attr => this%attributes%at(attr_name)
      isPresent = associated(attr)
      _RETURN(_SUCCESS)

   end function is_attribute_present

   function get_attribute(this, attr_name, rc) result(attr)
      type (Attribute), pointer :: attr
      class (Variable), target, intent(in) :: this
      character(len=*), intent(in) :: attr_name
      integer, optional, intent(out) :: rc

      attr => this%attributes%at(attr_name)
      _ASSERT(associated(attr), "no such attribute : " // trim(attr_name))
      _RETURN(_SUCCESS)
   end function get_attribute

   subroutine add_const_value(this, const_value, rc)
      class (Variable), target, intent(inout) :: this
      type (UnlimitedEntity), intent(in) :: const_value
      integer, optional, intent(out) :: rc
      integer :: rank, dims
      !integer,allocatable :: shp(:), dims(:)

      rank = const_value%get_rank()
      dims = this%dimensions%size()

      _ASSERT( dims == rank, "dimensions and rank don't match.  Add dimension first")

      this%const_value = const_value
      _RETURN(_SUCCESS)
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
      _VERIFY(status)
      buffer = [buffer, tmp_buffer] 
      call this%const_value%serialize(tmp_buffer, status)
      _VERIFY(status)
      buffer = [buffer, tmp_buffer,serialize_intrinsic(this%deflation)] 

      if( .not. allocated(this%chunksizes)) then
        buffer =[buffer,[1]]
      else
        buffer =[buffer,serialize_intrinsic(this%chunksizes)]
      endif

      length = serialize_buffer_length(length) + serialize_buffer_length(Variable_SERIALIZE_TYPE) + size(buffer)
      buffer = [serialize_intrinsic(length), serialize_intrinsic(Variable_SERIALIZE_TYPE), buffer]
      _RETURN(_SUCCESS) 
   end subroutine

   subroutine Variable_deserialize(buffer, var, rc)
      integer, intent(in) :: buffer(:)
      type (Variable), intent(inout) :: var
      integer, optional, intent(out) :: rc
      integer :: status
      var = Variable()
      call deserialize(var, buffer, rc=status)
      _VERIFY(status)
      _RETURN(_SUCCESS)
   contains
      subroutine deserialize(this, buffer, rc)
         class (Variable), intent(inout) :: this
         integer,intent(in) :: buffer(:)
         integer, optional, intent(out) :: rc
         integer :: n,length, v_type
         type (UnlimitedEntity) :: const
         integer :: status
 
         n = 1
         call deserialize_intrinsic(buffer(n:),length)
         _ASSERT(length == size(buffer), "length does not match")

         length = serialize_buffer_length(length)
         n = n+length
         call deserialize_intrinsic(buffer(n:),v_type)
         length = serialize_buffer_length(v_type)
         n = n+length
         call deserialize_intrinsic(buffer(n:),this%type)
         length = serialize_buffer_length(this%type)
         n = n+length
         call StringVector_deserialize(buffer(n:), this%dimensions, status)
         _VERIFY(status)
         call deserialize_intrinsic(buffer(n:),length)
         n = n + length
         call deserialize_intrinsic(buffer(n:),length)
         call StringAttributeMap_deserialize(buffer(n:n+length-1),this%attributes, status)
         _VERIFY(status)

         n = n + length
         !allocate(const)
         call deserialize_intrinsic(buffer(n:),length)
         call UnlimitedEntity_deserialize(buffer(n:(n+length-1)), this%const_value, status)
         _VERIFY(status)
         !this%const_value = const
         n = n + length
         call deserialize_intrinsic(buffer(n:),this%deflation)
         length = serialize_buffer_length(this%deflation)
         n = n + length
         call deserialize_intrinsic(buffer(n:),this%chunksizes)
         _RETURN(_SUCCESS)
      end subroutine deserialize
  end subroutine Variable_deserialize

end module pFIO_VariableMod
