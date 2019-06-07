#include "unused_dummy.H"

module pFIO_VariableMod
   use pFIO_UtilitiesMod
   use pFIO_StringVectorMod
   use pFIO_StringVectorUtilMod
   use pFIO_KeywordEnforcerMod
   use pFIO_ConstantsMod
   use pFIO_ThrowMod
   use pFIO_AttributeMod
   use pFIO_StringAttributeMapMod
   use pFIO_StringAttributeMapUtilMod
   use, intrinsic :: iso_fortran_env, only: INT32, INT64
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64
   implicit none
   private

   public :: Variable

   type :: Variable
      private
      integer :: type
      type (StringVector) :: dimensions
      type (StringAttributeMap) :: attributes
      integer, allocatable :: chunksizes(:)
   contains
      procedure :: get_type
      procedure :: get_ith_dimension
      procedure :: get_dimensions
      procedure :: get_attributes

      procedure :: get_attribute
      generic :: add_attribute => add_attribute_0d
      generic :: add_attribute => add_attribute_1d
      procedure :: add_attribute_0d
      procedure :: add_attribute_1d

      procedure :: get_chunksizes

      generic :: operator(==) => equal
      generic :: operator(/=) => not_equal
      procedure :: equal
      procedure :: not_equal

      procedure :: serialize
      procedure :: deserialize

   end type Variable


   interface Variable
      module procedure new_Variable
   end interface Variable

contains


   function new_Variable(type, unusable, dimensions, chunksizes, rc) result(var)
      type (Variable) :: var
      integer, intent(in) :: type
      class (KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), optional, intent(in) :: dimensions
      integer, optional, intent(in) :: chunksizes(:)
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(unusable)

      var%type = type

      if (present(dimensions)) then
         call parse_dimensions()
      end if

      if (present(chunksizes)) then
         var%chunksizes = chunksizes
      end if
 
     if (present(rc)) then
         rc = pFIO_SUCCESS
      end if

      return

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

      _UNUSED_DUMMY(unusable)
      if (i <= 0 .or. i > this%dimensions%size()) then
         if (present(rc)) then
            rc = pFIO_ILLEGAL_DIMENSION_INDEX
         end if
         dimension_name => null()
      else
         dimension_name => this%dimensions%at(i)
         if (present(rc)) then
            rc = pFIO_SUCCESS
         end if
      end if

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


   subroutine add_attribute_0d(this, attr_name, attr_value)
      class (Variable), target, intent(inout) :: this
      character(len=*), intent(in) :: attr_name
      class (*), intent(in) :: attr_value

      type (Attribute) :: attr

      select type (q => attr_value)
      class is (Attribute)
         call this%attributes%insert(attr_name, q)
      class default
         call attr%set(q)
!$$         attr = Attribute(attr_value)
         call this%attributes%insert(attr_name, attr)
      end select
         

   end subroutine add_attribute_0d

   subroutine add_attribute_1d(this, attr_name, attr_values)
      class (Variable), target, intent(inout) :: this
      character(len=*), intent(in) :: attr_name
      class (*), intent(in) :: attr_values(:)

      call this%attributes%insert(attr_name, Attribute(attr_values))

   end subroutine add_attribute_1d


   function get_attribute(this, attr_name) result(attr)
      type (Attribute), pointer :: attr
      class (Variable), target, intent(in) :: this
      character(len=*), intent(in) :: attr_name

      attr => this%attributes%at(attr_name)

   end function get_attribute

   
   function get_chunksizes(this) result(chunksizes)
      class (Variable), target, intent(in) :: this
      integer, pointer :: chunksizes(:)

      if (allocated(this%chunksizes)) then
         chunksizes => this%chunksizes
      else
         nullify(chunksizes)
      end if

   end function get_chunksizes


   logical function equal(a, b)
      class (Variable), target, intent(in) :: a
      class (Variable), target, intent(in) :: b

      type (StringAttributeMapIterator) :: iter
      type (Attribute), pointer :: attr_a, attr_b
      character(len=:), pointer :: attr_name

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
      class (Variable), intent(in) :: b

      not_equal = .not. (a == b)
   end function not_equal

   subroutine serialize(this, buffer)
      class (Variable), intent(in) :: this
      integer, allocatable,intent(inout) :: buffer(:)
      integer, allocatable :: tmp_buffer(:)
      integer :: length

      if(allocated(buffer)) deallocate(buffer)
      
      call StringVector_serialize(this%dimensions,tmp_buffer)
      buffer =[serialize_intrinsic(this%type),tmp_buffer]
      call StringAttributeMap_serialize(this%attributes,tmp_buffer)
      buffer =[buffer,tmp_buffer,serialize_intrinsic(this%chunksizes)]
      length = serialize_buffer_length(length) + size(buffer)
      buffer = [serialize_intrinsic(length),buffer]
      
   end subroutine

   subroutine deserialize(this, buffer)
      class (Variable), intent(inout) :: this
      integer,intent(in) :: buffer(:)
      integer :: n,length

      n = 1
      length = serialize_buffer_length(length)
      n = n+length
      call deserialize_intrinsic(buffer(n:),this%type)
      length = serialize_buffer_length(this%type)
      n = n+length
      this%dimensions = StringVector_deserialize(buffer(n:))
      call deserialize_intrinsic(buffer(n:),length)
      n = n + length
      this%attributes = StringAttributeMap_deserialize(buffer(n:))
      call deserialize_intrinsic(buffer(n:),length)
      n = n + length
      call deserialize_intrinsic(buffer(n:),this%chunksizes)

   end subroutine deserialize

end module pFIO_VariableMod


module pFIO_StringVariableMapMod
   use pFIO_ThrowMod
   use ESMF
   use pFIO_VariableMod
   
   ! Create a map (associative array) between names and pFIO_Variables.
   
#include "types/key_deferredLengthString.inc"   
#define _value class (Variable)
#define _value_allocatable

#define _map StringVariableMap
#define _iterator StringVariableMapIterator

#define _alt
#define _FTL_THROW pFIO_throw_exception

#include "templates/map.inc"
   
end module pFIO_StringVariableMapMod

module pFIO_StringVariableMapUtilMod
   use pFIO_UtilitiesMod
   use pFIO_VariableMod
   use pFIO_StringVariableMapMod
   implicit none
   private
   public :: StringVariableMap_serialize
   public :: StringVariableMap_deserialize

contains

    subroutine StringVariableMap_serialize(map,buffer)
       type (StringVariableMap) ,intent(in):: map
       integer, allocatable,intent(inout) :: buffer(:)
       type (StringVariableMapIterator) :: iter
       character(len=:),pointer :: key
       type(Variable),pointer :: var_ptr
       integer :: length
       integer, allocatable :: tmp_buffer(:)

       if (allocated(buffer)) deallocate(buffer)
       allocate(buffer(0))
       iter = map%begin()
       do while (iter /= map%end())
          key => iter%key()
          buffer=[buffer,serialize_intrinsic(key)]
          var_ptr => iter%value()
          call var_ptr%serialize(tmp_buffer)
          buffer = [buffer, tmp_buffer]
          deallocate(tmp_buffer)
          call iter%next()
       enddo
       length = serialize_buffer_length(length)+size(buffer)
       buffer = [serialize_intrinsic(length),buffer]
    end subroutine StringVariableMap_serialize

    function StringVariableMap_deserialize(buffer) result(map)
       type (StringVariableMap) :: map
       integer, intent(in) :: buffer(:)

       character(len=:),allocatable :: key
       integer :: length,n,n0,n1,n2
       type (Variable), allocatable :: var

       n = 1
       call deserialize_intrinsic(buffer(n:),length)
       n0 = serialize_buffer_length(length)
       n = n + n0
       length = length - n0

       do while (length > 0)
          call deserialize_intrinsic(buffer(n:),key)
          n1 = serialize_buffer_length(key)
          n = n + n1
          allocate(var)
          call var%deserialize(buffer(n:))
          call deserialize_intrinsic(buffer(n:),n2)
          n = n + n2
          length = length - n1 - n2
          call map%insert(key,var)
          deallocate(key)
          deallocate(var)
       enddo
    end function StringVariableMap_deserialize

end module pFIO_StringVariableMapUtilMod
