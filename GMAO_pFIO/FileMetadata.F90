#define _SUCCESS      0
#define _FAILURE     1
#define _VERIFY(A)   if(  A/=0) then; call pFIO_throw_exception(__FILE__,__LINE__); return; endif
#define _ASSERT(A)   if(.not.A) then; if(present(rc)) rc=_FAILURE; PRINT *, Iam, __LINE__; return; endif
#define _RETURN(A)   if(present(rc)) rc=A; return
#include "unused_dummy.H"

module pFIO_FileMetaDataMod
   use pFIO_KeywordEnforcerMod
   use pFIO_StringIntegerMapMod
   use pFIO_StringIntegerMapUtilMod
   use pFIO_ThrowMod
   use pFIO_ConstantsMod
   use pFIO_UtilitiesMod
   use pFIO_AttributeMod
   use pFIO_VariableMod
   use pFIO_StringVariableMapMod
   use pFIO_StringVariableMapUtilMod
   use pFIO_StringAttributeMapMod
   use pFIO_StringVectorMod
   implicit none
   private

   public :: FileMetadata 

   type :: FileMetadata
      private
      type (StringIntegerMap) :: dimensions
      type (Variable) :: global
      type (StringVariableMap) :: variables
   contains

      procedure :: get_dimensions
      procedure :: add_dimension
      procedure :: get_dimension

      procedure :: get_attributes
      generic :: add_attribute => add_attribute_0d, add_attribute_1d
      procedure :: add_attribute_0d
      procedure :: add_attribute_1d
      procedure :: get_attribute

      procedure :: get_variable
      procedure :: add_variable
      procedure :: get_variables

      generic :: operator(==) => equal
      generic :: operator(/=) => not_equal
      procedure :: equal
      procedure :: not_equal

      procedure :: serialize
      procedure :: deserialize

   end type FileMetadata


contains


   function get_dimensions(this) result(dimensions)
      type (StringIntegerMap), pointer :: dimensions
      class (FileMetadata), target, intent(in) :: this

      dimensions => this%dimensions

   end function get_dimensions


   subroutine add_dimension(this, dim_name, extent, unusable, rc)
      class (FileMetadata), target, intent(inout) :: this
      character(len=*), intent(in) :: dim_name
      integer, intent(in) :: extent

      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(rc)

      call this%dimensions%insert(dim_name, extent)
      
   end subroutine add_dimension


   integer function get_dimension(this, dim_name, unusable, rc) result(extent)
      class (FileMetadata), target, intent(in) :: this
      character(len=*), intent(in) :: dim_name
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type (StringIntegerMapIterator) :: iter
      integer, pointer :: ptr

      _UNUSED_DUMMY(unusable)

      iter = this%dimensions%find(dim_name)
      ptr => iter%value()

      if (associated(ptr)) then
         extent = this%dimensions%at(dim_name)
         if (present(rc)) then
            rc = pFIO_SUCCESS
         end if
      else
         extent = 0
         if (present(rc)) then
            rc = pFIO_DIMENSION_NOT_FOUND
         else
            call pFIO_throw_exception(__FILE__,__LINE__, &
                 & message='FileMetadata::get_dimension() - no such dimension <'//dim_name//'>.')
         end if
      end if
      
   end function get_dimension


   subroutine add_attribute_0d(this, attr_name, attr_value, unusable, rc)
      class (FileMetadata), target, intent(inout) :: this
      character(len=*), intent(in) :: attr_name
      class (*), intent(in) :: attr_value
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(rc)

      call this%global%add_attribute(attr_name, attr_value)

   end subroutine add_attribute_0d

   subroutine add_attribute_1d(this, attr_name, values, unusable, rc)
      class (FileMetadata), target, intent(inout) :: this
      character(len=*), intent(in) :: attr_name
      class (*), intent(in) :: values(:)

      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(rc)

      call this%global%add_attribute(attr_name, values)
      
   end subroutine add_attribute_1d


   function get_attribute(this, attr_name, unusable, rc) result(ref)
      type (Attribute), pointer :: ref
      class (FileMetadata), target, intent(in) :: this
      character(len=*), intent(in) :: attr_name
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(unusable)

      ref => this%global%get_attribute(attr_name)
      if (associated(ref)) then
         if (present(rc)) then
            rc = pFIO_SUCCESS
         end if
      else
         call pFIO_throw_exception(__FILE__,__LINE__, &
              & message='FileMetadata::get_attribute() - no such attribute <'//attr_name//'>.')
         if (present(rc)) then
            rc = pFIO_ATTRIBUTE_NOT_FOUND
         end if
      end if
      
   end function get_attribute


   function get_attributes(this) result(attributes)
      type (StringAttributeMap), pointer :: attributes
      class (FileMetadata), target, intent(in) :: this

      attributes => this%global%get_attributes()

   end function get_attributes


   function get_variable(this, var_name, unusable, rc) result(var)
      type (Variable), pointer :: var
      class (FileMetadata), target, intent(in) :: this
      character(len=*), intent(in) :: var_name
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(rc)

      var => this%variables%at(var_name)
      
   end function get_variable


   function get_variables(this) result(variables)
      type (StringVariableMap), pointer :: variables
      class (FileMetadata), target, intent(in) :: this

      variables => this%variables

   end function get_variables


   subroutine add_variable(this, var_name, var, unusable, rc)
      class (FileMetadata), target, intent(inout) :: this
      character(len=*), intent(in) :: var_name
      class (Variable), intent(in) :: var
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type (StringVector), pointer :: dims
      type (StringVectorIterator) :: iter
      integer, pointer :: dim_this
      character(len=:), pointer :: dim_name

      _UNUSED_DUMMY(unusable)

      ! ensure all of var's dimensions are defined
      dims => var%get_dimensions()
      iter = dims%begin()
      do while (iter /= dims%end())

         dim_name => iter%get()
         dim_this => this%dimensions%at(dim_name)
         if (.not. associated(dim_this)) then
            if (present(rc)) then
               rc = pFIO_UNDEFINED_DIMENSION
            end if
            call pFIO_throw_exception(__FILE__,__LINE__, &
                 & "FileMetadata::add_variable() - undefined dimension '" // dim_name // "'.")
            return
         end if

         call iter%next()
      end do

      call this%variables%insert(var_name, var)

      _RETURN(_SUCCESS)
      
   end subroutine add_variable


   subroutine add_var_attribute_0d(this, var_name, attr_name, value, unusable, rc)
      class (FileMetadata), target, intent(inout) :: this
      character(len=*), intent(in) :: var_name
      character(len=*), intent(in) :: attr_name
      class (*), intent(in) :: value
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      class (Variable), pointer :: var

      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(rc)

      var => this%get_variable(var_name)
      call var%add_attribute(attr_name, value)

      _RETURN(_SUCCESS)
      
   end subroutine add_var_attribute_0d

   subroutine add_var_attribute_1d(this, var_name, attr_name, values, unusable, rc)
      class (FileMetadata), target, intent(inout) :: this
      character(len=*), intent(in) :: var_name
      character(len=*), intent(in) :: attr_name
      class (*), intent(in) :: values(:)

      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      class (Variable), pointer :: var

      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(rc)

      var => this%get_variable(var_name)
      call var%add_attribute(attr_name, values)
      
   end subroutine add_var_attribute_1d


   logical function equal(a, b)
      class (FileMetadata), target, intent(in) :: a
      class (FileMetadata), target, intent(in) :: b

      equal = same_dimensions(a,b)
      if (.not. equal) return

      equal = same_attributes(a,b)
      if (.not. equal) return

      equal = same_variables(a,b)

   contains

      logical function same_dimensions(a, b) result(equal)
         class (FileMetadata), target, intent(in) :: a
         class (FileMetadata), target, intent(in) :: b
         type (StringIntegerMapIterator) :: iter
         integer, pointer :: dim_a, dim_b
         character(len=:), pointer :: dim_name

         equal = a%dimensions%size() == b%dimensions%size()
         if (.not. equal) return
         
         iter = a%dimensions%begin()
         do while (iter /= a%dimensions%end())

            dim_name => iter%key()
            dim_b => b%dimensions%at(dim_name)

            equal = (associated(dim_b))
            if (.not. equal) return

            dim_a => iter%value()
            equal = (dim_a == dim_b)
            if (.not. equal) return

            call iter%next()
         end do
      end function same_dimensions

      logical function same_attributes(a, b) result(equal)
         class (FileMetadata), target, intent(in) :: a
         class (FileMetadata), target, intent(in) :: b

         type (StringAttributeMapIterator) :: iter
         type (Attribute), pointer :: attr_a, attr_b
         character(len=:), pointer :: attr_name

         equal = (a%global == b%global)

      end function same_attributes
      
      logical function same_variables(a, b) result(equal)
         class (FileMetadata), target, intent(in) :: a
         class (FileMetadata), target, intent(in) :: b

         type (StringVariableMapIterator) :: iter
         class (Variable), pointer :: var_a, var_b
         character(len=:), pointer :: var_name

         equal = a%variables%size() == b%variables%size()
         if (.not. equal) return

         iter = a%variables%begin()
         do while (iter /= a%variables%end())
            
            var_name => iter%key()
            var_b => b%variables%at(var_name)
            
            equal = (associated(var_b))
            if (.not. equal) return
            
            var_a => iter%value()
            equal = (var_a == var_b)
            if (.not. equal) return
            
            call iter%next()
         end do

      end function same_variables
      
      
   end function equal


   logical function not_equal(a, b)
      class (FileMetadata), target, intent(in) :: a
      class (FileMetadata), target, intent(in) :: b

      not_equal = .not. (a == b)
   end function not_equal

   subroutine serialize(this, buffer)
      class (FileMetaData), intent(in) :: this
      integer,allocatable,intent(inout) :: buffer(:)
      integer :: length 
      integer, allocatable :: tmp_buffer(:)

      if(allocated(buffer)) deallocate(buffer)
            
      call StringIntegerMap_serialize(this%dimensions, tmp_buffer)
      buffer = [tmp_buffer]
      call this%global%serialize(tmp_buffer)
      buffer = [buffer,tmp_buffer]
      call StringVariableMap_serialize(this%variables, tmp_buffer)
      buffer = [buffer,tmp_buffer]

      length = serialize_buffer_length(length) + size(buffer)
      buffer = [serialize_intrinsic(length),buffer]
   end subroutine

   subroutine deserialize(this, buffer)
      class (FileMetaData), intent(inout) :: this
      integer,intent(in) :: buffer(:)
      integer :: n,length
      
      n = 1
      length = serialize_buffer_length(length)
      n = n+length
      this%dimensions = StringIntegerMap_deserialize(buffer(n:))
      call deserialize_intrinsic(buffer(n:),length)
      n = n + length
      call this%global%deserialize(buffer(n:))
      call deserialize_intrinsic(buffer(n:),length)
      n = n + length
      this%variables = StringVariableMap_deserialize(buffer(n:))

   end subroutine deserialize

end module pFIO_FileMetaDataMod
