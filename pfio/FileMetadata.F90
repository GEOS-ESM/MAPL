#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module pFIO_FileMetadataMod
   use mapl_KeywordEnforcerMod
   use gFTL_StringIntegerMap
   use pFIO_StringIntegerMapUtilMod
   use pFIO_ConstantsMod
   use pFIO_UtilitiesMod
   use MAPL_ExceptionHandling
   use pFIO_UnlimitedEntityMod
   use pFIO_AttributeMod
   use pFIO_VariableMod
   use pFIO_CoordinateVariableMod
   use pFIO_StringVariableMapMod
   use pFIO_StringVariableMapUtilMod
   use pFIO_StringAttributeMapMod
   use gFTL_StringVector
   use pFIO_StringVectorUtilMod
   implicit none
   private

   public :: FileMetadata
   public :: FileMetadata_deserialize

   type :: FileMetadata
      private
      type (StringIntegerMap) :: dimensions
      type (Variable) :: global_var
      type (StringVariableMap) :: variables
      type (StringVector) :: order
      character(len=:), allocatable :: source_file
   contains

      procedure :: get_dimensions
      procedure :: get_global_var
      procedure :: add_dimension
      procedure :: get_dimension
      procedure :: modify_dimension

      procedure :: get_attributes
      generic :: add_attribute => add_attribute_0d, add_attribute_1d
      procedure :: add_attribute_0d
      procedure :: add_attribute_1d
      procedure :: get_attribute
      procedure :: has_attribute
      procedure :: remove_attribute

      procedure :: get_variable
      procedure :: get_coordinate_variable
      procedure :: add_variable
      procedure :: get_variables
      procedure :: remove_variable
      procedure :: get_order
      procedure :: set_order
      procedure :: modify_variable
      procedure :: has_dimension
      procedure :: has_variable
      procedure :: merge

      generic :: operator(==) => equal
      generic :: operator(/=) => not_equal
      procedure :: equal
      procedure :: not_equal

      procedure :: serialize
      procedure :: is_coordinate_variable
      procedure :: get_source_file
      procedure :: set_source_file

      procedure :: write_formatted
      generic :: write(formatted) => write_formatted

   end type FileMetadata

   interface FileMetadata
      module procedure new_FileMetadata
   end interface

contains

   function new_FileMetadata(unusable, dimensions, global, variables, order) result (fmd)
     type (FileMetadata) :: fmd
     class (KeywordEnforcer), optional, intent(in) :: unusable
     type (StringIntegerMap), optional, intent(in) :: dimensions
     type (Variable), optional, intent(in) :: global
     type (StringVariableMap), optional, intent(in) :: variables
     type (StringVector), optional, intent(in) :: order



     fmd%dimensions = StringIntegerMap()
     if (present(dimensions)) fmd%dimensions = dimensions

     fmd%global_var = Variable()
     if (present(global)) fmd%global_var = global

     fmd%variables = StringVariableMap()
     if (present(variables)) fmd%variables = variables

     fmd%order = StringVector()
     if (present(order)) fmd%order = order

     __UNUSED_DUMMY(unusable)
  end function

   function get_dimensions(this) result(dimensions)
      type (StringIntegerMap), pointer :: dimensions
      class (FileMetadata), target, intent(in) :: this

      dimensions => this%dimensions

   end function get_dimensions

   function get_global_var(this) result(global_var)
      type (Variable), pointer :: global_var
      class (FileMetadata), target, intent(in) :: this

      global_var => this%global_var

   end function get_global_var


   subroutine add_dimension(this, dim_name, extent, unusable, rc)
      class (FileMetadata), target, intent(inout) :: this
      character(len=*), intent(in) :: dim_name
      integer, intent(in) :: extent

      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      call this%dimensions%insert(dim_name, extent)
      __RETURN(__SUCCESS)
      __UNUSED_DUMMY(unusable)
   end subroutine add_dimension

   subroutine modify_dimension(this, dim_name, extent, unusable, rc)
      class (FileMetadata), target, intent(inout) :: this
      character(len=*), intent(in) :: dim_name
      integer, intent(in) :: extent

      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      call this%dimensions%set(dim_name, extent)
      __RETURN(__SUCCESS)
      __UNUSED_DUMMY(unusable)

   end subroutine modify_dimension

   function has_dimension(this, dim_name, unusable, rc) result(isPresent)
      class (FileMetadata), target, intent(in) :: this
      character(len=*), intent(in) :: dim_name
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type (StringIntegerMapIterator) :: iter
      logical :: isPresent

      iter = this%dimensions%find(dim_name)
      isPresent = (iter /=this%dimensions%end())
      __RETURN(__SUCCESS)
      __UNUSED_DUMMY(unusable)

   end function has_dimension

   integer function get_dimension(this, dim_name, unusable, rc) result(extent)
      class (FileMetadata), target, intent(in) :: this
      character(len=*), intent(in) :: dim_name
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type (StringIntegerMapIterator) :: iter


      iter = this%dimensions%find(dim_name)

      if (iter /= this%dimensions%end()) then
         extent = this%dimensions%at(dim_name)
         __RETURN(__SUCCESS)
      else
         extent = 0
         if (present(rc)) rc=pFIO_DIMENSION_NOT_FOUND
      end if

      __UNUSED_DUMMY(unusable)
   end function get_dimension


   subroutine add_attribute_0d(this, attr_name, attr_value, unusable, rc)
      class (FileMetadata), target, intent(inout) :: this
      character(len=*), intent(in) :: attr_name
      class (*), intent(in) :: attr_value
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      call this%global_var%add_attribute(attr_name, attr_value)
      __RETURN(__SUCCESS)
      __UNUSED_DUMMY(unusable)
   end subroutine add_attribute_0d

   subroutine add_attribute_1d(this, attr_name, values, unusable, rc)
      class (FileMetadata), target, intent(inout) :: this
      character(len=*), intent(in) :: attr_name
      class (*), intent(in) :: values(:)

      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      call this%global_var%add_attribute(attr_name, values)
      __RETURN(__SUCCESS)
      __UNUSED_DUMMY(unusable)
   end subroutine add_attribute_1d


   function get_attribute(this, attr_name, unusable, rc) result(ref)
      type (Attribute), pointer :: ref
      class (FileMetadata), target, intent(in) :: this
      character(len=*), intent(in) :: attr_name
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      ref => this%global_var%get_attribute(attr_name)
      __ASSERT(associated(ref),'FileMetadata::get_attribute() - no such attribute <'//attr_name//'>.')
      __RETURN(__SUCCESS)
      __UNUSED_DUMMY(unusable)
   end function get_attribute


   ! No RC is necessary - no failure mode.
   logical function has_attribute(this, attr_name)
      class (FileMetadata), target, intent(in) :: this
      character(len=*), intent(in) :: attr_name

      has_attribute = this%global_var%is_attribute_present(attr_name)

   end function has_attribute

   subroutine remove_attribute(this, attr_name)
      class (FileMetadata), target, intent(inout) :: this
      character(len=*), intent(in) :: attr_name

      call this%global_var%remove_attribute(attr_name)

   end subroutine


   function get_attributes(this, rc ) result(attributes)
      type (StringAttributeMap), pointer :: attributes
      class (FileMetadata), target, intent(in) :: this
      integer, optional, intent(out) :: rc

      attributes => this%global_var%get_attributes()
      __RETURN(__SUCCESS)
   end function get_attributes


   function get_variable(this, var_name, unusable, rc) result(var)
      class (Variable), pointer :: var
      class (FileMetadata), target, intent(in) :: this
      character(len=*), intent(in) :: var_name
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      var => this%variables%at(var_name)
      __RETURN(__SUCCESS)
      __UNUSED_DUMMY(unusable)
   end function get_variable

   logical function has_variable(this, var_name, unusable, rc) result(has)
      class (FileMetadata), target, intent(in) :: this
      character(len=*), intent(in) :: var_name
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      class (Variable), pointer :: var

      has = .false.
      var => this%variables%at(var_name)
      if (associated(var)) has = .true.
      __RETURN(__SUCCESS)
      __UNUSED_DUMMY(unusable)
   end function has_variable

   ! Returns null pointer unless var_name is a key corresponding to
   ! a CoordinateVariable value.
   ! rc returns __SUCCESS unless the var_name is not found at all.

   function get_coordinate_variable(this, var_name, unusable, rc) result(var)
      class (CoordinateVariable), pointer :: var
      class (FileMetadata), target, intent(in) :: this
      character(len=*), intent(in) :: var_name
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      class (Variable), pointer :: tmp


      tmp => this%variables%at(var_name)

      __ASSERT(associated(tmp),'can not find '//trim(var_name))

      select type (tmp)
      class is (CoordinateVariable)
         var => tmp
      class default
         var => null()
      end select

      __RETURN(__SUCCESS)
      __UNUSED_DUMMY(unusable)

   end function get_coordinate_variable

   logical function is_coordinate_variable(this, var_name, unusable, rc)
      class (FileMetadata),target, intent(in) :: this
      character(*), intent(in) :: var_name

      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      class (Variable), pointer :: tmp

      tmp => this%variables%at(var_name)

      __ASSERT(associated(tmp), 'can not find the varaible '//trim(var_name))
      select type (tmp)
      class is (CoordinateVariable)
         is_coordinate_variable = .true.
      class default
         is_coordinate_variable = .false.
      end select

      __RETURN(__SUCCESS)
      __UNUSED_DUMMY(unusable)
   end function is_coordinate_variable


   function get_variables(this, rc ) result(variables)
      type (StringVariableMap), pointer :: variables
      class (FileMetadata), target, intent(in) :: this
      integer, optional, intent(out) :: rc

      variables => this%variables
      __RETURN(__SUCCESS)
   end function get_variables

   function get_order(this, unusable, rc ) result(order)
      class (FileMetadata), target, intent(in) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      type (StringVector) :: order

      order = this%order
      __RETURN(__SUCCESS)
      __UNUSED_DUMMY(unusable)
   end function get_order

   subroutine set_order(this, newOrder, unusable, rc)
      class (FileMetadata), target, intent(inout) :: this
      type (StringVector), intent(in) :: newOrder
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(StringVectorIterator) :: iter
      class (Variable), pointer :: var
      character(len=:), pointer :: var_name

      __ASSERT(newOrder%size() == this%variables%size(),'New order must be same size as the variables')
      call this%order%erase(this%order%begin(),this%order%end())
      this%order = newOrder
      iter = this%order%begin()
      do while (iter/=this%order%end())
         var_name => iter%get()
         var => this%variables%at(var_name)
         __ASSERT(associated(var),trim(var_name)//' not in metadata')
         call iter%next()
      enddo
      __RETURN(__SUCCESS)
      __UNUSED_DUMMY(unusable)
   end subroutine set_order


   subroutine add_variable(this, var_name, var, unusable, rc)
      class (FileMetadata), target, intent(inout) :: this
      character(len=*), intent(in) :: var_name
      class (Variable), target, intent(in) :: var
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type (StringVector), pointer :: dims
      type (StringVectorIterator) :: iter
      integer, pointer :: dim_this
      character(len=:), pointer :: dim_name
      type (UnlimitedEntity), pointer :: const_value_ptr
      integer, allocatable :: shp(:), shp_const(:)
      integer :: empty(0)

      ! ensure all of var's dimensions are defined
      shp = empty
      dims => var%get_dimensions()
      iter = dims%begin()
      do while (iter /= dims%end())

         dim_name => iter%get()
         dim_this => this%dimensions%at(dim_name)
         __ASSERT( associated(dim_this),"FileMetadata::add_variable() - undefined dimension: " // dim_name)
         shp =[shp,dim_this]
         call iter%next()
      end do

      const_value_ptr => var%get_const_value()
      if ( .not. const_value_ptr%is_empty() ) then
         shp_const = const_value_ptr%get_shape()
         __ASSERT( all(shp == shp_const), "const_value shape does not match dims")
      endif

      call this%variables%insert(var_name, var)
      call this%order%push_back(var_name)
      __RETURN(__SUCCESS)
      __UNUSED_DUMMY(unusable)

   end subroutine add_variable

   subroutine modify_variable(this, var_name, var, unusable, rc)
      class (FileMetadata), target, intent(inout) :: this
      character(len=*), intent(in) :: var_name
      class (Variable), target, intent(in) :: var
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type (StringVector), pointer :: dims
      type (StringVectorIterator) :: iter
      integer, pointer :: dim_this
      character(len=:), pointer :: dim_name


      ! ensure all of var's dimensions are defined
      dims => var%get_dimensions()
      iter = dims%begin()
      do while (iter /= dims%end())
         dim_name => iter%get()
         dim_this => this%dimensions%at(dim_name)
         __ASSERT( associated(dim_this), "FileMetadata:: modify_variable() - undefined dimension " // dim_name )
         call iter%next()
      end do

      call this%variables%set(var_name, var)

      __RETURN(__SUCCESS)
      __UNUSED_DUMMY(unusable)

   end subroutine modify_variable

   subroutine remove_variable(this, var_name, unusable, rc)
      class (FileMetadata), target, intent(inout) :: this
      character(len=*), intent(in) :: var_name
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(StringVectorIterator)      :: viter
      type(StringVariableMapIterator) :: miter

      viter = this%order%begin()
      do while (viter /= this%order%end())
         if ( var_name == viter%get() ) then
           call  this%order%erase(viter)
           exit
         endif
         call viter%next()
      enddo
      miter = this%variables%find(var_name)
      call  this%variables%erase(miter)

      __RETURN(__SUCCESS)
      __UNUSED_DUMMY(unusable)

   end subroutine remove_variable

   subroutine add_var_attribute_0d(this, var_name, attr_name, value, unusable, rc)
      class (FileMetadata), target, intent(inout) :: this
      character(len=*), intent(in) :: var_name
      character(len=*), intent(in) :: attr_name
      class (*), intent(in) :: value
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      class (Variable), pointer :: var


      var => this%get_variable(var_name)
      call var%add_attribute(attr_name, value)

      __RETURN(__SUCCESS)
      __UNUSED_DUMMY(unusable)

   end subroutine add_var_attribute_0d

   subroutine add_var_attribute_1d(this, var_name, attr_name, values, unusable, rc)
      class (FileMetadata), target, intent(inout) :: this
      character(len=*), intent(in) :: var_name
      character(len=*), intent(in) :: attr_name
      class (*), intent(in) :: values(:)

      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      class (Variable), pointer :: var


      var => this%get_variable(var_name)
      call var%add_attribute(attr_name, values)
      __RETURN(__SUCCESS)
      __UNUSED_DUMMY(unusable)
   end subroutine add_var_attribute_1d

   subroutine merge(this, meta,rc)
      class (FileMetadata), target, intent(inout) :: this
      class (FileMetadata), target, intent(in) :: meta
      integer, optional, intent(out) :: rc
      type (StringIntegerMap), pointer :: dims
      type (StringVariableMap), pointer :: vars
      type (StringAttributeMap), pointer :: atts
      type (StringIntegerMapIterator) :: dim_iter
      type (StringVariableMapIterator):: var_iter
      type (StringAttributeMapIterator) :: att_iter
      type (Variable), pointer  :: var
      type (Attribute), pointer :: att
      character(len=:), pointer :: name
      integer :: extent

      ! merge dims
      dims => meta%get_dimensions()
      dim_iter = dims%begin()
      do while (dim_iter /= dims%end())
        name => dim_iter%key()
        extent = dim_iter%value()
        call this%add_dimension(name, extent)
        call dim_iter%next()
      end do

      ! merge attribute
      atts => meta%get_attributes()
      att_iter = atts%begin()
      do while (att_iter /= atts%end())
        name => att_iter%key()
        att => att_iter%value()
        call this%add_attribute(name, att)
        call att_iter%next()
      enddo

      ! merge variables
      vars => meta%get_variables()
      var_iter = vars%begin()
      do while (var_iter /= vars%end())
        name => var_iter%key()
        var  => var_iter%value()
        call this%add_variable(name, var)
        call var_iter%next()
      end do

      __RETURN(__SUCCESS)
   end subroutine merge

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

         equal = (a%global_var == b%global_var)

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

   subroutine serialize(this, buffer, rc)
      class (FileMetadata), intent(in) :: this
      integer,allocatable,intent(inout) :: buffer(:)
      integer, optional, intent(out) :: rc
      integer :: length
      integer, allocatable :: tmp_buffer(:)

      if(allocated(buffer)) deallocate(buffer)

      call StringIntegerMap_serialize(this%dimensions, tmp_buffer)
      buffer = [tmp_buffer]
      call this%global_var%serialize(tmp_buffer)
      buffer = [buffer,tmp_buffer]
      call StringVariableMap_serialize(this%variables, tmp_buffer)
      buffer = [buffer,tmp_buffer]
      call StringVector_serialize(this%order, tmp_buffer)
      buffer = [buffer,tmp_buffer]

      length = serialize_buffer_length(length) + size(buffer)
      buffer = [serialize_intrinsic(length),buffer]
      __RETURN(__SUCCESS)
   end subroutine

   subroutine FileMetadata_deserialize(buffer, fmd, rc)
      integer, intent(in) :: buffer(:)
      type (FileMetadata), intent(inout) :: fmd
      integer, optional, intent(out) :: rc
      integer :: status

      fmd = FileMetaData()
      call deserialize(fmd, buffer, rc=status)
      __VERIFY(status)
      __RETURN(__SUCCESS)

   contains

      subroutine deserialize(this, buffer, rc)
         class (FileMetadata), intent(inout) :: this
         integer, intent(in) :: buffer(:)
         integer, optional, intent(out) :: rc
         integer :: n,length
         integer :: status
         n = 1
         call deserialize_intrinsic(buffer(n:),length)
         __ASSERT(length <= size(buffer), "length does not match")

         length = serialize_buffer_length(length)
         n = n+length
         call StringIntegerMap_deserialize(buffer(n:),this%dimensions, status)
         __VERIFY(status)
         call deserialize_intrinsic(buffer(n:),length)
         n = n + length
         call deserialize_intrinsic(buffer(n:),length)
         call Variable_deserialize(buffer(n:n+length-1),this%global_var, status)
         __VERIFY(status)
         n = n + length
         call StringVariableMap_deserialize(buffer(n:), this%variables, status)

         call deserialize_intrinsic(buffer(n:),length)
         n = n + length
         call StringVector_deserialize(buffer(n:), this%order, status)
         __VERIFY(status)
         __RETURN(__SUCCESS)
      end subroutine deserialize
   end subroutine FileMetadata_deserialize

   subroutine set_source_file(this,source_file,rc)
      class (FileMetadata), intent(inout) :: this
      character(len=*), intent(in) :: source_file
      integer, optional, intent(out) :: rc

      this%source_file=source_file
      __RETURN(__SUCCESS)
   end subroutine

   function get_source_file(this,rc) result(source_file)
      character(len=:), allocatable :: source_file
      class (FileMetadata), intent(in) :: this
      integer, optional, intent(out) :: rc

      if (.not.allocated(this%source_file)) then
         __FAIL("FileMetadata not created via a file, no source_file present")
      end if
      source_file=this%source_file
      __RETURN(__SUCCESS)
   end function

   subroutine write_formatted(this, unit, iotype, v_list, iostat, iomsg)
      class(FileMetadata), intent(in) :: this
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      call write_dims(this%dimensions, unit, iotype, v_list, iostat, iomsg)
      if (iostat /= 0) return
      call write_variables(this%variables, unit, iotype, v_list, iostat, iomsg)
      if (iostat /= 0) return

      __UNUSED_DUMMY(v_list)
   end subroutine write_formatted

   subroutine write_dims(dimensions, unit, iotype, v_list, iostat, iomsg)
      type(StringIntegerMap), target, intent(in) :: dimensions
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      type(StringIntegerMapIterator) :: iter

      iostat = 0
      write(unit,'(a,/)')'dimensions:'
      associate (e => dimensions%end())
        iter = dimensions%begin()
        do while (iter /= e)
           write(unit, '(T8,a,1x,a,1x,i0,/)', iostat=iostat, iomsg=iomsg) iter%key(), "=" , iter%value()
           if (iostat /= 0) return
           call iter%next()
        end do
      end associate

      __UNUSED_DUMMY(iotype)
      __UNUSED_DUMMY(v_list)
   end subroutine write_dims

   subroutine write_variables(variables, unit, iotype, v_list, iostat, iomsg)
      type(StringVariableMap), target, intent(in) :: variables
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      type(StringVariableMapIterator) :: var_iter
      character(:), allocatable :: type_name, dims_str
      class(Variable), pointer :: var
      type(StringVector), pointer :: dims
      character(:), pointer :: var_name
      integer :: i

      iostat = 0
      write(unit,'(a,/)')'variables:'
      associate (e => variables%end())
        var_iter = variables%begin()
        do while (var_iter /= e)

           var_name => var_iter%key()
           var => var_iter%value()
           dims => var%get_dimensions()

           select case (var%get_type())
           case (pFIO_REAL32)
              type_name = 'float'
           case (pFIO_REAL64)
              type_name = 'double'
           case default
              type_name = '<unknown>'
           end select

           dims_str = "(" // dims%of(1)
           do i = 2, dims%size()
              dims_str = dims_str // ", " // dims%of(i)
           end do
           dims_str = dims_str // ")"

           write(unit, '(T8,a,1x,a,a,/)', iostat=iostat, iomsg=iomsg) type_name, var_name, dims_str
           if (iostat /= 0) return
           call var_iter%next()
        end do
      end associate

      __UNUSED_DUMMY(iotype)
      __UNUSED_DUMMY(v_list)
   end subroutine write_variables

end module pFIO_FileMetadataMod
