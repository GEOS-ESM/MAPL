#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

!=============================================================================
!FPP macros for repeated (type-dependent) code

#ifdef SET_VAL
#  undef SET_VAL
#endif

#define SET_VAL(T, VAL) \
type is (T) ;\
   if (default_is_present .and. .not. label_is_present) then ;\
      select type(default) ;\
      type is(T) ;\
         VAL = default ;\
      class default ;\
         _FAIL("Type of 'default' does not match type of 'VAL'.") ;\
      end select ;\
   else ;\
      call ESMF_ConfigGetAttribute(config, VAL, label = actual_label, _RC) ;\
   end if


#ifdef SET_VALS
#  undef SET_VALS
#endif

#define SET_VALS(T, VALS) \
type is (T) ;\
   if (default_is_present .and. .not. label_is_present) then ;\
      select type(default) ;\
      type is(T) ;\
         VALS = default ;\
      class default ;\
         _FAIL("Type of 'default' does not match type of 'VALS'.") ;\
      end select ;\
   else ;\
      call ESMF_ConfigGetAttribute(config, valuelist = VALS, count = count, label = actual_label, _RC) ;\
   end if

#ifdef SET_STRINGS
#  undef SET_STRINGS
#endif

#define SET_STRINGS(T, TSTR, TFMT) \
type is (T) ;\
   type_str = TSTR ;\
   val_str = intrinsic_to_string(val, TFMT) ;\
   if (present(default)) then ;\
      default_str = intrinsic_to_string(default, TFMT) ;\
   end if

#ifdef SET_TO_DEFAULT_VALUE
#  undef SET_TO_DEFAULT_VALUE
#endif

#define SET_TO_DEFAULT_VALUE(VALUE, TYPE) \
   select type(default) ; \
   type is(TYPE) ; \
      VALUE = default ; \
   class default ; \
         _FAIL("Type of 'default' does not match type of 'value'.") ; \
   end select


#ifdef PRINT_RESOURCE
#  undef PRINT_RESOURCE
#endif

#define PRINT_RESOURCE(VALUE, TYPE, FMT, TYPE_STR, FORMATTED) \
VALUE, TYPE_STRING, FORMATTED) ; \
   TYPE, intent(in) :: VALUE ; \
   character(len=:), intent(out) :: type_string ; \
   character(len=:), intent(out) :: formated_string ; \
   type_string = TYPE_STR ; \
   write(FORMATTED, FMT) value

!=============================================================================

module MAPL_ResourceMod

   !BOP
   ! !MODULE: MAPL_ResourceMod
   !
   ! !DESCRIPTION:  MAPL\_ResourceMod provides subroutines get scalar and array
   ! resources from ESMF_Config objects.

   ! !USES:

   use ESMF
   use ESMFL_Mod
   use gFTL2_StringVector
   use MAPL_CommsMod
   use MAPL_Constants, only: MAPL_CF_COMPONENT_SEPARATOR
   use MAPL_ExceptionHandling
   use MAPL_KeywordEnforcerMod
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64, int32, int64

   ! !PUBLIC MEMBER FUNCTIONS:

   implicit none
   private

   public MAPL_GetResource_config_scalar
   public MAPL_GetResource_config_array

contains

   ! MAPL searches for labels with certain prefixes as well as just the label itself
   pure function get_labels_with_prefix(label, component_name) result(labels_with_prefix)
      character(len=*), intent(in) :: label
      character(len=*), optional, intent(in) :: component_name
      character(len=ESMF_MAXSTR) :: component_type
      character(len=ESMF_MAXSTR) :: labels_with_prefix(4)

      if(present(component_name)) then
         component_type = component_name(index(component_name, ":") + 1:)

         ! The order to search for labels in resource files
         labels_with_prefix(1) = trim(component_name)//"_"//trim(label)
         labels_with_prefix(2) = trim(component_type)//"_"//trim(label)
         labels_with_prefix(3) = trim(label)
         labels_with_prefix(4) = trim(component_name)//MAPL_CF_COMPONENT_SEPARATOR//trim(label)
      else
         labels_with_prefix = ''
         labels_with_prefix(1) = label
      end if

   end function get_labels_with_prefix

   ! If possible, find label or label with prefix. Out: label found (logical)  ! version of label found,
   subroutine get_actual_label(config, label, label_is_present, actual_label, unusable, component_name, rc)
      type(ESMF_Config), intent(inout) :: config
      character(len=*), intent(in) :: label
      logical, intent(out) :: label_is_present
      character(len=:), allocatable, intent(out) :: actual_label
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), optional, intent(in) :: component_name
      integer, optional, intent(out) :: rc

      character(len=ESMF_MAXSTR), allocatable :: labels_to_try(:)
      integer :: i
      integer :: status

      _UNUSED_DUMMY(unusable)

      label_is_present = .false.

      ! If component_name is present, find label in some form in config. Else search
      ! for exact label

      labels_to_try = get_labels_with_prefix(label, component_name)

      do i = 1, size(labels_to_try)
         actual_label = trim(labels_to_try(i))
         if (len_trim(actual_label) == 0 ) cycle
         call ESMF_ConfigFindLabel(config, label = actual_label, isPresent = label_is_present, _RC)
         if (label_is_present) exit
      end do

      _RETURN(_SUCCESS)
   end subroutine get_actual_label

   ! Find value of scalar variable in config
   subroutine MAPL_GetResource_config_scalar(config, val, label, value_is_set, unusable, default, component_name, rc)
      type(ESMF_Config), intent(inout) :: config
      class(*), intent(inout) :: val
      character(len=*), intent(in) :: label
      logical , intent(out) :: value_is_set
      class(KeywordEnforcer), optional, intent(in) :: unusable
      class(*), optional, intent(in) :: default
      character(len=*), optional, intent(in) :: component_name
      integer, optional, intent(out) :: rc

      integer :: status, printrc
      logical :: default_is_present, label_is_present
      character(len=:), allocatable :: label_to_print
      character(len=:), allocatable :: actual_label

      _UNUSED_DUMMY(unusable)

      value_is_set = .FALSE.

      default_is_present = present(default)

      if (default_is_present) then
         _ASSERT(same_type_as(val, default), "Value and default must have same type")
      end if

      call get_actual_label(config, label, label_is_present, actual_label, component_name = component_name, _RC)

      ! No default and not in config, error
      ! label or default must be present
      if (.not. label_is_present .and. .not. default_is_present) then
         value_is_set = .FALSE.
         return
      end if

      select type(val)
      SET_VAL(integer(int32), val)
      SET_VAL(integer(int64), val)
      SET_VAL(real(real32), val)
      SET_VAL(real(real64), val)
      SET_VAL(character(len=*), val)
      SET_VAL(logical, val)
      class default
         _FAIL( "Unsupported type")
      end select
      
      value_is_set = .TRUE.

      call ESMF_ConfigGetAttribute(config, printrc, label = 'PRINTRC:', default = 0, _RC)  

      ! Can set printrc to negative to not print at all
      if (MAPL_AM_I_Root() .and. printrc >= 0) then
         if (label_is_present) then
            label_to_print = actual_label
         else
            label_to_print = trim(label)
         end if
         call print_resource_scalar(printrc, label_to_print, val, default=default, _RC)
      end if

      _RETURN(ESMF_SUCCESS)

   end subroutine MAPL_GetResource_config_scalar

   ! Find value of array variable in config
   subroutine MAPL_GetResource_config_array(config, vals, label, value_is_set, unusable, default, component_name, rc)
      type(ESMF_Config), intent(inout) :: config
      class(*), intent(inout) :: vals(:)
      character(len=*), intent(in) :: label
      logical, intent(out) :: value_is_set
      class(KeywordEnforcer), optional, intent(in) :: unusable
      class(*), optional, intent(in) :: default(:)
      character(len=*), optional, intent(in) :: component_name
      integer, optional, intent(out) :: rc

      character(len=:), allocatable :: actual_label
      integer :: status, count
      logical :: label_is_present, default_is_present

      _UNUSED_DUMMY(unusable)

      value_is_set = .FALSE.

      default_is_present = present(default)

      if (default_is_present) then
         _ASSERT(same_type_as(vals, default), "Value and default must have same type")
      end if

      _ASSERT(present(component_name), "Component name is necessary but not present.")
      call get_actual_label(config, label, label_is_present, actual_label, component_name = component_name, _RC)

      ! No default and not in config, error
      ! label or default must be present
      if (.not. label_is_present .and. .not. default_is_present) then
         value_is_set = .FALSE.
         return
      end if

      count = size(vals)

      select type(vals)
      SET_VALS(integer(int32), vals)
      SET_VALS(integer(int64), vals)
      SET_VALS(real(real32), vals)
      SET_VALS(real(real64), vals)
      SET_VALS(character(len=*), vals)
      SET_VALS(logical, vals)
      class default
         _FAIL( "Unsupported type")
      end select

      value_is_set = .TRUE.

      _RETURN(ESMF_SUCCESS)

   end subroutine MAPL_GetResource_config_array

   ! Print the resource value according to the value of printrc
   ! printrc = 0 - Only print non-default values
   ! printrc = 1 - Print all values
   subroutine print_resource_scalar(printrc, label, val, default, rc)
      integer, intent(in) :: printrc
      character(len=*), intent(in) :: label
      class(*), intent(in) :: val
      class(*), optional, intent(in) :: default
      integer, optional, intent(out) :: rc

      character(len=:), allocatable :: val_str, default_str, output_format, type_str, type_format
      type(StringVector), pointer, save :: already_printed_labels => null()
      integer :: status

      if (.not. associated(already_printed_labels)) then
         allocate(already_printed_labels)
      end if

      ! Do not print label more than once
      if (.not. vector_contains_str(already_printed_labels, trim(label))) then
         call already_printed_labels%push_back(trim(label))
      else
         return
      end if

      select type(val)
      SET_STRINGS(integer(int32), "'Integer*4 '", '(i0.1)')
      SET_STRINGS(integer(int64), "'Integer*8 '", '(i0.1)')
      SET_STRINGS(real(real32), "'Real*4 '" , '(f0.6)')
      SET_STRINGS(real(real64), "'Real*8 '" , '(f0.6)')
      SET_STRINGS(logical, "'Logical '" , '(l1)' )
      SET_STRINGS(character(len=*),"'Character '", '(a)') 
      class default
         _FAIL("Unsupported type")
      end select

      output_format = "(1x, " // type_str // ", 'Resource Parameter: '" // ", a"// ", a)a"

      ! printrc = 0 - Only print non-default values
      ! printrc = 1 - Print all values
      if (present(default)) then
         if (trim(val_str) /= trim(default_str) .or. printrc == 1) then
            print output_format, trim(label), trim(val_str)
         end if
      else
         print output_format, trim(label), trim(val_str)
      end if

      _RETURN(_SUCCESS)

   end subroutine print_resource_scalar

   ! Check if vector contains string
   logical function vector_contains_str(vector, string)
      type(StringVector), intent(in) :: vector
      character(len=*), intent(in) :: string
      type(StringVectorIterator) :: iter

      iter = vector%begin()

      vector_contains_str = .false.

      do while (iter /= vector%end())
         if (trim(string) == iter%of()) then
            vector_contains_str = .true.
            return
         end if
         call iter%next()
      end do

   end function vector_contains_str

   ! Convert val to string according to str_format
   function intrinsic_to_string(val, str_format, rc) result(formatted_str)
      class(*), intent(in) :: val
      character(len=*), intent(in) :: str_format
      character(len=256) :: formatted_str
      integer, optional, intent(out) :: rc

      select type(val)
      type is(integer(int32))
         write(formatted_str, str_format) val
      type is(integer(int64))
         write(formatted_str, str_format) val
      type is(real(real32))
         write(formatted_str, str_format) val
      type is(real(real64))
         write(formatted_str, str_format) val
      type is(logical)
         write(formatted_str, str_format) val
      type is(character(len=*))
         formatted_str = trim(val)
         class default
         _FAIL( "Unsupported type in intrinsic_to_string")
      end select

      _RETURN(_SUCCESS)

   end function intrinsic_to_string

   subroutine print_resource_array(printrc, label, vals, default, rc)
      integer, intent(in) :: printrc
      character(len=*), intent(in) :: label
      class(*), intent(in) :: vals(:)
      class(*), optional, intent(in) :: default(:)
      integer, optional, intent(out) :: rc

      character(len=:), allocatable :: val_str, default_str, output_format
      character(len=:), allocatable :: type_str, type_format
      type(StringVector), pointer, save :: already_printed_labels => null()
      integer :: i
      character(len=2) :: size_vals_str
      logical :: value_equals_default
      character(len=:), allocatable :: default_tag

      if (.not. associated(already_printed_labels)) then
         allocate(already_printed_labels)
      end if

      ! Do not print label more than once
      if (.not. vector_contains_str(already_printed_labels, trim(label))) then
         call already_printed_labels%push_back(trim(label))
      else
         return
      end if

      ! Cast the size(vals) to a string making sure to strip any spaces
      ! We assume we'll never have more than 99 values
      write(size_vals_str, '(i2)') size(vals)

      select type(vals)
      type is(integer(int32))
         type_str = "'Integer*4 '"
         type_format = '('//trim(size_vals_str)//'(i0.1,1X))'
         val_str = intrinsic_array_to_string(vals, type_format)
         if (present(default)) then
            default_str = intrinsic_array_to_string(default, type_format)
         end if
      type is(integer(int64))
         type_str = "'Integer*8 '"
         type_format = '('//trim(size_vals_str)//'(i0.1,1X))'
         val_str = intrinsic_array_to_string(vals, type_format)
         if (present(default)) then
            default_str = intrinsic_array_to_string(default, type_format)
         end if
      type is(real(real32))
         type_str = "'Real*4 '"
         type_format = '('//trim(size_vals_str)//'(f0.6,1X))'
         val_str = intrinsic_array_to_string(vals, type_format)
         if (present(default)) then
            default_str = intrinsic_array_to_string(default, type_format)
         end if
      type is(real(real64))
         type_str = "'Real*8 '"
         type_format = '('//trim(size_vals_str)//'(f0.6,1X))'
         val_str = intrinsic_array_to_string(vals, type_format)
         if (present(default)) then
            default_str = intrinsic_array_to_string(default, type_format)
         end if
       type is(logical)
         type_str = "'Logical '"
         type_format = '('//trim(size_vals_str)//'(l1,1X))'
         val_str = intrinsic_array_to_string(vals, type_format)
         if (present(default)) then
            default_str = intrinsic_array_to_string(default, type_format)
         end if
      type is(character(len=*))
         type_str = "'Character '"
         do i = 1, size(vals)
            val_str(i:i) = trim(vals(i))
         end do
         if (present(default)) then
            default_str = intrinsic_array_to_string(default, 'a')
         end if
      class default
         _FAIL("Unsupported type")
      end select

      if(allocated(default_str)) then
         output_parameter(printrc, type_str, val_str, default_str)
      else
         output_parameter(printrc, type_str, val_str)
      end if

   end subroutine print_resource_array

   function intrinsic_array_to_string(vals, str_format, rc) result(formatted_str)
      class(*), intent(in) :: vals(:)
      character(len=*), intent(in) :: str_format
      character(len=1024) :: formatted_str
      integer, optional, intent(out) :: rc

      integer :: i

      select type(vals)
      type is(integer(int32))
         write(formatted_str, str_format) vals
      type is(integer(int64))
         write(formatted_str, str_format) vals
      type is(real(real32))
         write(formatted_str, str_format) vals
      type is(real(real64))
         write(formatted_str, str_format) vals
      type is(logical)
         write(formatted_str, str_format) vals
      type is(character(len=*))
         do i = 1, size(vals)
            formatted_str(i:i) = vals(i)
         end do
      class default
         _FAIL( "Unsupported type in intrinsic_array_to_string")
      end select

   end function intrinsic_array_to_string

   subroutine output_parameter(printrc, type_str, val_str, default_str)
      character(len=*), intent(in) :: type_str
      character(len=*), intent(in) :: val_str
      character(len=*), intent(in), optional :: default_str
      integer, intent(in) :: printrc
      character(len=:), allocatable :: output_format
      character(len=*), parameter :: DEFAULT_ = ", (default value)"
      character(len=*), parameter :: NOT_DEFAULT = ''
      character(len=*), parameter :: NO_DEFAULT = ''

      output_format = "(1x, " // type_str // ", 'Resource Parameter: '" // ", a" // ", a" // "a)"

      ! printrc = 0 - Only print non-default values
      ! printrc = 1 - Print all values with label if default value
      if (present(default_str)) then
         if trim(val_str) == trim(default_str) then
            if printrc == 1 then
               print output_format, trim(label), trim(val_str), trim(DEFAULT_)
            end if
         else
            print output_format, trim(label), trim(val_str), trim(NOT_DEFAULT)
         end if 
      else
         print output_format, trim(label), trim(val_str), trim(NO_DEFAULT)
      end if
   end subroutine output_parameter
!!!!!!!!!!!!!!!!!!!
   ! Find value of scalar variable in config
   subroutine MAPL_GetResource_config_scalar(config, val, label, value_is_set, unusable, default, component_name, rc)
      type(ESMF_Config), intent(inout) :: config
      class(*), intent(inout) :: val
      character(len=*), intent(in) :: label
      logical , intent(out) :: value_is_set
      class(KeywordEnforcer), optional, intent(in) :: unusable
      class(*), optional, intent(in) :: default
      character(len=*), optional, intent(in) :: component_name
      integer, optional, intent(out) :: rc

      integer :: status, printrc
      logical :: default_is_present, label_is_present
      logical :: do_print_value
      character(len=:), allocatable :: label_to_print
      character(len=:), allocatable :: actual_label

      _UNUSED_DUMMY(unusable)

      value_is_set = .FALSE.
      do_print_value = .FALSE.

      default_is_present = present(default)

      if (default_is_present) then
         _ASSERT(same_type_as(val, default), "Value and default must have same type")
      end if

      call get_actual_label(config, label, label_is_present, actual_label, component_name = component_name, _RC)

      ! No default and not in config, error
      ! label or default must be present
      if (.not. (label_is_present .or. default_is_present)) then
         value_is_set = .FALSE.
         return
      end if

      ! Can set printrc to negative to not print at all
      if (do_print_value .and. MAPL_AM_I_Root()) then
         call ESMF_ConfigGetAttribute(config, printrc, label = 'PRINTRC:', default = 0, _RC)  
         do_print_value = (printrc >= 0)
      end if

      if do_print_value then
         if label_is_present then
            label_to_print = actual_label
         else
            label_to_print = trim(label)
         end if
      end if

      if label_is_present then
         if default_is_present then
            ! use config setter with value checker
         else
            ! use config setter without checker
         end if
      else
         ! use default setter without checker
      end if

      select type(val)
      SET_VAL(integer(int32), val)
      SET_VAL(integer(int64), val)
      SET_VAL(real(real32), val)
      SET_VAL(real(real64), val)
      SET_VAL(character(len=*), val)
      SET_VAL(logical, val)
      class default
         _FAIL( "Unsupported type")
      end select
      
      value_is_set = .TRUE.

      _RETURN(ESMF_SUCCESS)

   end subroutine MAPL_GetResource_config_scalar

end module MAPL_ResourceMod

module HandlerMod

   integer, parameter :: MAX_LEN = 256

   interface setter
      module procedure default_setter 
      module procedure config_setter
   end interface setter

   interface printer
      module procedure value_printer
      module procedure null_printer
   end interface printer

   type Handler
      type(setter)  :: _setter
      type(outputter) :: _outputter
   contains
      module procedure set_value
      module procedure output_value 
   end type Handler

   abstract interface
      subroutine value_setter
      end subroutine value_setter
   end abstract interface

   interface set_value_to_default_value
      module procedure set_to_default_value_int32
   end interface set_value_to_default_value

   type, abstract :: AResource
      character(len=MAX_LEN), parameter :: type_string = 'TYPE'
   contains
      procedure(make_resource_string), deferred, pass(this) :: get_string
      procedure(return_type_string), deferred, pass(this) :: get_type_string   
   end type AResource
 
   abstract interface 
      function make_resource_string() result(formatted_string)
         character(len=MAX_LEN) :: formatted_string
      end function make_resource_string

      function return_type_string() result (type_string)
         character(len=MAX_LEN) :: type_string
      end function return_type_string
   end abstract interface 

   type, extends(AResource) :: Int32Resource
      character(len=MAX_LEN), parameter :: type_string = "'Integer*4 '"
      integer(int32) :: value
   contains
      procedure(make_resource_string), pass(this) :: get_string => get_string_int32
      procedure(return_type_string), pass(this) :: get_type_string
   end type Int32Resource
contains
   
   subroutine get_string_int32(this) result(formatted_string)
      class(Int32Resource), intent(in) :: this
      character(len=MAX_LEN) :: formatted_string
      character(len=*), parameter :: fstring='(i0.1)' 
      write(formatted_string, fstring) this%value
   end subroutine get_string_int32
   
   subroutine return_type_string(this) result(type_string)
      class(AResource), intent(in) :: this
      character(len=MAX_LEN) :: type_string
      type_string = this%type_string
   end subroutine return_type_string

   subroutine set_to_default_value_int32(value)
      integer(int32), intent(in) :: value
      SET_TO_DEFAULT_VALUE(value, integer(int32))
   end subroutine set_to_default_value_int32

   subroutine set_to_default_caller(_printer)
      procedure(printer), intent(in) :: _printer
      call set_to_default_value(value)
      call _printer(value, .TRUE.)
   end subroutine set_to_default_caller

   subroutine set_value_from config(value)
   end subroutine set_value_from config

   subroutine print_resource_int32(print_resource_scalar(value, integer(int32), '(i0.1)', type_string, formatted_string)
   end subroutine print_resource_int32

! derived type that accepts value/type, a setter, and a printer
! do_print, label_is_present, default_is_present => setter and printer
!  if not (label_is_present or default_is_present) => value not set and return
!
!  getter abstract interface for sub on value, with default polymorphic
!  if label_is_present 
!     getter: [get value from config]
!  else
!     getter: [get value from default]
!  end if
!
!  printer abstract interface for sub on value, with default polymorphic
!  if do_print
!     if default_is_present and label_is_present
!        printer: [print based on default]
!     else
!        printer: [print based value]
!     end if
!  else
!     printer: null
!  end if
end module HandlerMod
