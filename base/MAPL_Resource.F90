#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

!=============================================================================
!FPP macros for repeated (type-dependent) code

#ifdef SET_VALUE
#  undef SET_VALUE
#endif

#define SET_VALUE(T, VAL) \
type is(T) ;\
   if (label_is_present) then ;\
      call ESMF_ConfigGetAttribute(config, VAL, label = actual_label, _RC) ;\
   else ;\
      select type(default) ;\
      type is(T) ;\
         VAL = default ;\
      class default ;\
         _FAIL(MISMATCH_MESSAGE) ;\
      end select ;\
      value_is_default = .TRUE. ;\
   end if ;\
   call set_do_print(actual_label, do_print)

!=============================================================================

#ifdef SET_ARRAY_VALUE
#  undef SET_ARRAY_VALUE
#endif

#define SET_ARRAY_VALUE(T, VAL) \
type is(T) ;\
   if (label_is_present) then ;\
      call ESMF_ConfigGetAttribute(config, valuelist = VAL, count = count, label = actual_label, _RC) ;\
   else ;\
      select type(default) ;\
      type is(T) ;\
         VAL = default ;\
      class default ;\
         _FAIL(MISMATCH_MESSAGE) ;\
      end select ;\
      value_is_default = .TRUE. ;\
   end if ;\
   call set_do_print(actual_label, do_print)

!=============================================================================

#ifdef MAKE_STRINGS
#  undef MAKE_STRINGS
#endif

#define MAKE_STRINGS(T, VAL, TSTR, FMT) \
      if (label_is_present) then ;\
         if(default_is_present) then ;\
            select type(default) ;\
            type is(T) ;\
               value_is_default = (VAL == default) ;\
            class default ;\
               _FAIL(MISMATCH_MESSAGE) ;\
            end select ;\
         else ;\
            value_is_default = .FALSE. ;\
         end if ;\
      end if ;\
      if (.not. (print_nondefault_only .and. value_is_default)) then ;\
         type_string = TSTR ;\
         type_format = scalar_format(FMT) ;\
         write(formatted_value, type_format) VAL ;\
      else ;\
         do_print = .FALSE. ;\
      end if

!=============================================================================

#ifdef MAKE_ARRAY_STRINGS
#  undef MAKE_ARRAY_STRINGS
#endif

#define MAKE_ARRAY_STRINGS(T, VAL, TSTR, FMT) \
      if (label_is_present) then ;\
         if(default_is_present) then ;\
            select type(default) ;\
            type is(T) ;\
               value_is_default = all(VAL == default) ;\
            class default ;\
               _FAIL(MISMATCH_MESSAGE) ;\
            end select ;\
         else ;\
            value_is_default = .FALSE. ;\
         end if ;\
      end if ;\
      if (.not. (print_nondefault_only .and. value_is_default)) then ;\
         type_string = TSTR ;\
         write(array_size_string, '(i2)') size(VAL) ;\
         type_format = array_format(FMT, array_size_string) ;\
         write(formatted_value, type_format) VAL ;\
      else ;\
         do_print = .FALSE. ;\
      end if

!END FPP macros for repeated (type-dependent) code
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

   character(len=*), parameter :: MISMATCH_MESSAGE = "Type of 'default' does not match type of 'value'."

   character(len=*), parameter :: FMT_INT32 = 'i0.1'
   character(len=*), parameter :: FMT_INT64 = 'i0.1'
   character(len=*), parameter :: FMT_REAL32 = 'f0.6'
   character(len=*), parameter :: FMT_REAL64 = 'f0.6'
   character(len=*), parameter :: FMT_LOGICAL= 'l1'
   character(len=*), parameter :: FMT_CHARACTER = 'a'

   character(len=*), parameter :: TYPE_INT32 = "'Integer*4 '" 
   character(len=*), parameter :: TYPE_INT64 = "'Integer*8 '" 
   character(len=*), parameter :: TYPE_REAL32 = "'Real*4 '"
   character(len=*), parameter :: TYPE_REAL64 = "'Real*8 '"
   character(len=*), parameter :: TYPE_LOGICAL =  "'Logical '"
   character(len=*), parameter :: TYPE_CHARACTER = "'Character '"

contains

   subroutine get_print_settings(config, default_is_present, do_print, print_nondefault_only, rc)
      type(ESMF_Config), intent(inout) :: config
      logical, intent(in) :: default_is_present
      logical, intent(out) :: do_print
      logical, intent(out) :: print_nondefault_only
      integer, optional, intent(out) :: rc

      integer, parameter :: PRINT_ALL = 1
      integer, parameter :: PRINT_DIFFERENT = 0
      
      integer :: printrc
      integer :: status

      if (MAPL_AM_I_Root()) then
         call ESMF_ConfigGetAttribute(config, printrc, label = 'PRINTRC:', default = 0, _RC)  
         print_nondefault_only = ((printrc == PRINT_DIFFERENT) .and. default_is_present)
         do_print = (print_nondefault_only .or. (printrc == PRINT_ALL))
      else
         do_print = .FALSE.
      end if

   end subroutine get_print_settings

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

   ! Check if resource has already been printed (vector contains label) or should be printed
   subroutine set_do_print(label, do_print)
      character(*), intent(in) :: label
      logical, intent(inout) :: do_print

      type(StringVector), pointer, save :: already_printed_labels => null()

      if (do_print) then
         if (.not. associated(already_printed_labels)) then
            allocate(already_printed_labels)
         end if

         ! Do not print label more than once
         if (.not. vector_contains_str(already_printed_labels, trim(label))) then
            call already_printed_labels%push_back(trim(label))
         else
            do_print = .FALSE.
         end if
      end if

   end subroutine set_do_print

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

      if (.not. label_is_present) actual_label = trim(label)

      _RETURN(_SUCCESS)
   end subroutine get_actual_label

   ! Find value of scalar variable in config
   subroutine MAPL_GetResource_config_scalar(config, val, label, value_is_set, unusable, default, component_name, rc)
      type(ESMF_Config), intent(inout) :: config
      class(*), intent(inout) :: val
      character(len=*), intent(in) :: label
      logical, intent(out) :: value_is_set
      class(KeywordEnforcer), optional, intent(in) :: unusable
      class(*), optional, intent(in) :: default
      character(len=*), optional, intent(in) :: component_name
      integer, optional, intent(out) :: rc

      character(len=:), allocatable :: actual_label
      character(len=:), allocatable :: type_format
      character(len=:), allocatable :: type_string
      character(len=ESMF_MAXSTR) :: formatted_value

      logical :: default_is_present
      logical :: label_is_present
      logical :: print_nondefault_only
      logical :: do_print
      logical :: value_is_default

      integer :: status

      _UNUSED_DUMMY(unusable)

      ! these need to be initialized explitictly 
      value_is_set = .FALSE.
      default_is_present = present(default)
      label_is_present = .FALSE.
      print_nondefault_only = .FALSE.
      do_print = .FALSE.
      value_is_default = .FALSE.
      
      if (default_is_present) then
         _ASSERT(same_type_as(val, default), "Value and default must have same type")
      end if

      call get_actual_label(config, label, label_is_present, actual_label, component_name = component_name, _RC)

      if(.not. (label_is_present .or. default_is_present)) then
         ! label or default must be present
         value_is_set = .FALSE.
         return
      end if

      ! only print if root
      call get_print_settings(config, default_is_present, do_print, print_nondefault_only, _RC)
   
      select type(val)

      SET_VALUE(integer(int32), val)
      if (do_print) then
         MAKE_STRINGS(integer(int32), val, TYPE_INT32, FMT_INT32)
      end if

      SET_VALUE(integer(int64), val)
      if (do_print) then
         MAKE_STRINGS(integer(int64), val, TYPE_INT64, FMT_INT64)
      end if

      SET_VALUE(real(real32), val)
      if (do_print) then
         MAKE_STRINGS(real(real32), val, TYPE_REAL32, FMT_REAL32)
      end if

      SET_VALUE(real(real64), val)
      if (do_print) then
         MAKE_STRINGS(real(real64), val, TYPE_REAL64, FMT_REAL64)
      end if

      SET_VALUE(logical, val)
      if (do_print) then
         MAKE_STRINGS(logical, val, TYPE_LOGICAL, FMT_LOGICAL)
      end if

      SET_VALUE(character(len=*), val)
      if (do_print) then 
         if (label_is_present) then 
            if(default_is_present) then 
               select type(default) 
               type is(character(len=*)) 
                  value_is_default = (trim(val) == trim(default)) 
               class default 
                  _FAIL(MISMATCH_MESSAGE) 
               end select 
            else 
               value_is_default = .FALSE. 
            end if 
         end if 
         if (.not. (print_nondefault_only .and. value_is_default)) then 
            type_string = TYPE_CHARACTER
            formatted_value = trim(val) 
         else 
            do_print = .FALSE. 
         end if 
      end if
      
      class default
         _FAIL( "Unsupported type")
      end select
      
      if(do_print) call print_resource(type_string, actual_label, formatted_value, value_is_default, _RC)

      value_is_set = .TRUE.

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
      character(len=2) :: array_size_string
      ! We assume we'll never have more than 99 values, hence len=2

      character(len=:), allocatable :: actual_label
      character(len=:), allocatable :: type_format
      character(len=:), allocatable :: type_string
      character(len=ESMF_MAXSTR) :: formatted_value

      logical :: default_is_present
      logical :: label_is_present
      logical :: print_nondefault_only
      logical :: do_print
      logical :: value_is_default
      integer :: count

      integer :: status

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

      SET_ARRAY_VALUE(integer(int32), vals)
      if (do_print) then
         MAKE_ARRAY_STRINGS(integer(int32), vals, TYPE_INT32, FMT_INT32)
      end if

      SET_ARRAY_VALUE(integer(int64), vals)
      if (do_print) then
         MAKE_ARRAY_STRINGS(integer(int64), vals,  TYPE_INT64, FMT_INT64)
      end if

      SET_ARRAY_VALUE(real(real32), vals)
      if (do_print) then
         MAKE_ARRAY_STRINGS(real(int32), vals,  TYPE_REAL32, FMT_REAL32)
      end if

      SET_ARRAY_VALUE(real(real64), vals)
      if (do_print) then
         MAKE_ARRAY_STRINGS(real(int64), vals,  TYPE_REAL64, FMT_REAL64)
      end if

      SET_ARRAY_VALUE(logical, vals)
      if (do_print) then
         MAKE_ARRAY_STRINGS(logical, vals,  TYPE_LOGICAL, FMT_LOGICAL)
      end if

      SET_ARRAY_VALUE(character(len=*), vals)
      if (do_print) then
         if (label_is_present) then 
            if(default_is_present) then 
               select type(default) 
               type is(character(len=*)) 
                  value_is_default = compare_all(vals, default) 
               class default 
                  _FAIL(MISMATCH_MESSAGE) 
               end select 
            else 
               value_is_default = .FALSE. 
            end if 
         end if 
         if (.not. (print_nondefault_only .and. value_is_default)) then 
            type_string = TYPE_CHARACTER
            write(array_size_string, '(i2)') size(vals)
            type_format = array_format(FMT_CHARACTER//new_line('a'), array_size_string) ;\
            write(formatted_value, type_format) vals
         else 
            do_print = .FALSE. 
         end if 
      end if

      class default
         _FAIL( "Unsupported type")
      end select

      if(do_print) call print_resource(type_string, actual_label, formatted_value, value_is_default, _RC)

      value_is_set = .TRUE.

      _RETURN(ESMF_SUCCESS)

   end subroutine MAPL_GetResource_config_array

   ! Print the resource value
   subroutine print_resource(type_string, label, formatted_value, value_is_default, rc)
      character(len=*), intent(in) :: type_string
      character(len=*), intent(in) :: label
      character(len=*), intent(in) :: formatted_value
      logical, intent(in) :: value_is_default
      integer, optional, intent(out) :: rc

      character(len=*), parameter :: DEFAULT_ = ", (default value)"
      character(len=*), parameter :: NONDEFAULT_ = ''
      character(len=:), allocatable :: output_format
      character(len=:), allocatable :: trailer
      character(len=:), allocatable :: value_out

      trailer = NONDEFAULT_
      if (value_is_default) trailer = DEFAULT_

      output_format = "(1x, " // type_string // ", 'Resource Parameter: '" // ", a"// ", a)"
      value_out = trim(formatted_value) // trim(trailer)

      print output_format, trim(label), value_out

      _RETURN(_SUCCESS)

   end subroutine print_resource

   pure function scalar_format(fmt)
      character(len=*), intent(in) :: fmt
      character(len=:), allocatable :: scalar_format

      scalar_format = '('//fmt//')'

   end function scalar_format

   pure function array_format(fmt, array_size_string)
      character(len=*), intent(in) :: fmt
      character(len=*), intent(in) :: array_size_string
      character(len=:), allocatable :: array_format

      array_format = '('//trim(array_size_string)//'('//fmt//')'

   end function array_format

   pure function compare_all(astrings, bstrings)
      character(len=*), dimension(:), intent(in) :: astrings
      character(len=*), dimension(:), intent(in) :: bstrings
      logical :: compare_all

      integer :: i

      compare_all = (size(astrings) == size(bstrings))

      do i=1, size(astrings)
         if(.not. compare_all) exit
         compare_all = (trim(astrings(i)) == trim(bstrings(i)))
      end do
         
   end function compare_all

!   pure function concat_strings(strings, join_string)
!      character(len=*), dimension(:), intent(in) :: strings
!      character(len=*), optional, intent(in) :: join_string
!      character(len=:), allocatable :: concat_strings
!      character(len=*) :: DEFAULT_JOIN_STRING
!      character(len=:), allocatable :: js
!
!   end function concat_strings

end module MAPL_ResourceMod
!===================================== OLD ====================================!
!   ! Print the resource value according to the value of printrc
!   ! printrc = 0 - Only print non-default values
!   ! printrc = 1 - Print all values
!   subroutine print_resource_scalar(printrc, label, val, default, rc)
!      integer, intent(in) :: printrc
!      character(len=*), intent(in) :: label
!      class(*), intent(in) :: val
!      class(*), optional, intent(in) :: default
!      integer, optional, intent(out) :: rc
!
!      character(len=:), allocatable :: val_str, default_str, output_format, type_str, type_format
!      type(StringVector), pointer, save :: already_printed_labels => null()
!      integer :: status
!
!      if (.not. associated(already_printed_labels)) then
!         allocate(already_printed_labels)
!      end if
!
!      ! Do not print label more than once
!      if (.not. vector_contains_str(already_printed_labels, trim(label))) then
!         call already_printed_labels%push_back(trim(label))
!      else
!         return
!      end if
!
!      select type(val)
!      SET_STRINGS(integer(int32), "'Integer*4 '", '(i0.1)')
!      SET_STRINGS(integer(int64), "'Integer*8 '", '(i0.1)')
!      SET_STRINGS(real(real32), "'Real*4 '" , '(f0.6)')
!      SET_STRINGS(real(real64), "'Real*8 '" , '(f0.6)')
!      SET_STRINGS(logical, "'Logical '" , '(l1)' )
!      SET_STRINGS(character(len=*),"'Character '", '(a)') 
!      class default
!         _FAIL("Unsupported type")
!      end select
!
!      output_format = "(1x, " // type_str // ", 'Resource Parameter: '" // ", a"// ", a)a"
!
!      ! printrc = 0 - Only print non-default values
!      ! printrc = 1 - Print all values
!      if (present(default)) then
!         if (trim(val_str) /= trim(default_str) .or. printrc == 1) then
!            print output_format, trim(label), trim(val_str)
!         end if
!      else
!         print output_format, trim(label), trim(val_str)
!      end if
!
!      _RETURN(_SUCCESS)
!
!   end subroutine print_resource_scalar
!
!   ! Convert val to string according to str_format
!   function intrinsic_to_string(val, str_format, rc) result(formatted_str)
!      class(*), intent(in) :: val
!      character(len=*), intent(in) :: str_format
!      character(len=256) :: formatted_str
!      integer, optional, intent(out) :: rc
!
!      select type(val)
!      type is(integer(int32))
!         write(formatted_str, str_format) val
!      type is(integer(int64))
!         write(formatted_str, str_format) val
!      type is(real(real32))
!         write(formatted_str, str_format) val
!      type is(real(real64))
!         write(formatted_str, str_format) val
!      type is(logical)
!         write(formatted_str, str_format) val
!      type is(character(len=*))
!         formatted_str = trim(val)
!         class default
!         _FAIL( "Unsupported type in intrinsic_to_string")
!      end select
!
!      _RETURN(_SUCCESS)
!
!   end function intrinsic_to_string
!   
!   subroutine print_resource_array(printrc, label, vals, default, rc)
!      integer, intent(in) :: printrc
!      character(len=*), intent(in) :: label
!      class(*), intent(in) :: vals(:)
!      class(*), optional, intent(in) :: default(:)
!      integer, optional, intent(out) :: rc
!
!      character(len=:), allocatable :: val_str, default_str, output_format
!      character(len=:), allocatable :: type_str, type_format
!      type(StringVector), pointer, save :: already_printed_labels => null()
!      integer :: i
!      character(len=2) :: size_vals_str
!      logical :: value_equals_default
!      character(len=:), allocatable :: default_tag
!
!      if (.not. associated(already_printed_labels)) then
!         allocate(already_printed_labels)
!      end if
!
!      ! Do not print label more than once
!      if (.not. vector_contains_str(already_printed_labels, trim(label))) then
!         call already_printed_labels%push_back(trim(label))
!      else
!         return
!      end if
!
!      character(len=2) :: size_vals_str
!      ! Cast the size(vals) to a string making sure to strip any spaces
!      ! We assume we'll never have more than 99 values
!      write(size_vals_str, '(i2)') size(vals)
!
!      select type(vals)
!      type is(integer(int32))
!         type_str = "'Integer*4 '"
!         type_format = '('//trim(size_vals_str)//'(i0.1,1X))'
!         val_str = intrinsic_array_to_string(vals, type_format)
!         if (present(default)) then
!            default_str = intrinsic_array_to_string(default, type_format)
!         end if
!      type is(integer(int64))
!         type_str = "'Integer*8 '"
!         type_format = '('//trim(size_vals_str)//'(i0.1,1X))'
!         val_str = intrinsic_array_to_string(vals, type_format)
!         if (present(default)) then
!            default_str = intrinsic_array_to_string(default, type_format)
!         end if
!      type is(real(real32))
!         type_str = "'Real*4 '"
!         type_format = '('//trim(size_vals_str)//'(f0.6,1X))'
!         val_str = intrinsic_array_to_string(vals, type_format)
!         if (present(default)) then
!            default_str = intrinsic_array_to_string(default, type_format)
!         end if
!      type is(real(real64))
!         type_str = "'Real*8 '"
!         type_format = '('//trim(size_vals_str)//'(f0.6,1X))'
!         val_str = intrinsic_array_to_string(vals, type_format)
!         if (present(default)) then
!            default_str = intrinsic_array_to_string(default, type_format)
!         end if
!       type is(logical)
!         type_str = "'Logical '"
!         type_format = '('//trim(size_vals_str)//'(l1,1X))'
!         val_str = intrinsic_array_to_string(vals, type_format)
!         if (present(default)) then
!            default_str = intrinsic_array_to_string(default, type_format)
!         end if
!      type is(character(len=*))
!         type_str = "'Character '"
!         do i = 1, size(vals)
!            val_str(i:i) = trim(vals(i))
!         end do
!         if (present(default)) then
!            default_str = intrinsic_array_to_string(default, 'a')
!         end if
!      class default
!         _FAIL("Unsupported type")
!      end select
!
!      if(allocated(default_str)) then
!         output_parameter(printrc, type_str, val_str, default_str)
!      else
!         output_parameter(printrc, type_str, val_str)
!      end if
!
!   end subroutine print_resource_array
!
!   function intrinsic_array_to_string(vals, str_format, rc) result(formatted_str)
!      class(*), intent(in) :: vals(:)
!      character(len=*), intent(in) :: str_format
!      character(len=1024) :: formatted_str
!      integer, optional, intent(out) :: rc
!
!      integer :: i
!
!      select type(vals)
!      type is(integer(int32))
!         write(formatted_str, str_format) vals
!      type is(integer(int64))
!         write(formatted_str, str_format) vals
!      type is(real(real32))
!         write(formatted_str, str_format) vals
!      type is(real(real64))
!         write(formatted_str, str_format) vals
!      type is(logical)
!         write(formatted_str, str_format) vals
!      type is(character(len=*))
!         do i = 1, size(vals)
!            formatted_str(i) = trim(vals(i))
!         end do
!      class default
!         _FAIL( "Unsupported type in intrinsic_array_to_string")
!      end select
!
!   end function intrinsic_array_to_string
!
!   subroutine output_parameter(printrc, type_str, val_str, default_str)
!      character(len=*), intent(in) :: type_str
!      character(len=*), intent(in) :: val_str
!      character(len=*), intent(in), optional :: default_str
!      integer, intent(in) :: printrc
!      character(len=:), allocatable :: output_format
!      character(len=*), parameter :: DEFAULT_ = ", (default value)"
!      character(len=*), parameter :: NOT_DEFAULT = ''
!      character(len=*), parameter :: NO_DEFAULT = ''
!
!      output_format = "(1x, " // type_str // ", 'Resource Parameter: '" // ", a" // ", a" // "a)"
!
!      ! printrc = 0 - Only print non-default values
!      ! printrc = 1 - Print all values with label if default value
!      if (present(default_str)) then
!         if trim(val_str) == trim(default_str) then
!            if printrc == 1 then
!               print output_format, trim(label), trim(val_str), trim(DEFAULT_)
!            end if
!         else
!            print output_format, trim(label), trim(val_str), trim(NOT_DEFAULT)
!         end if 
!      else
!         print output_format, trim(label), trim(val_str), trim(NO_DEFAULT)
!      end if
!   end subroutine output_parameter
!
!#ifdef SET_VAL
!#  undef SET_VAL
!#endif
!
!#define SET_VAL(T, VAL) \
!type is (T) ;\
!   if (default_is_present .and. .not. label_is_present) then ;\
!      select type(default) ;\
!      type is(T) ;\
!         VAL = default ;\
!      class default ;\
!         _FAIL("Type of 'default' does not match type of 'VAL'.") ;\
!      end select ;\
!   else ;\
!      call ESMF_ConfigGetAttribute(config, VAL, label = actual_label, _RC) ;\
!   end if
!
!!=============================================================================
!
!#ifdef SET_VALS
!#  undef SET_VALS
!#endif
!
!#define SET_VALS(T, VALS) \
!type is (T) ;\
!   if (default_is_present .and. .not. label_is_present) then ;\
!      select type(default) ;\
!      type is(T) ;\
!         VALS = default ;\
!      class default ;\
!         _FAIL("Type of 'default' does not match type of 'VALS'.") ;\
!      end select ;\
!   else ;\
!      call ESMF_ConfigGetAttribute(config, valuelist = VALS, count = count, label = actual_label, _RC) ;\
!   end if
!
!!=============================================================================
!
!#ifdef SET_STRINGS
!#  undef SET_STRINGS
!#endif
!
!#define SET_STRINGS(T, TSTR, TFMT) \
!type is (T) ;\
!   type_str = TSTR ;\
!   val_str = intrinsic_to_string(val, TFMT) ;\
!   if (present(default)) then ;\
!      default_str = intrinsic_to_string(default, TFMT) ;\
!   end if
!
!!=============================================================================
