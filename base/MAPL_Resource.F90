#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
#include "unused_dummy.H"
!=============================================================================
! FPP macros

#if defined(IO_SUCCESS)
#undef IO_SUCCESS
#endif
#define IO_SUCCESS 0

#if defined(MISMATCH_MESSAGE)
#undef MISMATCH_MESSAGE
#endif
#define MISMATCH_MESSAGE "Type of 'default' does not match type of 'value'."

#if defined(TYPE_CHARACTER)
#undef TYPE_CHARACTER
#endif
#define TYPE_CHARACTER character(len=*)

#if defined(TYPE_INTEGER4)
#undef TYPE_INTEGER4
#endif
#define TYPE_INTEGER4 integer(int32)

#if defined(TYPE_INTEGER8)
#undef TYPE_INTEGER8
#endif
#define TYPE_INTEGER8 integer(int64)

#if defined(TYPE_REAL4)
#undef TYPE_REAL4
#endif
#define TYPE_REAL4 real(real32)

#if defined(TYPE_REAL8)
#undef TYPE_REAL8
#endif
#define TYPE_REAL8 real(real64)

#if defined(TYPE_LOGICAL)
#undef TYPE_LOGICAL
#endif
#define TYPE_LOGICAL logical

#if defined(TYPENUM_CHARACTER)
#undef TYPENUM_CHARACTER
#endif
#define TYPENUM_CHARACTER 0

#if defined(TYPENUM_INTEGER4)
#undef TYPENUM_INTEGER4
#endif
#define TYPENUM_INTEGER4 1

#if defined(TYPENUM_INTEGER8)
#undef TYPENUM_INTEGER8
#endif
#define TYPENUM_INTEGER8 2

#if defined(TYPENUM_REAL4)
#undef TYPENUM_REAL4
#endif
#define TYPENUM_REAL4 3

#if defined(TYPENUM_REAL8)
#undef TYPENUM_REAL8
#endif
#define TYPENUM_REAL8 4

#if defined(TYPENUM_LOGICAL)
#undef TYPENUM_LOGICAL
#endif
#define TYPENUM_LOGICAL 5

!=============================================================================
!END FPP macros
!=============================================================================
!>
!### Module `MAPL_ResourceMod`
!
! Author: GMAO SI-Team
!
! `MAPL_ResourceMod` provides subroutines get scalar and array
! resources from ESMF_Config objects.
!
module MAPL_ResourceMod

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

   enum, bind(c)
      enumerator :: MAPL_RESOURCE_VALUE_DEFAULT_MISMATCH = -1
      enumerator :: MAPL_RESOURCE_ARRAY_SIZE_FORMAT_CODE_FAILURE = -2
   end enum

   character(len=*), parameter :: EMPTY_STRING = ''
   integer, parameter :: MAX_LINE_LENGTH = 256

   public MAPL_GetResource_config_scalar
   public MAPL_GetResource_config_array
   public MAX_LINE_LENGTH

   interface array_format
      module procedure :: array_format_simple
      module procedure :: array_format_string
   end interface array_format

   character(len=*), parameter :: TYPE_STRING_CHARACTER = 'Character '
   character(len=*), parameter :: TYPE_STRING_INTEGER4 = 'Integer*4 '
   character(len=*), parameter :: TYPE_STRING_INTEGER8 = 'Integer*8 '
   character(len=*), parameter :: TYPE_STRING_REAL4 = 'Real*4 '
   character(len=*), parameter :: TYPE_STRING_REAL8 = 'Real*8 '
   character(len=*), parameter :: TYPE_STRING_LOGICAL = 'Logical '

   character(len=*), parameter :: CHARACTER_FMT = "(A)"
   character(len=*), parameter :: INTEGER_FMT = "(I0.1)"
   character(len=*), parameter :: REAL_FMT = "(F0.6)"
   character(len=*), parameter :: LOGICAL_FMT = "(L1)"
contains

   !>
   ! Set do_print & print_nondefault_only based on config and if default is present
   ! Print only (do_print) only if printrc is 0 or 1
   ! Print only nondefault values if printrc == 0 and if default is present
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
         call ESMF_ConfigGetAttribute(config, printrc, label = 'PRINTRC:', default = 0, __RC)
         do_print = (printrc == PRINT_ALL) .or. (printrc == PRINT_DIFFERENT)
         print_nondefault_only = (printrc == PRINT_DIFFERENT) .and. default_is_present
      else
         do_print = .FALSE.
      end if

   end subroutine get_print_settings

   !>
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

   !>
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

   !>
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
         labels_with_prefix = EMPTY_STRING
         labels_with_prefix(1) = label
      end if

   end function get_labels_with_prefix

   !>
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

      __UNUSED_DUMMY(unusable)

      label_is_present = .false.

      ! If component_name is present, find label in some form in config. Else search
      ! for exact label

      labels_to_try = get_labels_with_prefix(label, component_name)

      do i = 1, size(labels_to_try)
         actual_label = trim(labels_to_try(i))
         if (len_trim(actual_label) == 0 ) cycle
         call ESMF_ConfigFindLabel(config, label = actual_label, isPresent = label_is_present, __RC)
         if (label_is_present) exit
      end do

      if (.not. label_is_present) actual_label = trim(label)

      __RETURN(__SUCCESS)
   end subroutine get_actual_label

   !>
   ! Find value of scalar variable in config
   subroutine MAPL_GetResource_config_scalar(config, val, label, value_is_set, unusable, default, component_name, iunit, rc)
      type(ESMF_Config), intent(inout) :: config
      class(*), intent(inout) :: val
      character(len=*), intent(in) :: label
      logical, intent(out) :: value_is_set
      class(KeywordEnforcer), optional, intent(in) :: unusable
      class(*), optional, intent(in) :: default
      character(len=*), optional, intent(in) :: component_name
      character(len=*), optional, intent(inout) :: iunit
      integer, optional, intent(out) :: rc

      character(len=:), allocatable :: actual_label
      character(len=:), allocatable :: type_format
      character(len=:), allocatable :: type_string
      character(len=MAX_LINE_LENGTH) :: formatted_value

      logical :: default_is_present
      logical :: label_is_present
      logical :: print_nondefault_only
      logical :: do_print
      logical :: value_is_default

      integer :: io_stat
      integer :: status

      __UNUSED_DUMMY(unusable)

#if defined(IS_ARRAY)
#undef IS_ARRAY
#endif

#if defined(VALUE_)
#undef VALUE_
#endif

#define VALUE_ val

#if defined(TYPE_)
#undef TYPE_
#endif

#if defined(TYPENUM)
#undef TYPENUM
#endif

      default_is_present = present(default)

      ! these need to be initialized explicitly
      value_is_set = .FALSE.
      label_is_present = .FALSE.
      print_nondefault_only = .FALSE.
      do_print = .FALSE.
      value_is_default = .FALSE.

      if (default_is_present) then
         __ASSERT(same_type_as(val, default), "Value and default must have same type")
      end if

      call get_actual_label(config, label, label_is_present, actual_label, component_name = component_name, __RC)

      if(.not. (label_is_present .or. default_is_present)) then
         ! label or default must be present
         value_is_set = .FALSE.
         return
      end if

      call get_print_settings(config, default_is_present, do_print, print_nondefault_only, __RC)

      select type(val)

      type is (TYPE_INTEGER4)

#define TYPE_ TYPE_INTEGER4
#define TYPENUM TYPENUM_INTEGER4
#include "MAPL_Resource_SetValue.h"
#include "MAPL_Resource_MakeString.h"
#undef TYPE_
#undef TYPENUM


      type is (TYPE_INTEGER8)

#define TYPE_ TYPE_INTEGER8
#define TYPENUM TYPENUM_INTEGER8
#include "MAPL_Resource_SetValue.h"
#include "MAPL_Resource_MakeString.h"
#undef TYPE_
#undef TYPENUM


      type is (TYPE_REAL4)

#define TYPE_ TYPE_REAL4
#define TYPENUM TYPENUM_REAL4
#include "MAPL_Resource_SetValue.h"
#include "MAPL_Resource_MakeString.h"
#undef TYPE_
#undef TYPENUM


      type is (TYPE_REAL8)

#define TYPE_ TYPE_REAL8
#define TYPENUM TYPENUM_REAL8
#include "MAPL_Resource_SetValue.h"
#include "MAPL_Resource_MakeString.h"
#undef TYPE_
#undef TYPENUM


      type is (TYPE_LOGICAL)

#define TYPE_ TYPE_LOGICAL
#define TYPENUM TYPENUM_LOGICAL
#include "MAPL_Resource_SetValue.h"
#include "MAPL_Resource_MakeString.h"
#undef TYPE_
#undef TYPENUM


      type is (TYPE_CHARACTER)

#define TYPE_ TYPE_CHARACTER
#define TYPENUM TYPENUM_CHARACTER
#include "MAPL_Resource_SetValue.h"
#include "MAPL_Resource_MakeString.h"
#undef TYPE_
#undef TYPENUM

      class default
         __FAIL( "Unsupported type")
      end select

      if(do_print) then
         call print_resource(type_string, actual_label, formatted_value, value_is_default, iunit=iunit, __RC)
      end if

      __RETURN(ESMF_SUCCESS)

#undef TYPE_
#undef TYPENUM

   end subroutine MAPL_GetResource_config_scalar

   !>
   ! Find value of array variable in config
   subroutine MAPL_GetResource_config_array(config, val, label, value_is_set, unusable, default, component_name, iunit, rc)
      type(ESMF_Config), intent(inout) :: config
      class(*), intent(inout) :: val(:)
      character(len=*), intent(in) :: label

      logical, intent(out) :: value_is_set
      class(KeywordEnforcer), optional, intent(in) :: unusable
      class(*), optional, intent(in) :: default(:)
      character(len=*), optional, intent(in) :: component_name
      character(len=*), optional, intent(inout) :: iunit
      integer, optional, intent(out) :: rc
      ! We assume we will never have more than 99 values, hence len=2

      character(len=:), allocatable :: actual_label
      character(len=:), allocatable :: type_format
      character(len=:), allocatable :: type_string
      character(len=MAX_LINE_LENGTH) :: formatted_value

      logical :: default_is_present
      logical :: label_is_present
      logical :: print_nondefault_only
      logical :: do_print
      logical :: value_is_default
      integer :: count

      integer :: io_stat
      integer :: status

      __UNUSED_DUMMY(unusable)

#if defined(TYPE_)
#undef TYPE_
#endif

#if defined(TYPENUM)
#undef TYPENUM
#endif

#if defined(IS_ARRAY)
#undef IS_ARRAY
#endif
#define IS_ARRAY

#if defined(VALUE_)
#undef VALUE_
#endif
#define VALUE_ val

#if defined(MAX_CHAR_LEN)
#undef MAX_CHAR_LEN
#endif

      default_is_present = present(default)

      ! these need to be initialized explicitly
      value_is_set = .FALSE.
      label_is_present = .FALSE.
      print_nondefault_only = .FALSE.
      do_print = .FALSE.
      value_is_default = .FALSE.

      if (default_is_present) then
         __ASSERT(same_type_as(val, default), "Value and default must have same type")
      end if

      call get_actual_label(config, label, label_is_present, actual_label, component_name = component_name, __RC)

      ! label or default must be present
      if (.not. label_is_present .and. .not. default_is_present) then
         value_is_set = .FALSE.
         return
      end if

      ! only print if root
      call get_print_settings(config, default_is_present, do_print, print_nondefault_only, __RC)

      count = size(val)

      select type(val)

      type is (TYPE_INTEGER4)

#define TYPE_ TYPE_INTEGER4
#define TYPENUM TYPENUM_INTEGER4
#include "MAPL_Resource_SetValue.h"
#include "MAPL_Resource_MakeString.h"
#undef TYPE_
#undef TYPENUM


      type is (TYPE_INTEGER8)

#define TYPE_ TYPE_INTEGER8
#define TYPENUM TYPENUM_INTEGER8
#include "MAPL_Resource_SetValue.h"
#include "MAPL_Resource_MakeString.h"
#undef TYPE_
#undef TYPENUM


      type is (TYPE_REAL4)

#define TYPE_ TYPE_REAL4
#define TYPENUM TYPENUM_REAL4
#include "MAPL_Resource_SetValue.h"
#include "MAPL_Resource_MakeString.h"
#undef TYPE_
#undef TYPENUM


      type is (TYPE_REAL8)

#define TYPE_ TYPE_REAL8
#define TYPENUM TYPENUM_REAL8
#include "MAPL_Resource_SetValue.h"
#include "MAPL_Resource_MakeString.h"
#undef TYPE_
#undef TYPENUM


      type is (TYPE_LOGICAL)

#define TYPE_ TYPE_LOGICAL
#define TYPENUM TYPENUM_LOGICAL
#include "MAPL_Resource_SetValue.h"
#include "MAPL_Resource_MakeString.h"
#undef TYPE_
#undef TYPENUM


      type is (TYPE_CHARACTER)

#define TYPE_ TYPE_CHARACTER
#define TYPENUM TYPENUM_CHARACTER
#include "MAPL_Resource_SetValue.h"
#include "MAPL_Resource_MakeString.h"
#undef TYPE_
#undef TYPENUM


      class default
         __FAIL( "Unsupported type")
      end select

      if(do_print) then
         call print_resource(type_string, actual_label, formatted_value, value_is_default, iunit=iunit, __RC)
      end if

      __RETURN(ESMF_SUCCESS)

   end subroutine MAPL_GetResource_config_array


   !>
   ! Print the resource value
   subroutine print_resource(type_string, label, formatted_value, value_is_default, unusable, iunit, rc)
      character(len=*), intent(in) :: type_string
      character(len=*), intent(in) :: label
      character(len=*), intent(in) :: formatted_value
      logical, intent(in) :: value_is_default
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), optional, intent(out) :: iunit
      integer, optional, intent(out) :: rc

      character(len=*), parameter :: DEFAULT_ = ", (default value)"
      character(len=*), parameter :: NONDEFAULT_ = EMPTY_STRING
      character(len=*), parameter :: TRUNCATE = '... [VALUE_TRUNCATED]'
      integer, parameter :: LENGTH_TRUNCATE = len(TRUNCATE)
      character(len=:), allocatable :: output_format
      character(len=:), allocatable :: trailer
      character(len=:), allocatable :: value_out
      character(len=ESMF_MAXSTR) :: output_string
      character(len=MAX_LINE_LENGTH) :: final_output
      integer :: max_length_value_out

      __UNUSED_DUMMY(unusable)

      if(value_is_default) then
         trailer = DEFAULT_
      else
         trailer =  NONDEFAULT_
      end if

      ! maximum line length before adding the label and trailer
      max_length_value_out = MAX_LINE_LENGTH - len_trim(label) - len(trailer)

      value_out = trim(formatted_value)
      if(len(value_out) == 0) then
      ! if something went wrong, the formatted_value will be empty, so provide alternative value_out
         value_out = "[Empty formatted value]"
      else if(len(value_out) > max_length_value_out) then
         ! if value_out is too long (such that the output string will be longer than maxium line length, truncate
         value_out = value_out(1:(max_length_value_out - LENGTH_TRUNCATE)) // TRUNCATE
      end if

      ! Make output_string including label but without the trailer
      output_string = " " // type_string // "Resource Parameter: " // trim(label) // value_out

      ! Add the trailer now
      output_string = trim(output_string) // trailer

      ! Output a string no longer than maximum line length
      final_output = output_string(1:len(final_output))
      output_format = '(a)'

      if(present(iunit)) then
         iunit = EMPTY_STRING
         iunit = final_output(1:min(len(iunit), MAX_LINE_LENGTH))
      else
         write(*, fmt='(a)') trim(final_output)
      end if

      __RETURN(__SUCCESS)

   end subroutine print_resource

   !>
   ! Create array format string from scalar format string
   pure function array_format_string(scalar_format, array_size_string) result(array_format)
      character(len=*), intent(in) :: scalar_format
      character(len=*), intent(in) :: array_size_string
      character(len=:), allocatable :: array_format
      character(len=:), allocatable :: one_group
      integer :: lsf

      lsf = len_trim(scalar_format)
      one_group = scalar_format(2:(lsf-1))
      array_format = '('//trim(adjustl(array_size_string))// '(' // one_group//',1X))'
!      array_format = '('//trim(adjustl(array_size_string))//scalar_format(1:len_trim(scalar_format)-1)//',1X))'

   end function array_format_string

   !wdb fixme This should replace array_format_string, be renamed array_format, and delete interface array_format
   pure function array_format_simple(scalar_format) result(array_format)
      character(len=*), intent(in) :: scalar_format
      character(len=:), allocatable :: array_format
      character(len=:), allocatable :: base_format
      character(len=*), parameter :: UNLIMITED_FORMAT_ITEM = "*"

      base_format = scalar_format(2:(len_trim(scalar_format)-1))
      array_format = '(' // UNLIMITED_FORMAT_ITEM // '(' // base_format //', 1X))'

   end function array_format_simple

   !>
   ! Create format string for array of strings
   pure function string_array_format(array_size_string)
      character(len=*), intent(in) :: array_size_string
      character(len=:), allocatable :: string_array_format
      character(len=:), allocatable :: N

      ! N specifies the number of repeats in the format string.
      N = trim(adjustl(array_size_string))
      string_array_format = '('//N//'(""a"",1X))'

   end function string_array_format

   !>
   ! Compare all the strings in two string arrays
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

   function make_string_character(value_, slen) result(string)
      character(len=*), intent(in) :: value_
      integer, intent(in) :: slen
      character(len=slen) :: string
      integer :: last

      last = min(slen, len(value_))
      string = value_(1:last)

   end function make_string_character

   function make_string_character_array(value_, slen) result(string)
      character(len=*), intent(in) :: value_(:)
      integer, intent(in) :: slen
      character(len=slen) :: string
      character(len=:), allocatable :: raw
      integer :: i, last

      string = ''
      if(size(value_) == 0) return

      raw = EMPTY_STRING
      do i=1, size(value_)
         raw = raw // ' ' // trim(value_(i))
         if(len(raw) > slen) exit
      end do

      last = min(slen, len(raw))
      string = raw(1:last)

   end function make_string_character_array

end module MAPL_ResourceMod
