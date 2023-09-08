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

#if defined(MAX_LINE_LENGTH)
#undef MAX_LINE_LENGTH
#endif
#define MAX_LINE_LENGTH 80

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

   public MAPL_GetResource_config_scalar
   public MAPL_GetResource_config_array

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
         call ESMF_ConfigGetAttribute(config, printrc, label = 'PRINTRC:', default = 0, _RC)  
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
         labels_with_prefix = ''
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

      _UNUSED_DUMMY(unusable)

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

#if defined(TYPE_NUM)
#undef TYPE_NUM
#endif

      default_is_present = present(default)

      ! these need to be initialized explicitly 
      value_is_set = .FALSE.
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

      call get_print_settings(config, default_is_present, do_print, print_nondefault_only, _RC)

      select type(val)

      type is (integer(int32))

#define TYPE_ integer(int32) 
#define TYPE_NUM 1
#include "MAPL_Resource_SetValue.h"
#include "MAPL_Resource_MakeString.h"
#undef TYPE_
#undef TYPE_NUM


      type is (integer(int64))

#define TYPE_ integer(int64) 
#define TYPE_NUM 2
#include "MAPL_Resource_SetValue.h"
#include "MAPL_Resource_MakeString.h"
#undef TYPE_
#undef TYPE_NUM


      type is (real(real32))

#define TYPE_ real(real32)
#define TYPE_NUM 3
#include "MAPL_Resource_SetValue.h"
#include "MAPL_Resource_MakeString.h"
#undef TYPE_
#undef TYPE_NUM


      type is (real(real64))

#define TYPE_ real(real64)
#define TYPE_NUM 4
#include "MAPL_Resource_SetValue.h"
#include "MAPL_Resource_MakeString.h"
#undef TYPE_
#undef TYPE_NUM


      type is (logical)

#define TYPE_ logical
#define TYPE_NUM 5
#include "MAPL_Resource_SetValue.h"
#include "MAPL_Resource_MakeString.h"
#undef TYPE_
#undef TYPE_NUM


      type is (character(len=*))

#define TYPE_ character(len=*)
#include "MAPL_Resource_SetValue.h"
#include "MAPL_Resource_MakeString.h"
#undef TYPE_
#undef TYPE_NUM

      class default
         _FAIL( "Unsupported type")
      end select
      
      if(do_print) then
         if(present(iunit)) then
            _ASSERT(len(iunit) <= MAX_LINE_LENGTH, 'iunit is too long (before: print_resource')
            call print_resource(type_string, actual_label, formatted_value, value_is_default, iunit=iunit, _RC)
            _ASSERT(len(iunit) <= MAX_LINE_LENGTH, 'iunit is too long (after: print_resource')
         else
            call print_resource(type_string, actual_label, formatted_value, value_is_default, _RC)
         endif
      end if

      value_is_set = .TRUE.

      _RETURN(ESMF_SUCCESS)

#undef TYPE_
#undef TYPE_NUM

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
      character(len=2) :: array_size_string
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

      _UNUSED_DUMMY(unusable)

#if defined(TYPE_)
#undef TYPE_
#endif

#if defined(TYPE_NUM)
#undef TYPE_NUM
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
         _ASSERT(same_type_as(val, default), "Value and default must have same type")
      end if

      call get_actual_label(config, label, label_is_present, actual_label, component_name = component_name, _RC)

      ! label or default must be present
      if (.not. label_is_present .and. .not. default_is_present) then
         value_is_set = .FALSE.
         return
      end if

      ! only print if root
      call get_print_settings(config, default_is_present, do_print, print_nondefault_only, _RC)

      count = size(val)

      select type(val)

      type is (integer(int32))

#define TYPE_ integer(int32) 
#define TYPE_NUM 1
#include "MAPL_Resource_SetValue.h"
#include "MAPL_Resource_MakeString.h"
#undef TYPE_
#undef TYPE_NUM


      type is (integer(int64))

#define TYPE_ integer(int64) 
#define TYPE_NUM 2
#include "MAPL_Resource_SetValue.h"
#include "MAPL_Resource_MakeString.h"
#undef TYPE_
#undef TYPE_NUM


      type is (real(real32))

#define TYPE_ real(real32)
#define TYPE_NUM 3
#include "MAPL_Resource_SetValue.h"
#include "MAPL_Resource_MakeString.h"
#undef TYPE_
#undef TYPE_NUM


      type is (real(real64))

#define TYPE_ real(real64)
#define TYPE_NUM 4
#include "MAPL_Resource_SetValue.h"
#include "MAPL_Resource_MakeString.h"
#undef TYPE_
#undef TYPE_NUM


      type is (logical)

#define TYPE_ logical
#define TYPE_NUM 5
#include "MAPL_Resource_SetValue.h"
#include "MAPL_Resource_MakeString.h"
#undef TYPE_
#undef TYPE_NUM


      type is (character(len=*))

#define TYPE_ character(len=*)
#define TYPE_NUM 0
#include "MAPL_Resource_SetValue.h"
#include "MAPL_Resource_MakeString.h"
#undef TYPE_
#undef TYPE_NUM


      class default
         _FAIL( "Unsupported type")
      end select

      if(do_print) then
         if(present(iunit)) then
            _ASSERT(len(iunit) <= MAX_LINE_LENGTH, 'iunit is too long (before: print_resource')
            call print_resource(type_string, actual_label, formatted_value, value_is_default, iunit=iunit, _RC)
            _ASSERT(len(iunit) <= MAX_LINE_LENGTH, 'iunit is too long (after: print_resource')
         else
            call print_resource(type_string, actual_label, formatted_value, value_is_default, _RC)
         endif
      end if

      value_is_set = .TRUE.

      _RETURN(ESMF_SUCCESS)

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
      character(len=*), parameter :: NONDEFAULT_ = ''
      character(len=*), parameter :: TRUNCATE = '... [VALUE_TRUNCATED]'
      integer, parameter :: LENGTH_TRUNCATE = len(TRUNCATE)
      character(len=:), allocatable :: output_format
      character(len=:), allocatable :: trailer
      character(len=:), allocatable :: value_out
      character(len=ESMF_MAXSTR) :: output_string
      character(len=MAX_LINE_LENGTH) :: final_output
      integer :: io_stat, max_length_value_out

      _UNUSED_DUMMY(unusable)

      trailer = NONDEFAULT_
      if (value_is_default) trailer = DEFAULT_

      ! maximum line length before adding the label and trailer
      max_length_value_out = MAX_LINE_LENGTH - len_trim(label) - len(trailer)

      ! format string to create output string
      output_format = "(1x, " // type_string // ", 'Resource Parameter: '" // ", a"// ", a)"

      value_out = trim(formatted_value)
      if(len(value_out) == 0) then
      ! if something went wrong, the formatted_value will be empty, so provide alternative value_out
         value_out = "[Unable to write formatted value]"
      else if(len(value_out) > max_length_value_out) then
         ! if value_out is too long (such that the output string will be longer than maxium line length, truncate
         value_out = value_out(1:(max_length_value_out - LENGTH_TRUNCATE)) // TRUNCATE
      end if

      ! Make output_string including label but without the trailer
      write(output_string, fmt=output_format, iostat=io_stat) trim(label), value_out

      ! If writing the output_string fails, provide alternative output_string
      if(io_stat /= IO_SUCCESS) output_string = trim(label) // ' [Unable to write resource value] '

      ! Add the trailer now
      output_string = trim(output_string) // trailer

      ! Output a string no longer than maximum line length
      final_output = output_string(1:len(final_output))
      output_format = '(a)'

      if(present(iunit)) then
         _ASSERT(len(iunit) <= MAX_LINE_LENGTH, 'iunit is too long (before)')
         write(iunit, fmt=output_format, iostat=io_stat) final_output(1:min(len(iunit), MAX_LINE_LENGTH))
         _ASSERT(io_stat == IO_SUCCESS, 'Failed writing the output string')
         _ASSERT(len(iunit) <= MAX_LINE_LENGTH, 'iunit is too long (after)')
      else
         write(*, fmt=output_format) final_output
      end if

      _RETURN(_SUCCESS)

   end subroutine print_resource

   !>
   ! Create array format string from scalar format string
   pure function array_format(scalar_format, array_size_string)
      character(len=*), intent(in) :: scalar_format
      character(len=*), intent(in) :: array_size_string
      character(len=:), allocatable :: array_format
      character(len=:), allocatable :: one_group
      integer :: lsf

      lsf = len_trim(scalar_format)
      one_group = scalar_format(2:(lsf-1))
      array_format = '('//trim(adjustl(array_size_string))// '(' // one_group//',1X))'
!      array_format = '('//trim(adjustl(array_size_string))//scalar_format(1:len_trim(scalar_format)-1)//',1X))'

   end function array_format

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

end module MAPL_ResourceMod
