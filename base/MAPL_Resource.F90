!------------------------------------------------------------------------------
!               Global Modeling and Assimilation Office (GMAO)                !
!                    Goddard Earth Observing System (GEOS)                    !
!                                 MAPL Component                              !
!------------------------------------------------------------------------------
!
#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
#include "unused_dummy.H"
<<<<<<< HEAD
!

=======
!=============================================================================
>>>>>>> main
!FPP macros for repeated (type-dependent) code

#ifdef IO_SUCCESS
#  undef IO_SUCCESS
#endif

#define IO_SUCCESS 0

!=============================================================================

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

#define MAKE_STRINGS(T, VAL, TSTR, SFMT) \
      if (label_is_present) then ;\
         if(default_is_present) then ;\
            select type(default) ;\
            type is(T) ;\
               value_is_default = are_equal(VAL, default) ;\
            class default ;\
               _FAIL(MISMATCH_MESSAGE) ;\
            end select ;\
         else ;\
            value_is_default = .FALSE. ;\
         end if ;\
      else ;\
         value_is_default = .TRUE. ;\
      end if ;\
      if (.not. (print_nondefault_only .and. value_is_default)) then ;\
         type_string = TSTR ;\
         type_format = SFMT ;\
         write(formatted_value, type_format, iostat=io_stat) VAL ;\
         _ASSERT((io_stat == IO_SUCCESS), 'Failure writing scalar formatted_value: ' // trim(actual_label)) ;\
      else ;\
         do_print = .FALSE. ;\
      end if

!=============================================================================

#ifdef MAKE_ARRAY_STRINGS
#  undef MAKE_ARRAY_STRINGS
#endif

#define MAKE_ARRAY_STRINGS(T, VAL, TSTR, SFMT) \
      if (label_is_present) then ;\
         if(default_is_present) then ;\
            select type(default) ;\
            type is(T) ;\
               value_is_default = all(are_equal(VAL, default)) ;\
            class default ;\
               _FAIL(MISMATCH_MESSAGE) ;\
            end select ;\
         else ;\
            value_is_default = .FALSE. ;\
         end if ;\
      else ;\
         value_is_default = .TRUE. ;\
      end if ;\
      if (.not. (print_nondefault_only .and. value_is_default)) then ;\
         type_string = TSTR ;\
         write(array_size_string, '(i2)', iostat=io_stat) size(VAL) ;\
         _ASSERT((io_stat == IO_SUCCESS), 'Failure writing array size string: ' // trim(actual_label)) ;\
         type_format = array_format(SFMT, array_size_string) ;\
         write(formatted_value, type_format, iostat=io_stat) VAL ;\
         _ASSERT((io_stat == IO_SUCCESS), 'Failure writing array formatted_value: ' // trim(actual_label)) ;\
      else ;\
         do_print = .FALSE. ;\
      end if

<<<<<<< HEAD
!------------------------------------------------------------------------------
!>
!### MODULE: `MAPL_ResourceMod`
!
! Author: GMAO SI-Team
!
! The module `MAPL_ResourceMod` provides subroutines get scalar and array
! resources from ESMF_Config objects.
!
=======
!=============================================================================

#ifdef ARE_EQUAL_FUNCTION
#  undef ARE_EQUAL_FUNCTION
#endif

#define ARE_EQUAL_FUNCTION(T) (a, b) result(res) ; T, intent(in) :: a, b ; logical :: res ; res = (a == b)

!=============================================================================
!END FPP macros for repeated (type-dependent) code
!=============================================================================

>>>>>>> main
module MAPL_ResourceMod

   use ESMF
   use ESMFL_Mod
   use gFTL2_StringVector
   use MAPL_CommsMod
   use MAPL_Constants, only: MAPL_CF_COMPONENT_SEPARATOR
   use MAPL_ExceptionHandling
   use MAPL_KeywordEnforcerMod
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64, int32, int64

<<<<<<< HEAD
=======
   ! !PUBLIC MEMBER FUNCTIONS:
>>>>>>> main
   implicit none
   private

   public MAPL_GetResource_config_scalar
   public MAPL_GetResource_config_array

   character(len=*), parameter :: MISMATCH_MESSAGE = "Type of 'default' does not match type of 'value'."

   character(len=*), parameter :: FMT_INT32 = '(i0.1)'
   character(len=*), parameter :: FMT_INT64 = '(i0.1)'
   character(len=*), parameter :: FMT_REAL32 = '(f0.6)'
   character(len=*), parameter :: FMT_REAL64 = '(f0.6)'
   character(len=*), parameter :: FMT_LOGICAL= '(l1)'

   character(len=*), parameter :: TYPE_INT32 = "'Integer*4 '" 
   character(len=*), parameter :: TYPE_INT64 = "'Integer*8 '" 
   character(len=*), parameter :: TYPE_REAL32 = "'Real*4 '"
   character(len=*), parameter :: TYPE_REAL64 = "'Real*8 '"
   character(len=*), parameter :: TYPE_LOGICAL =  "'Logical '"
   character(len=*), parameter :: TYPE_CHARACTER = "'Character '"

   interface are_equal
      module procedure :: are_equivalent
      module procedure :: are_equal_int32
      module procedure :: are_equal_int64
      module procedure :: are_equal_real32
      module procedure :: are_equal_real64
      module procedure :: are_equal_character
   end interface are_equal

contains
<<<<<<< HEAD
!--------------------------------------------------------------------------------
!>
! MAPL searches for labels with certain prefixes as well as just the label itself.
!
=======

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
>>>>>>> main
   pure function get_labels_with_prefix(label, component_name) result(labels_with_prefix)
      character(len=*),           intent(in) :: label
      character(len=*), optional, intent(in) :: component_name
      character(len=ESMF_MAXSTR)             :: component_type
      character(len=ESMF_MAXSTR)             :: labels_with_prefix(4)

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

!--------------------------------------------------------------------------------
!>
! If possible, find label or label with prefix. 
! Out: label found (logical) - version of label found
!
   subroutine get_actual_label(config, label, label_is_present, actual_label, unusable, component_name, rc)
      type(ESMF_Config),                intent(inout) :: config
      character(len=*),                 intent(in)    :: label
      logical,                          intent(out)   :: label_is_present
      character(len=:), allocatable,    intent(out)   :: actual_label
      class(KeywordEnforcer), optional, intent(in)    :: unusable
      character(len=*),       optional, intent(in)    :: component_name
      integer,                optional, intent(out)   :: rc

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

!--------------------------------------------------------------------------------
!>
! Find value of scalar variable in config.
!
   subroutine MAPL_GetResource_config_scalar(config, val, label, value_is_set, unusable, default, component_name, rc)
<<<<<<< HEAD
      type(ESMF_Config),                intent(inout) :: config
      class(*),                         intent(inout) :: val
      character(len=*),                 intent(in)    :: label
      logical,                          intent(out)   :: value_is_set
      class(KeywordEnforcer), optional, intent(in)    :: unusable
      class(*),               optional, intent(in)    :: default
      character(len=*),       optional, intent(in)    :: component_name
      integer,                optional, intent(out)   :: rc
=======
      type(ESMF_Config), intent(inout) :: config
      class(*), intent(inout) :: val
      character(len=*), intent(in) :: label
      logical, intent(out) :: value_is_set
      class(KeywordEnforcer), optional, intent(in) :: unusable
      class(*), optional, intent(in) :: default
      character(len=*), optional, intent(in) :: component_name
      integer, optional, intent(out) :: rc
>>>>>>> main

      character(len=:), allocatable :: actual_label
      character(len=:), allocatable :: type_format
      character(len=:), allocatable :: type_string
      character(len=ESMF_MAXSTR) :: formatted_value

      logical :: default_is_present
      logical :: label_is_present
      logical :: print_nondefault_only
      logical :: do_print
      logical :: value_is_default

      integer :: io_stat
      integer :: status

      _UNUSED_DUMMY(unusable)

      default_is_present = present(default)

      ! these need to be initialized explitictly 
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
      ! character value can't use the MAKE_STRINGS macro (formatted differently)
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

!--------------------------------------------------------------------------------
!>
! Find value of array variable in config.
!
   subroutine MAPL_GetResource_config_array(config, vals, label, value_is_set, unusable, default, component_name, rc)
<<<<<<< HEAD
      type(ESMF_Config),                intent(inout) :: config
      class(*),                         intent(inout) :: vals(:)
      character(len=*),                 intent(in)    :: label
      logical,                          intent(out)   :: value_is_set
      class(KeywordEnforcer), optional, intent(in)    :: unusable
      class(*),               optional, intent(in)    :: default(:)
      character(len=*),       optional, intent(in)    :: component_name
      integer,                optional, intent(out)   :: rc
=======
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
>>>>>>> main

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

      integer :: io_stat
      integer :: status

      _UNUSED_DUMMY(unusable)

      default_is_present = present(default)

      ! these need to be initialized explitictly 
      value_is_set = .FALSE.
      label_is_present = .FALSE.
      print_nondefault_only = .FALSE.
      do_print = .FALSE.
      value_is_default = .FALSE.

      if (default_is_present) then
         _ASSERT(same_type_as(vals, default), "Value and default must have same type")
      end if

      call get_actual_label(config, label, label_is_present, actual_label, component_name = component_name, _RC)

      ! label or default must be present
      if (.not. label_is_present .and. .not. default_is_present) then
         value_is_set = .FALSE.
         return
      end if

      ! only print if root
      call get_print_settings(config, default_is_present, do_print, print_nondefault_only, _RC)

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
            write(array_size_string, '(i2)', iostat=io_stat) size(vals)
            _ASSERT((io_stat == IO_SUCCESS), 'Failure writing array size string: ' // trim(actual_label))
            type_format = string_array_format(array_size_string)
            write(formatted_value, type_format, iostat=io_stat) vals
            _ASSERT((io_stat == IO_SUCCESS), 'Failure writing array formatted_value: ' // trim(actual_label))
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

<<<<<<< HEAD
!--------------------------------------------------------------------------------
!>
! Print the resource value according to the value of printrc:
!- `printrc = 0` - Only print non-default values
!- `printrc = 1` - Print all values
!
   subroutine print_resource(printrc, label, val, default, rc)
      integer,                    intent(in)  :: printrc
      character(len=*),           intent(in)  :: label
      class(*),                   intent(in)  :: val
      class(*),         optional, intent(in)  :: default
      integer,          optional, intent(out) :: rc
=======
   ! Print the resource value
   subroutine print_resource(type_string, label, formatted_value, value_is_default, rc)
      character(len=*), intent(in) :: type_string
      character(len=*), intent(in) :: label
      character(len=*), intent(in) :: formatted_value
      logical, intent(in) :: value_is_default
      integer, optional, intent(out) :: rc
>>>>>>> main

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

   ! Create array format string from scalar format string
   pure function array_format(scalar_format, array_size_string)
      character(len=*), intent(in) :: scalar_format
      character(len=*), intent(in) :: array_size_string
      character(len=:), allocatable :: array_format

      array_format = '('//trim(adjustl(array_size_string))//scalar_format(1:len_trim(scalar_format)-1)//',1X))'

   end function array_format

   ! Create format string for array of strings
   pure function string_array_format(array_size_string)
      character(len=*), intent(in) :: array_size_string
      character(len=:), allocatable :: string_array_format
      character(len=:), allocatable :: N

      ! N specifies the number of repeats in the format string.
      N = trim(adjustl(array_size_string))
      string_array_format = '('//N//'(""a"",1X))'

<<<<<<< HEAD
!--------------------------------------------------------------------------------
!>
! Convert val to string according to str_format.
!
   function intrinsic_to_string(val, str_format, rc) result(formatted_str)
      class(*),          intent(in)  :: val
      character(len=*),  intent(in)  :: str_format
      character(len=256)             :: formatted_str
      integer, optional, intent(out) :: rc
=======
   end function string_array_format
   
   ! Compare all the strings in two string arrays
   pure function compare_all(astrings, bstrings)
      character(len=*), dimension(:), intent(in) :: astrings
      character(len=*), dimension(:), intent(in) :: bstrings
      logical :: compare_all
>>>>>>> main

      integer :: i

      compare_all = (size(astrings) == size(bstrings))

      do i=1, size(astrings)
         if(.not. compare_all) exit
         compare_all = (trim(astrings(i)) == trim(bstrings(i)))
      end do
         
   end function compare_all

   ! Test if two logicals are equivalent
   ! Basically a wrapper function for use in are_equal generic function
   pure elemental function are_equivalent(a, b) result(res)
      logical, intent(in) :: a
      logical, intent(in) :: b
      logical :: res
      res = a .eqv. b
   end function are_equivalent

   ! These are specific functions for the are_equal generic function.
   ! Basically wrapper functions for the == binary relational operator 
   pure elemental function are_equal_int32 ARE_EQUAL_FUNCTION(integer(int32)) ; end function are_equal_int32
   pure elemental function are_equal_int64 ARE_EQUAL_FUNCTION(integer(int64)) ; end function are_equal_int64
   pure elemental function are_equal_real32 ARE_EQUAL_FUNCTION(real(real32)) ; end function are_equal_real32
   pure elemental function are_equal_real64 ARE_EQUAL_FUNCTION(real(real64)) ; end function are_equal_real64
   pure elemental function are_equal_character ARE_EQUAL_FUNCTION(character(len=*)) ; end function are_equal_character

end module MAPL_ResourceMod
