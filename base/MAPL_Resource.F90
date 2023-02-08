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

!=============================================================================


module MAPL_ResourceMod

   !BOP
   ! !MODULE: MAPL_ResourceMod
   !
   ! !DESCRIPTION:  MAPL\_ResourceMod ...

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
   subroutine MAPL_GetResource_config_scalar(config, val, label, unusable, default, component_name, rc)
      type(ESMF_Config), intent(inout) :: config
      class(*), intent(inout) :: val
      character(len=*), intent(in) :: label
      class(KeywordEnforcer), optional, intent(in) :: unusable
      class(*), optional, intent(in) :: default
      character(len=*), optional, intent(in) :: component_name
      integer, optional, intent(out) :: rc

      integer :: status, printrc
      logical :: default_is_present, label_is_present
      character(len=:), allocatable :: label_to_print
      character(len=:), allocatable :: actual_label

      _UNUSED_DUMMY(unusable)

      default_is_present = present(default)

      if (default_is_present) then
         _ASSERT(same_type_as(val, default), "Value and default must have same type")
      end if

      call get_actual_label(config, label, label_is_present, actual_label, component_name = component_name, _RC)

      ! No default and not in config, error
      ! label or default must be present
      if (.not. label_is_present .and. .not. default_is_present) then
         if (present(rc)) rc = ESMF_FAILURE
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
         _FAIL( "Unupported type")
      end select

      call ESMF_ConfigGetAttribute(config, printrc, label = 'PRINTRC:', default = 0, _RC)

      ! Can set printrc to negative to not print at all
      if (MAPL_AM_I_Root() .and. printrc >= 0) then
         if (label_is_present) then
            label_to_print = actual_label
         else
            label_to_print = trim(label)
         end if
         call print_resource(printrc, label_to_print, val, default=default, _RC)
      end if

      _RETURN(ESMF_SUCCESS)

   end subroutine MAPL_GetResource_config_scalar

   ! Find value of array variable in config
   subroutine MAPL_GetResource_config_array(config, vals, label, unusable, default, component_name, rc)
      type(ESMF_Config), intent(inout) :: config
      character(len=*), intent(in) :: label
      class(*), intent(inout) :: vals(:)
      class(KeywordEnforcer), optional, intent(in) :: unusable
      class(*), optional, intent(in) :: default(:)
      character(len=*), optional, intent(in) :: component_name
      integer, optional, intent(out) :: rc

      character(len=:), allocatable :: actual_label
      integer :: status, count
      logical :: label_is_present, default_is_present

      _UNUSED_DUMMY(unusable)

      default_is_present = present(default)

      if (default_is_present) then
         _ASSERT(same_type_as(vals, default), "Value and default must have same type")
      end if

      _ASSERT(present(component_name), "Component name is necessary but not present.")
      call get_actual_label(config, label, label_is_present, actual_label, component_name = component_name, _RC)

      ! No default and not in config, error
      ! label or default must be present
      if (.not. label_is_present .and. .not. default_is_present) then
         if (present(rc)) rc = ESMF_FAILURE
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

      _RETURN(ESMF_SUCCESS)

   end subroutine MAPL_GetResource_config_array

   subroutine print_resource(printrc, label, val, default, rc)
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
         _RETURN(_SUCCESS)
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

   end subroutine print_resource

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

end module MAPL_ResourceMod
!   call ESMF_ConfigGetAttribute(config, VAL, label = actual_label, rc = status)
!   if (status /= ESMF_SUCCESS) then 
!      write(*,*) "label '" // actual_label // "' not found."
!   end if
