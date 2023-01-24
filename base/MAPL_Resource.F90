#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

!=============================================================================


module MAPL_ResourceMod

   !BOP
   ! !MODULE: MAPL_ResourceMod
   !
   ! !DESCRIPTION:  MAPL\_ResourceMod ...

   ! !USES:

   use ESMF
   use ESMFL_Mod
   use gFTL_StringVector
   use MAPL_CommsMod
   use MAPL_Constants, only: MAPL_CF_COMPONENT_SEPARATOR
   use MAPL_ExceptionHandling
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64, int32, int64

   ! !PUBLIC MEMBER FUNCTIONS:

   implicit none
   private

   public MAPL_GetResource_config_scalar
   public MAPL_GetResource_config_array

contains

   ! MAPL searches for labels with certain prefixes as well as just the label itself
   pure function get_labels_with_prefix(compname, label) result(labels_with_prefix)
      character(len=*), intent(in) :: compname, label
      character(len=ESMF_MAXSTR) :: labels_with_prefix(4), component_type

      component_type = compname(index(compname, ":") + 1:)

      ! The order to search for labels in resource files
      labels_with_prefix(1) = trim(compname)//"_"//trim(label)
      labels_with_prefix(2) = trim(component_type)//"_"//trim(label)
      labels_with_prefix(3) = trim(label)
      labels_with_prefix(4) = trim(compname)//MAPL_CF_COMPONENT_SEPARATOR//trim(label)

   end function get_labels_with_prefix

   ! If possible, find label or label with prefix. Out: logical to if label found, 
   ! version of label found,
   subroutine get_label_to_use(config, label, compname, label_is_present, label_to_use, rc)
      type(ESMF_Config), intent(inout) :: config
      character(len=*), intent(in) :: label
      character(len=*), intent(in) :: compname
      logical, intent(out) :: label_is_present
      character(len=:), allocatable, intent(out) :: label_to_use
      integer, optional, intent(out) :: rc

      character(len=ESMF_MAXSTR), allocatable :: labels_to_try(:)
      integer :: i
      integer :: status

      label_is_present = .false.
      labels_to_try = get_labels_with_prefix(compname, label)

      do i = 1, size(labels_to_try)
         label_to_use = trim(labels_to_try(i))
         call ESMF_ConfigFindLabel(config, label = label_to_use, isPresent = label_is_present, _RC)

         if (label_is_present) then
            exit
         end if
      end do

      _RETURN(_SUCCESS)

   end subroutine get_label_to_use

   ! Find value of scalar variable in config
   subroutine MAPL_GetResource_config_scalar(config, val, label_to_find, default, compname, rc)
      type(ESMF_Config), intent(inout) :: config
      class(*), intent(inout) :: val
      character(len=*), intent(in) :: label_to_find
      class(*), optional, intent(in) :: default
      character(len=*), optional, intent(in) :: compname
      integer, optional, intent(out) :: rc

      integer :: status, printrc
      logical :: default_is_present, label_is_present
      character(len=:), allocatable :: label_to_print
      character(len=:), allocatable :: label

      default_is_present = present(default)

      if (default_is_present) then
         _ASSERT(same_type_as(val, default), "Value and default must have same type")
      end if

      ! If compname is present, find label in some form in config. Else search
      ! for exact label
      if (present(compname)) then
         call get_label_to_use(config, label_to_find, compname, label_is_present, label, _RC)
      else
         label = label_to_find
         call ESMF_ConfigFindLabel(config, label = label, isPresent = label_is_present, _RC)
      end if

      ! No default and not in config, error
      ! label or default must be present
      if (.not. label_is_present .and. .not. default_is_present) then
         if (present(rc)) rc = ESMF_FAILURE
         return
      end if

      select type(val)
      type is(integer(int32))
         if (default_is_present .and. .not. label_is_present) then
            select type(default)
            type is(integer(int32))
               val = default
            end select
         else
            call ESMF_ConfigGetAttribute(config, val, label = label, _RC)
         end if
      type is(integer(int64))
         if (default_is_present .and. .not. label_is_present) then
            select type(default)
            type is(integer(int64))
               val = default
            end select
         else
            call ESMF_ConfigGetAttribute(config, val, label = label, _RC)
         end if
      type is(real(real32))
         if (default_is_present .and. .not. label_is_present) then
            select type(default)
            type is(real(real32))
               val = default
            end select
         else
            call ESMF_ConfigGetAttribute(config, val, label = label, _RC)
         end if
      type is (real(real64))
         if (default_is_present .and. .not. label_is_present) then
            select type(default)
            type is(real(real64))
               val = default
            end select
         else
            call ESMF_ConfigGetAttribute(config, val, label = label, _RC)
         end if
      type is(character(len=*))
         if (default_is_present .and. .not. label_is_present) then
            select type(default)
            type is(character(len=*))
               val = trim(default)
            end select
         else
            call ESMF_ConfigGetAttribute(config, val, label = label, _RC)
         end if
      type is(logical)
         if (default_is_present .and. .not. label_is_present) then
            select type(default)
            type is(logical)
               val = default
            end select
         else
            call ESMF_ConfigGetAttribute(config, val, label = label, _RC)
         end if
         class default
         _FAIL( "Unupported type")
      end select

      call ESMF_ConfigGetAttribute(config, printrc, label = 'PRINTRC:', default = 0, _RC)

      ! Can set printrc to negative to not print at all
      if (MAPL_AM_I_Root() .and. printrc >= 0) then
         if (label_is_present) then
            label_to_print = label
         else
            label_to_print = trim(label_to_find)
         end if
         call print_resource(printrc, label_to_print, val, default=default, _RC)
      end if

      _RETURN(ESMF_SUCCESS)

   end subroutine MAPL_GetResource_config_scalar

   ! Find value of array variable in config
   subroutine MAPL_GetResource_config_array(config, compname, vals, label_to_find, default, rc)
      type(ESMF_Config), intent(inout) :: config
      character(len=*), intent(in) :: compname
      character(len=*), intent(in) :: label_to_find
      class(*), intent(inout) :: vals(:)
      class(*), optional, intent(in) :: default(:)
      integer, optional, intent(out) :: rc

      character(len=:), allocatable :: label_to_use
      integer :: status, count
      logical :: label_is_present, default_is_present

      default_is_present = present(default)

      if (default_is_present) then
         _ASSERT(same_type_as(vals, default), "Value and default must have same type")
      end if

      call get_label_to_use(config, label_to_find, compname, label_is_present, label_to_use, _RC)

      ! No default and not in config, error
      ! label or default must be present
      if (.not. label_is_present .and. .not. default_is_present) then
         if (present(rc)) rc = ESMF_FAILURE
         return
      end if

      count = size(vals)

      select type(vals)
      type is(integer(int32))
         if (default_is_present .and. .not. label_is_present) then
            select type(default)
            type is(integer(int32))
               if (.not. label_is_present) vals = default
            end select
         else
            call ESMF_ConfigGetAttribute(config, valuelist = vals, count = count, label = label_to_use, _RC)
         end if
      type is(integer(int64))
         if (default_is_present .and. .not. label_is_present) then
            select type(default)
            type is(integer(int64))
               vals = default
            end select
         else
            call ESMF_ConfigGetAttribute(config, valuelist = vals, count = count, label = label_to_use, _RC)
         end if
      type is(real(real32))
         if (default_is_present .and. .not. label_is_present) then
            select type(default)
            type is(integer(real32))
               vals = default
            end select
         else
            call ESMF_ConfigGetAttribute(config, valuelist = vals, count = count, label = label_to_use, _RC)
         end if
      type is (real(real64))
         if (default_is_present .and. .not. label_is_present) then
            select type(default)
            type is(integer(real64))
               vals = default
            end select
         else
            call ESMF_ConfigGetAttribute(config, valuelist = vals, count = count, label = label_to_use, _RC)
         end if
      type is(character(len=*))
         if (default_is_present .and. .not. label_is_present) then
            select type(default)
            type is(character(*))
               vals = default
            end select
         else
            call ESMF_ConfigGetAttribute(config, valuelist = vals, count = count, label = label_to_use, _RC)
         end if
      type is(logical)
         if (default_is_present .and. .not. label_is_present) then
            select type(default)
            type is(logical)
               vals = default
            end select
         else
            call ESMF_ConfigGetAttribute(config, valuelist = vals, count = count, label = label_to_use, _RC)
         end if
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
      type is(integer(int32))
         type_str = "'Integer*4 '"
         type_format = '(i0.1)'
         val_str = intrinsic_to_string(val, type_format)
         if (present(default)) then
            default_str = intrinsic_to_string(default, type_format)
         end if
      type is(integer(int64))
         type_str = "'Integer*8 '"
         type_format = '(i0.1)'
         val_str = intrinsic_to_string(val, type_format)
         if (present(default)) then
            default_str = intrinsic_to_string(default, type_format)
         end if
      type is(real(real32))
         type_str = "'Real*4 '"
         type_format = '(f0.6)'
         val_str = intrinsic_to_string(val, type_format)
         if (present(default)) then
            default_str = intrinsic_to_string(default, type_format)
         end if
      type is(real(real64))
         type_str = "'Real*8 '"
         type_format = '(f0.6)'
         val_str = intrinsic_to_string(val, type_format)
         if (present(default)) then
            default_str = intrinsic_to_string(default, type_format)
         end if
      type is(logical)
         type_str = "'Logical '"
         type_format = '(l1)'
         val_str = intrinsic_to_string(val, type_format)
         if (present(default)) then
            default_str = intrinsic_to_string(default, type_format)
         end if
      type is(character(len=*))
         type_str = "'Character '"
         val_str = trim(val)
         if (present(default)) then
            default_str = intrinsic_to_string(default, 'a')
         end if
         class default
         _FAIL("Unsupported type")
      end select

      output_format = "(1x, " // type_str // ", 'Resource Parameter: '" // ", a"// ", a)"

      ! printrc = 0 - Only print non-default values
      ! printrc = 1 - Print all values
      if (present(default)) then
         if (trim(val_str) /= trim(default_str) .or. printrc == 1) then
            print output_format, trim(label), trim(val_str)
         end if
      else
         print output_format, trim(label), trim(val_str)
      end if

   end subroutine print_resource

   logical function vector_contains_str(vector, string)
      type(StringVector), intent(in) :: vector
      character(len=*), intent(in) :: string
      type(StringVectorIterator) :: iter

      iter = vector%begin()

      vector_contains_str = .false.

      if (vector%size() /= 0) then
         do while (iter /= vector%end())
            if (trim(string) == iter%get()) then
               vector_contains_str = .true.
               return
            end if
            call iter%next()
         end do
      end if

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
