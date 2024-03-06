! vim:set ft=fortran:
#include "hconfig_macros.h"

   implicit none

contains

   subroutine GETFCT (hconfig, keystring, value, found, default, typestring, valuestring, rc)
      type(ESMF_HConfig), intent(in) :: hconfig
      character(len=*), intent(in) :: keystring
      logical, intent(out) :: found
      character(len=:), allocatable, optional, intent(out) :: typestring
      character(len=:), allocatable, optional, intent(out) :: valuestring
      integer, optional, intent(out) :: rc
      character(len=*), parameter :: DEFAULT_TAG = ' (default)'
      integer :: status
      logical :: value_equals_default
      character(len=MAXSTRLEN) :: raw
#if defined IS_ARRAY
      MTYPE, intent(out):: value(:)
      class(*), optional, intent(in) :: default(:)
      MTYPE, allocatable :: default_(:)
      character(len=*), parameter :: DELIMITER = ' '
      integer :: i, sz
#else
      MTYPE, intent(out) :: value
      class(*), optional, intent(in) :: default
      MTYPE, allocatable :: default_
#endif

      if(present(typestring)) typestring = TYPESTR

      if(present(default)) then
         select type(default)
         type is(VTYPE)
            default_ = default
         end select
      end if

      found = ESMF_HConfigIsDefined(hconfig, keyString=keystring, _RC)
      if(found) then
         value = ESMF_HCONFIG_AS(hconfig, keyString=keystring, _RC)
      else if(present(default)) then
         value = default_
      else
         _RETURN(_SUCCESS)
      end if

      if(.not. present(valuestring)) then
         _RETURN(_SUCCESS)
      end if

      if(.not. found) then
         value_equals_default = .TRUE.
      else if(.not. present(default)) then
         value_equals_default = .FALSE.
      else
         value_equals_default = PROPFCT(value == default_)
      end if

#if defined IS_ARRAY
      WRITE_STATEMENT(raw, status, value(1))
#else
      WRITE_STATEMENT(raw, status, this%value_ptr)
#endif
      _ASSERT(status == 0, 'Failed to write raw string')
      valuestring = trim(adjustl(raw))
#if defined IS_ARRAY
      do i = 2, size(value)
         WRITE_STATEMENT(raw, status, value(i))
         _ASSERT(status == 0, 'Failed to write raw string')
         valuestring = valuestring // DELIMITER // trim(adjustl(raw))
      end do
#endif

      if(value_equals_default) valuestring = valuestring // DEFAULT_TAG

      _RETURN(_SUCCESS)

   end subroutine GETFCT         
