! vim:ft=fortran
!#include "mapl3g_hconfig_get_value_declarations.h"
      type(HConfigParams), intent(inout) :: params
      character(len=:), allocatable, optional, intent(out) :: valuestring
      integer, optional, intent(out) :: rc
      integer :: status
      logical :: found = .FALSE.
      logical :: value_equals_default = .FALSE.
      character(len=:), allocatable :: valuestring_
      character(len=ESMF_MAXSTR) :: buffer
      character(len=:), allocatable :: fmtstr

      found = ESMF_HConfigIsDefined(params%hconfig, keyString=params%label, _RC)
      if(present(rc)) rc = merge(_SUCCESS, _FAILURE, params%check_value_set)
      params%value_set = .FALSE.
      if(.not. (found .or. present(default))) return
      ! At this point, either the label was found or default is present.
      
      value_equals_default = present(default) .and. .not. found
      if(found) then
         call get_hconfig(value, params, _RC)
      end if

      if(present(default)) then
         if(found) then
            value_equals_default = found .and. (are_equal(value, default))
         else
            value = default
         end if
      end if

      params%value_set = .TRUE.

      ! If there is no logger, can return now.
      _RETURN_UNLESS(params%has_logger() .or. present(valuestring))
      
      fmtstr = make_fmt(edit_descriptor)
      write(buffer, fmt=fmtstr, iostat=status) value
      _VERIFY(status)
#if defined ISARRAY
      valuestring_ = '[' // trim(buffer) // ']'
#else
      valuestring_ = trim(buffer)
#endif
      if(present(valuestring)) valuestring = valuestring_

      _RETURN_UNLESS(params%has_logger())
      call params%log_message(typestring, valuestring_, _RC)
      
      _RETURN(_SUCCESS)

