#include "mapl_hconfig_get_value_macros.h"

   subroutine _SUB_(hconfig, label, val, unusable, default, lgr, rc)  
      type(ESMF_HConfig), intent(in) :: hconfig
      character(len=*), intent(in) :: label
      _VTYPE_ , intent(inout) :: val _DIMS_
      class(KeywordEnforcer), optional, intent(in) :: unusable
      _DTYPE_ , optional, intent(in) :: default _DIMS_
      class(Logger), pointer, optional :: lgr
      integer, optional, intent(out) :: rc

      integer :: status
      class(Logger), pointer :: lgr_
      character(len=*), parameter :: EDIT_DESCRIPTOR = _EDIT_DESCRIPTOR_
      character(len=*), parameter :: TYPESTRING = _TYPESTRING_
      logical :: found, value_equals_default, value_is_unset
      character(len=:), allocatable :: valuestring
      character(len=ESMF_MAXSTR) :: buffer
      character(len=:), allocatable :: fmtstr
      character(len=:), allocatable :: message
      _DECL_NUM_ITEMS_

      if(present(lgr)) then
         lgr_ => lgr
      else
         lgr_ => logging%get_logger('MAPL') 
      end if
      message = 'The Logger is unknown.'
      _ASSERT(associated(lgr_),message)

      found = ESMF_HConfigIsDefined(hconfig, keyString=label, _RC)
      value_is_unset = .not. found
      value_equals_default = found
      message = 'Label "' // trim(label) // '" was not found.'
      _ASSERT(found .or. present(default), message)

      if(found) then
         val = _ESMF_FUNC_(hconfig, keyString=label, _RC)
      end if

      if(present(default)) value_equals_default = _COMPARE(val, default)
      if(value_is_unset) val = default

      fmtstr = make_fmt(EDIT_DESCRIPTOR)
      _SET_NUM_ITEMS(num_items, val)
      write(buffer, fmt=fmtstr, iostat=status) val _WRITE_DIMS_
      _VERIFY(status)
      valuestring = trim(buffer)
      _ADJUST_VALUESTRING(valuestring, val)
      if(value_equals_default) valuestring = valuestring // DEFAULT_TAG
      message = typestring //' '// trim(label) //' = '// valuestring
      _ASSERT(allocated(message), 'message has not been allocated.')
      _ASSERT(len(message) > 0, 'message is empty.')
      _ASSERT(len(message) <= 256, 'message is too long.')
!      call lgr_%info(message)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine _SUB_

#include "mapl_hconfig_get_value_macros_undef.h"

! vim:ft=fortran
