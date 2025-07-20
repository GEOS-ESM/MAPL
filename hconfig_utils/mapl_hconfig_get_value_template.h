#include "mapl_hconfig_get_value_macros.h"
   subroutine _SUB_(hconfig, label, _VAL_, _DEFAULT_, logger, rc)  
      type(ESMF_HConfig), intent(in) :: hconfig
      character(len=*), intent(in) :: label
      _FTYPE_, intent(inout) :: _VALARG_
      _FTYPE_, optional, intent(in) :: _DEFARG_
      class(Logger_t), pointer, optional, intent(in) :: logger
      integer, optional, intent(out) :: rc

      integer :: status
      class(Logger_t), pointer :: logger_
      character(len=*), parameter :: EDIT_DESCRIPTOR = _QUOTE(_ED_)
      character(len=*), parameter :: TYPESTRING = _TYPESTRING_
      logical :: found, value_equals_default
      character(len=:), allocatable :: valuestring
      character(len=ESMF_MAXSTR) :: buffer
      character(len=:), allocatable :: fmtstr
      integer :: num_items
      character(len=:), allocatable :: message

      found = .FALSE.
      value_equals_default = .TRUE.
      logger_ => null()
      if(present(logger)) then
         logger_ => logger
      else
         logger_ => null() !wdb fixme deleteme This needs to be replaced with the MAPL Logger
      end if
      message = 'The Logger is unknown.'
      _ASSERT(associated(logger_),message)

      found = ESMF_HConfigIsDefined(hconfig, keyString=label, _RC)
      message = 'Label "' // trim(label) // '" was not found.'
      _ASSERT(found .or. present(_DEFAULT_), message)

      if(present(_DEFAULT_)) _VAL_ = _DEFAULT_
      if(found) then
         _VAL_ = _ESMF_FUNC_(hconfig, keyString=label, _RC)
         value_equals_default = are_equal(_VAL_, _DEFAULT_)
      end if

      fmtstr = make_fmt(EDIT_DESCRIPTOR)
#if defined _ARRAY_
      num_items = min(size(_VAL_), MAX_NUM_ITEMS_OUTPUT)
      write(buffer, fmt=fmtstr, iostat=status) _VAL_(1:num_items)
      _VERIFY(status)
      valuestring = trim(buffer)
      if(size(_VAL_) > num_items) valuestring = valuestring // ELLIPSIS
      valuestring = '[' // valuestring // ']'
#else
      num_items = 0
      write(buffer, fmt=fmtstr, iostat=status) _VAL_
      _VERIFY(status)
      valuestring = trim(buffer)
#endif
      if(value_equals_default) valuestring = valuestring // DEFAULT_TAG
      message = typestring //' '// trim(label) //' = '// valuestring
      call logger_%info(message)

      _RETURN(_SUCCESS)

   end subroutine _SUB_
#include "mapl_hconfig_get_value_macros_undef.h"
! vim:ft=fortran
