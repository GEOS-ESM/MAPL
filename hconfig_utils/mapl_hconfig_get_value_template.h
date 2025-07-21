#define _PO_ (
#define _PC_ )
#define _COLON_ :
#define _GROUP(A) _PO_ A _PC_
#define _DIMS_ _GROUP(_COLON_)

#ifndef _RELATION_
   #define _RELATION_ ==
#endif

! if ARRAY
!define _DECLARE(V) V _DIMS_
!define _COMPARE(V, R, D) all _GROUP(V R D) 
!define _NUM_ITEMS(V) min(size(val), MAX_NUM_ITEMS_OUTPUT)

!define _SET_VALUESTRING(S, B) S=trim(B)\
! if(size(val)>num_items) S=S//ELLIPSIS\
! S='['//S//']'

!define _WRITE_BUFFER(B, V) write(B, fmt=fmtstr, iostat=status) V(1:num_items)
! ELSE
!define _DECLARE(V) V
!define _COMPARE(V, R, D) _GROUP(V R D)
!define _NUM_ITEMS(V) 0
!define _SET_VALUESTRING(S, B) S=trim(B)
!define _WRITE_BUFFER(B, V) write(B, fmt=fmtstr, iostat=status) V
!end 

   subroutine _SUB_(hconfig, label, val, default, logger, rc)  
      type(ESMF_HConfig), intent(in) :: hconfig
      character(len=*), intent(in) :: label
#ifdef _ARRAY_
      _FTYPE_ , intent(inout) :: val(:)
      _FTYPE_ , optional, intent(in) :: default(:)
#else
      _FTYPE_ , intent(inout) :: val
      _FTYPE_ , optional, intent(in) :: default
#endif
      class(Logger_t), pointer, optional, intent(in) :: logger
      integer, optional, intent(out) :: rc

      integer :: status
      class(Logger_t), pointer :: logger_
      character(len=*), parameter :: EDIT_DESCRIPTOR = _EDIT_DESCRIPTOR_
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
      _ASSERT(found .or. present(default), message)

      if(present(default)) val = default
      if(found) then
         val = _ESMF_FUNC_(hconfig, keyString=label, _RC)

#ifdef _ARRAY_
         value_equals_default = all(val _RELATION_ default)
#else
         value_equals_default = val _RELATION_ default
#endif
      end if

      fmtstr = make_fmt(EDIT_DESCRIPTOR)
! num_items: min(size(val), MAX_NUM_ITEMS_OUTPUT) or 0 => _NUM_ITEMS_
! write variable val(1:num_items) or val => _WRITE_VAL_
! trim: yes or no 
! group symbols: '[', ']' or '', ''
#if defined _ARRAY_
      num_items = min(size(val), MAX_NUM_ITEMS_OUTPUT)
      write(buffer, fmt=fmtstr, iostat=status) val(1:num_items)
      _VERIFY(status)
      valuestring = trim(buffer)
      if(size(val) > num_items) valuestring = valuestring // ELLIPSIS
      valuestring = '[' // valuestring // ']'
#else
      num_items = 0
      write(buffer, fmt=fmtstr, iostat=status) val
      _VERIFY(status)
      valuestring = trim(buffer)
#endif
      if(value_equals_default) valuestring = valuestring // DEFAULT_TAG
      message = typestring //' '// trim(label) //' = '// valuestring
      call logger_%info(message)

      _RETURN(_SUCCESS)

   end subroutine _SUB_

#undef _SUB_
#undef _FTYPE_
#undef _ESMF_FUNC_
#undef _TYPESTRING_
#undef _EDIT_DESCRIPTOR_
#undef _RELATION_

#ifdef _ARRAY_
#undef _ARRAY_
#endif

! vim:ft=fortran
