      type(HConfigParams), intent(inout) :: params
      character(len=:), allocatable, optional, intent(out) :: valuestring
      integer, optional, intent(out) :: rc
      integer :: status
      logical :: found = .FALSE.
      logical :: value_equals_default = .FALSE.
      character(len=:), allocatable :: valuestring_
      character(len=ESMF_MAXSTR) :: buffer
      character(len=:), allocatable :: fmtstr
      integer :: num_items

      found = ESMF_HConfigIsDefined(params%hconfig, keyString=params%label, _RC)
      if(present(rc)) rc = merge(_SUCCESS, _FAILURE, params%check_value_set)
      params%value_set = .FALSE.
      _RETURN_UNLESS(found .or. present(default))
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
#if defined ISARRAY
      num_items = min(size(value), MAX_NUM_ITEMS_OUTPUT)
      write(buffer, fmt=fmtstr, iostat=status) value(1:num_items)
      _VERIFY(status)
      valuestring_ = trim(buffer)
      if(size(value) > num_items) valuestring_ = valuestring_ // ELLIPSIS
      valuestring_ = '[' // valuestring_  // ']'
#else
      write(buffer, fmt=fmtstr, iostat=status) value
      _VERIFY(status)
      valuestring_ = trim(buffer)
#endif
      if(value_equals_default) valuestring_ = valuestring_ // DEFAULT_TAG
      if(present(valuestring)) valuestring = valuestring_

      _RETURN_UNLESS(params%has_logger())
      call params%log_message(typestring, valuestring_, _RC)
      
      _RETURN(_SUCCESS)

! vim:ft=fortran
