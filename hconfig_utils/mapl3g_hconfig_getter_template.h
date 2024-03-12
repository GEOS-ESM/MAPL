   subroutine SET_VALUE_PROCEDURE (this, value, default, rc)
      class(HConfigGetter), intent(inout) :: this
      VALTYPE, intent(out) :: value
      DEFTYPE, optional, intent(in) :: default
      integer, optional,intent(out) :: rc
      integer :: status

      this%typestring = TYPESTRING_

      if(this%found) then
         value = ESMF_HCONFIG_AS_PROCEDURE (this%hconfig, keyString=this%label, _RC)
         ! Do not set value to default. Compare only.
      end if
      if(present(default)) then
         call handle_default(default, value, this%value_equals_default, compare_only=this%found, _RC)
      _RETURN_UNLESS(this%do_log())
      call this%log_message(value, _RC)
      _RETURN(_SUCCESS)
      
   end subroutine SET_VALUE_PROCEDURE

   subroutine HANDLE_DEFAULT_PROCEDURE (default, value, are_equal, compare_only, rc)
      VALTYPE, intent(inout) :: value
      DEFTYPE, intent(in) :: default
      logical, intent(in) :: compare_only
      logical, intent(out) :: are_equal
      integer, optional, intent(out) :: rc
      integer :: status

      select type(default)
      type is (TYPE_)
         if(compare_only) then
            are_equal = REL_FCT(value, default)
            return
         end if
         ! Therefore compare_only is .FALSE.
         value = default
         ! So are_equal must be equal.
         are_equal = .TRUE.
      class default
         _FAIL('type unrecognized')
      end select
   end subroutine HANDLE_DEFAULT_PROCEDURE

   subroutine LOG_MESSAGE_PROCEDURE (this, value, rc, valuestring_out)
      VALTYPEIN, intent(in) :: value
      class(HConfigGetter), intent(inout) :: this
      integer, intent(out) :: rc
      character(len=:), allocatable :: valuestring
      character(len=:), allocatable, optional, intent(out) :: valuestring_out
      integer :: status

      allocate(character(len=MAXSTRLEN) :: valuestring)
      write(valuestring, fmt=this%formatstring, iostat=status) value
      _ASSERT(status == 0, 'Error writing valuestring')
      valuestring = trim(valuestring)
      if(this%value_equals_default) valuestring = valuestring // DEFAULT_VALUE_TAG
      call this%log_resource_message(valuestring, _RC)
      if(present(valuestring_out)) valuestring_out = valuestring
      _RETURN(_SUCCESS)
   end subroutine LOG_MESSAGE_PROCEDURE
