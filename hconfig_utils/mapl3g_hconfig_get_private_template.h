! vim:ft=fortran

   subroutine PRIVATE_GET_VALUE_ (hconfig, value, label, unusable, default, valueset, logger, rc)
      type(ESMF_HConfig), intent(in) :: hconfig
      class(*) VALTYPEDIMS intent(out) :: value
      character(len=*), intent(in) :: label
      class(KeywordEnforcer), optional, intent(in) :: unusable
      class(*) DEFTYPEDIMS optional, intent(in) :: default
      logical, optional, intent(out) :: valueset
      class(Logger_t), optional, target, intent(inout) :: logger
      integer, optional, intent(out) :: rc
      character(len=*), parameter :: fmt_ = '(' // FMT_ //')'
      integer :: status
      type(logger_t), pointer :: logger_ptr
      logical :: found = .FALSE.
      logical :: value_equals_default = .FALSE.
      character(len=*), allocatable :: typestring
      character(len=:), allocatable :: valuestring
      character(len=MAXSTRLEN) :: buffer

      if(present(default)) then
         _ASSERT(same_type_as(value, default), 'value and default are not the same type.')
      end if
      if(present(valueset)) valueset = .FALSE.
      found = ESMF_HConfigIsDefined(hconfig, keyString=label, _RC)
      if(.not. present(valueset)) status = _FAILURE
      if(present(rc)) rc = status
      if(.not. (found .or. present(default))) return
      ! At this point, either the label was found or default is present.
      
      select type(value)
      type is (integer(kind=ESMF_KIND_I4))
         typestring = 'I4'
         call get_by_type(hconfig, found, label, value, valuestring, value_equals_default, default=default, rc)
      class default
         _FAIL('unrecognized type') !wdb todo better message
      end select
      if(present(valueset)) valueset = .TRUE.
      ! If there is no logger, can return now.
      _RETURN_UNLESS(present(logger))
      call logger_ptr%info(typestring //' '// label //' = '// valuestring)
      
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine PRIVATE_GET_VALUE_
