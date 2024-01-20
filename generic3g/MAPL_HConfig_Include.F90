#if (T_ == logical)
#define TYPE_SIG T_
#define TYPE_NAME Logical

#elif (T_ == character)
#define TYPE_SIG T_(len=KL_)
#define TYPE_NAME String

#else
#if (T_ == real)
#define LETTER_ R

#else
#define LETTER_ I

#endif

#define TYPE_SIG T_(kind=ESMF_KIND_LETTER_KL_)
#define TYPE_NAME RKL_

#endif

#if defined(SEQ) 
#define BOUNDS_ (:)
#define _SEQ_ Seq

#else
#define BOUNDS_
#define _SEQ_

#endif

subroutine hconfig_get_TYPE_NAME_SEQ_(hconfig, keystring, value, unusable, default, asString, found, rc)
    type(ESMF_HConfig), intent(inout) :: hconfig
    character(len=*), intent(in) :: keystring
    TYPE_SIG, intent(out) :: value BOUNDS_
    class(KeywordEnforcer), optional, intent(in) :: unusable
    TYPE_SIG, optional, intent(in) :: default BOUNDS_
    character(len=*), optional, intent(inout) :: asString
    logical, optional, intent(out) :: found
    integer, optional, intent(out) :: rc

    integer :: status

    if(hconfig_get_i8_simple(hconfig, keystring, value, rc=status)) then
       if(present(asString)) then
          asString = ESMF_HConfigAsString(hconfig, keystring=keystring, _RC)
       end if
       if(present(found)) found = .TRUE.
       _RETURN(_SUCCESS)
    end if

      _ASSERT(present(default), 'Keystring <'//trim(keystring)//'> not found in hconfig')

      value = default
      _UNUSED_DUMMY(unusable)
      _RETURN(_SUCCESS)

end subroutine hconfig_get_TYPE_NAME_SEQ_
