module regex_module
! http://fortranwiki.org/fortran/show/regex_module
!-------------------------------------------------------------------
! Fortran interface to POSIX regex, using ISO_C_BINDING.
!
! Regex is defined as an API using C headers. It does not define the
! exact value of flag tokens, just the names. It also uses an opaque
! data structure and a declared numeric type for the match array.
! Therefore, the code must either be generated for each target
! platform, or it use wrapper functions written in C.
!
! Fortran wrapper functions are also required to present a normal
! Fortran API, ant not require C conversions by the caller.
!
! The interface here is not strictly correct, because it does not
! explicitly convert Fortran strings to the C character kind. 
! Fortran only supports conversion of string kinds by assignment,
! or by a rather slow internal WRITE. For now, the easiest approach
! is to assume that C and Fortran default  character kinds are the
! same. This is generally true, but UTF-8 strings are likely to
! cause problems.
!-------------------------------------------------------------------
! API:
!
! Compile a regex into a regex object
!  subroutine regcomp(this,pattern,flags,status)
!    type(regex_type), intent(out) :: this           ! new regex object
!    character(len=*), intent(in) :: pattern         ! regex pattern string
!    character(len=*), intent(in), &
!                         optional :: flags ! flag characters:
!                                           ! x = extended regex (REG_EXTENDED)
!                                           ! m = multi-line     (REG_NEWLINE)
!                                           ! i = case-insensitive (REG_ICASE)
!                                           ! n = no MATCH required (REG_NOSUB)
!    integer, intent(out), optional :: status ! If absent, errors are fatal
!  end subroutine regcomp
!
! Execute a compiled regex against a string
!  function regexec(this,string,matches,flags,status) result(match)
!    logical :: match ! .TRUE. if the pattern matched
!    type(regex_type), intent(in) :: this ! regex object
!    character(len=*), intent(in) :: string ! target string
!    character(len=*), intent(in), &
!                     optional :: flags ! flag characters (for partial lines):
!                                       ! b = no beginning-of-line (REG_NOTBOL)
!                                       ! e = no end-of-line (REG_NOTEOL)
!    integer, intent(out), optional :: matches(:,:) ! match locations,
!                                                   ! dimension(2,nmatches)
!    integer, intent(out), optional :: status ! If absent, errors are fatal
!  end function
!
! Get the string message for a status error value
!  subroutine regerror(this,errcode,errmsg,errmsg_len)
!    type(regex_type), intent(in) :: this
!    integer, intent(in) :: errcode
!    character, intent(out) :: errmsg
!    integer, intent(out) :: errmsg_len
!    errmsg_len = C_regerror(int(errcode,C_int), this%preg, &
!                 errmsg, int(len(errmsg),C_size_t))
!  end subroutine regerror
!
! Release 
!  subroutine regfree(this)
!    type(regex_type), intent(inout) :: this
!  end subroutine regfree
!-------------------------------------------------------------------
! TODO:
! * More documentation.
! * Implement allocatable-length strings when commonly available.
! * Maybe store the matches array inside the regex_type structure?
!-------------------------------------------------------------------
  use ISO_C_Binding, only: C_ptr, C_int, C_size_t, C_char, &
                           C_NULL_char, C_NULL_ptr
  use ISO_Fortran_Env, only: ERROR_UNIT

  integer, parameter  :: NO_MATCH = -1
! Fortran regex structure holds a pointer to an opaque C structure
  type regex_type
    type(C_ptr) :: preg
  end type regex_type
  interface
    subroutine C_regalloc(preg_return) &
        bind(C,name="C_regalloc")
      import
      type(C_ptr), intent(out) :: preg_return
    end subroutine C_regalloc
    subroutine C_regcomp(preg,pattern,flags,status) &
        bind(C,name="C_regcomp")
      import
      type(C_ptr), intent(in), value :: preg
      character(len=1,kind=C_char), intent(in) :: pattern(*)
      character(len=1,kind=C_char), intent(in) :: flags(*)
      integer(C_int), intent(inout) :: status
    end subroutine C_regcomp
    subroutine C_regexec(preg,string,nmatch,matches,flags,status) &
        bind(C,name="C_regexec")
      import
      type(C_ptr), intent(in), value :: preg
      character(len=1,kind=C_char), intent(in) :: string(*)
      integer(C_int), intent(in), value :: nmatch
      integer(C_int), intent(out) :: matches(2,nmatch)
      character(len=1,kind=C_char), intent(in) :: flags(*)
      integer(C_int), intent(out) :: status
    end subroutine C_regexec
    function C_regerror(errcode, preg, errbuf, errbuf_size) &
        result(regerror) bind(C,name="regerror")
      import
      integer(C_size_t) :: regerror
      integer(C_int), value :: errcode
      type(C_ptr), intent(in), value :: preg
      character(len=1,kind=C_char), intent(out) :: errbuf
      integer(C_size_t), value :: errbuf_size
    end function C_regerror
    subroutine C_regfree(preg) bind(C,name="regfree")
      import
      type(C_ptr), intent(in), value :: preg
    end subroutine C_regfree
  end interface
contains
  subroutine regcomp(this,pattern,flags,status)
    type(regex_type), intent(out) :: this
    character(len=*), intent(in) :: pattern
    character(len=*), intent(in), optional :: flags
    integer, intent(out), optional :: status
! local
    integer(C_int) :: status_
    character(len=10,kind=C_char) :: flags_
! begin
    flags_=' '
    if (present(flags)) flags_=flags
    this%preg = C_NULL_ptr
    call C_regalloc(this%preg)
    call C_regcomp(this%preg, trim(pattern)//C_NULL_char, &
                   trim(flags)//C_NULL_char, status_)
    if (present(status)) then
      status=status_
!    else if (status_/=0) then
!ALT: STOP is not-a-good-idea
!      stop 'Regex runtime error: regcomp failed.'
    end if
  end subroutine regcomp
  logical function regexec(this,string,matches,flags,status) &
        result(match)
    type(regex_type), intent(in) :: this
    character(len=*), intent(in) :: string
    character(len=*), intent(in), optional :: flags
    integer, intent(out), optional :: matches(:,:)
    integer, intent(out), optional :: status
! local
    integer(C_int) :: status_, matches_(2,1)
    character(len=10,kind=C_char) :: flags_
! begin
    flags_=' '
    if (present(flags)) flags_=flags
!    write(*,*) 'calling C, nmatches=',size(matches,2)
    if (present(matches)) then
      matches = NO_MATCH   !! m.m. added to allow for the extension nmatch > 1
      call C_regexec(this%preg, trim(string)//C_NULL_char, &
                   size(matches,2),matches, &
                   trim(flags_)//C_NULL_char, status_)
    else
      call C_regexec(this%preg, trim(string)//C_NULL_char, &
                   int(0,C_int),matches_, &
                   trim(flags_)//C_NULL_char, status_)
    end if
    match = status_==0
    if (status_ == 1 ) status_ = 0 ! value "1" is not an error
   if (present(status)) then
      status=status_
!    else if (status_/=0.and.status_/=1) then
!ALT: STOP is not-a-good-idea
!      stop 'Regex runtime error: regexec failed.'
    end if
  end function regexec
  function regmatch(match,string,matches)
    integer, intent(in) :: match, matches(2,*)
    character(len=*), intent(in) :: string
    character(len=matches(2,match)-matches(1,match)) :: regmatch
    regmatch = string(matches(1,match)+1:matches(2,match))
  end function regmatch
  subroutine regerror(this,errcode,errmsg,errmsg_len)
    type(regex_type), intent(in) :: this
    integer, intent(in) :: errcode
    character, intent(out) :: errmsg
    integer, intent(out) :: errmsg_len
    errmsg_len = C_regerror(int(errcode,C_int), this%preg, &
                 errmsg, int(len(errmsg),C_size_t))
  end subroutine regerror
  subroutine regfree(this)
    type(regex_type), intent(inout) :: this
    call C_regfree(this%preg)
    this%preg = C_NULL_ptr
  end subroutine regfree
end module regex_module
