#if defined(MAXPATHLEN)
#undef MAXPATHLEN
#endif
#define MAXPATHLEN 1024

#if defined(SUCCESS)
#undef SUCCESS
#endif
#define SUCCESS 0

#if defined(FAILURE)
#undef FAILURE
#endif
#define FAILURE SUCCESS-1

#define FMTAI '(A,1X,I2)'

module udunits2mod

   use iso_c_binding, only: c_ptr, c_loc, c_associated, c_null_ptr, c_null_char, c_char, c_int, c_float, c_double, c_size_t, c_f_pointer

   implicit none

   public :: Converter
   public :: get_converter

   !private
!=========================== PARAMETERS (CONSTANTS) ============================
   character(len=*), parameter :: EMPTY_STRING = ''

!================================ ENUMERATORS ==================================
   include 'udunits2enumerators.h'
   integer(ut_encoding), parameter :: UT_ENCODING_DEFAULT = UT_ASCII

!================================ C INTERFACES =================================
   include "udunits2interfaces.h"

   type, abstract :: CptrWrapper
      private
      type(c_ptr) :: cptr = c_null_ptr
      logical :: is_set_ = .FALSE.
   contains
      procedure, public, pass(this) :: get => get_cptr
      procedure, public, pass(this) :: is_set => cptr_is_set
      procedure, public, pass(this) :: shutdown => shutdown_cptr_wrapper
      procedure, private, pass(this) :: set => set_cptr
      procedure(WrapperSub), private, deferred, pass(this) :: free_space
   end type CptrWrapper

   abstract interface

      subroutine WrapperSub(this)
         import :: CptrWrapper
         class(CptrWrapper), intent(in) :: this
      end subroutine WrapperSub

   end interface

!================================= CONVERTER ===================================
   type, extends(CptrWrapper) :: Converter
   contains
      procedure, public, pass(this) :: free_space => free_cv_converter
      procedure, public, pass(this) :: convert_double
      procedure, public, pass(this) :: convert_float
      procedure, public, pass(this) :: convert_doubles
      procedure, public, pass(this) :: convert_floats
   end type Converter

   interface Converter
      module procedure :: construct_converter
   end interface Converter

!=============================== SYSTEMWRAPPER =================================
   type, extends(CptrWrapper) :: SystemWrapper
   contains
      procedure, public, pass(this) :: free_space => free_ut_system
   end type SystemWrapper

   interface SystemWrapper
      module procedure :: construct_system
   end interface SystemWrapper

!=================================== UTUNIT ====================================
   type, extends(CptrWrapper) :: UnitWrapper
   contains
      procedure, public, pass(this) :: free_space => free_ut_unit
   end type UnitWrapper

   interface UnitWrapper
      module procedure :: construct_unit
   end interface UnitWrapper

!============================= INSTANCE VARIABLES ==============================
   type(SystemWrapper) :: SYSTEM_INSTANCE

contains

!================================= PROCEDURES ==================================

   type(c_ptr) function get_cptr(this)
      class(CptrWrapper), intent(in) :: this

      get_cptr = this % cptr

   end function get_cptr

   logical function cptr_is_set(this)
      class(CptrWrapper), intent(in) :: this
         
      cptr_is_set = this % is_set_

   end function cptr_is_set

   subroutine set_cptr(this, cptr)
      class(CptrWrapper), intent(inout) :: this
      type(c_ptr), optional, intent(in) :: cptr

      if(present(cptr)) then
         this % cptr = cptr
         this % is_set_ = .TRUE.
      else
         this % cptr = c_null_ptr
         this % is_set_ = .FALSE.
      end if

   end subroutine set_cptr

   subroutine shutdown_cptr_wrapper(this)
      class(CptrWrapper), intent(inout) :: this

      if(this % is_set()) call this % free_space()
      call this % set()

   end subroutine shutdown_cptr_wrapper

   function construct_converter(from_unit, to_unit) result(converter)
      type(Converter) :: converter
      type(UnitWrapper), intent(in) :: from_unit
      type(UnitWrapper), intent(in) :: to_unit
      type(c_ptr) :: cvconverter
      logical :: convertible
      integer(ut_status) :: status

      call converter % set() 
      if(.not. from_unit % is_set()) return
      if(.not. to_unit % is_set()) return

      call are_convertible(from_unit, to_unit, convertible, rc=status)
      status = ut_get_status()
      if(.not. utsuccess(status)) return
      if(.not. convertible) return

      cvconverter = c_null_ptr
      cvconverter = ut_get_converter(from_unit % get(), to_unit % get())
      status = ut_get_status()

      if(utsuccess(status)) then
         call converter % set(cvconverter)
      else
         if(c_associated(cvconverter)) call cv_free(cvconverter)
      end if

   end function construct_converter

   function construct_system(path) result(wrapper)
      type(SystemWrapper) :: wrapper
      character(len=*), optional, intent(in) :: path
      type(c_ptr) :: utsystem
      integer(ut_status) :: status

      call read_xml(path, utsystem, rc = status)
      if(utsuccess(status)) then
         call wrapper % set(utsystem)   
      else
         if(c_associated(utsystem)) call ut_free_system(utsystem)
         call wrapper % set()
      end if

   end function construct_system

   function construct_unit(syswrapper, identifier, encoding) result(wrapper)
      type(UnitWrapper) :: wrapper
      class(SystemWrapper), intent(in) :: syswrapper
      character(len=*), intent(in) :: identifier
      integer(ut_encoding), optional, intent(in) :: encoding
      character(kind=c_char, len=MAXPATHLEN) :: identifier_
      integer(ut_encoding)  :: encoding_ = UT_ENCODING_DEFAULT
      integer(ut_status) :: status
      type(c_ptr) :: utunit

      identifier_ = cstring(adjustl(identifier))
      if(present(encoding)) encoding_ = encoding
      utunit = ut_parse(syswrapper % get(), trim(identifier_), encoding_)
      status = ut_get_status()

      if(utsuccess(status)) then
         call wrapper % set(utunit)
      else
         if(c_associated(utunit)) call ut_free(utunit)
         call wrapper % set()
      end if

   end function construct_unit

   subroutine get_converter(conv, from, to, path, encoding, rc)
      type(Converter), intent(inout) :: conv
      character(len=*), intent(in) :: from, to
      character(len=*), optional, intent(in) :: path
      integer(ut_encoding), optional, intent(in) :: encoding
      integer(ut_status), optional, intent(out) :: rc
      integer(ut_status) :: status

      conv = get_converter_function(from, to, path, encoding)
      
      if(conv % is_set()) then
         status = UT_SUCCESS
      else
         status = FAILURE
      end if

      if(present(rc)) rc = status

   end subroutine get_converter

   function get_converter_function(from, to, path, encoding) result(conv)
      type(Converter) :: conv
      character(len=*), intent(in) :: from, to
      character(len=*), optional, intent(in) :: path
      integer(ut_encoding), optional, intent(in) :: encoding
      type(UnitWrapper) :: from_unit
      type(UnitWrapper) :: to_unit

      call conv % set()
      ! wdb Replace with initializer
      call initialize_system(SYSTEM_INSTANCE, path)
      if(.not. SYSTEM_INSTANCE % is_set()) return

      from_unit = UnitWrapper(SYSTEM_INSTANCE, from, encoding)
      to_unit = UnitWrapper(SYSTEM_INSTANCE, to, encoding)

      if(from_unit % is_set() .and. to_unit % is_set()) conv = Converter(from_unit, to_unit)

      call from_unit % shutdown()
      call to_unit % shutdown()

   end function get_converter_function

   function convert_double(this, from) result(to)
      class(Converter), intent(in) :: this
      real(c_double), intent(in) :: from
      real(c_double) :: to
      type(c_ptr) :: cv_converter

      cv_converter = this % get()
      to = cv_convert_double(cv_converter, from)

   end function convert_double

   function convert_float(this, from) result(to)
      class(Converter), intent(in) :: this
      real(c_float), intent(in) :: from
      real(c_float) :: to
      type(c_ptr) :: cv_converter

      cv_converter = this % get()
      to = cv_convert_float(cv_converter, from)

   end function convert_float

   subroutine convert_doubles(this, from, to)
      class(Converter), intent(in) :: this
      real(c_double), intent(in) :: from(:)
      real(c_double), intent(out) :: to(:)
      type(c_ptr) :: cv_converter

      cv_converter = this % get()
      call cv_convert_doubles(cv_converter, from, size(from), to)

   end subroutine convert_doubles

   subroutine convert_floats(this, from, to)
      class(Converter), intent(in) :: this
      real(c_float), intent(in) :: from(:)
      real(c_float), intent(out) :: to(:)
      type(c_ptr) :: cv_converter

      cv_converter = this % get()
      call cv_convert_floats(cv_converter, from, size(from), to)

   end subroutine convert_floats

   logical function utsuccess(utstatus)
      integer(ut_status) :: utstatus

      utsuccess = (utstatus == UT_SUCCESS)

   end function utsuccess

   subroutine read_xml(path, utsystem, rc)
      character(len=*), optional, intent(in) :: path
      type(c_ptr), intent(out) :: utsystem
      integer(ut_status), optional, intent(out) :: rc
      integer(ut_status) :: status
      character(kind=c_char, len=MAXPATHLEN) :: path_

      if(present(path)) then
         path_ = cstring(path)
         utsystem = ut_read_xml(path_)
      else
         utsystem = ut_read_xml_cptr(c_null_ptr)
      end if

      status = ut_get_status()
      if(present(rc)) rc = status

   end subroutine read_xml

   subroutine initialize_system(system, path)
      type(SystemWrapper), intent(inout) :: system
      character(len=*), optional, intent(in) :: path
      integer(ut_status) :: status
      type(c_ptr) :: utsystem

      if(system % is_set()) return
      call read_xml(path, utsystem, rc = status)
      if(.not. utsuccess(status)) then
         call ut_free_system(utsystem)
         return
      end if

      call system % set(utsystem)

   end subroutine initialize_system

   subroutine free_ut_system(this)
      class(SystemWrapper), intent(in) :: this
      type(c_ptr) :: cptr
        
      cptr = this % get()
      if(c_associated(cptr)) call ut_free_system(cptr)

   end subroutine free_ut_system

   subroutine free_ut_unit(this)
      class(UnitWrapper), intent(in) :: this
      type(c_ptr) :: cptr

      cptr = this % get()
      if(c_associated(cptr)) call ut_free(cptr)

   end subroutine free_ut_unit

   subroutine free_cv_converter(this)
      class(Converter), intent(in) :: this
      type(c_ptr) :: cptr 

      cptr = this % get()
      if(c_associated(cptr)) call cv_free(cptr)

   end subroutine free_cv_converter

   subroutine shutdown_system_instance()

      if(SYSTEM_INSTANCE % is_set()) call SYSTEM_INSTANCE % shutdown()

   end subroutine shutdown_system_instance

   subroutine are_convertible(unit1, unit2, convertible, rc)
      type(UnitWrapper), intent(in) :: unit1, unit2
      logical, intent(out) :: convertible
      integer, optional, intent(out) :: rc
      integer(ut_status) :: status
      integer(c_int), parameter :: ZERO = 0_c_int
      type(c_ptr) :: utunit1, utunit2
      
      utunit1 = unit1 % get()
      utunit2 = unit2 % get()
      convertible = (ut_are_convertible(utunit1, utunit2)  /= ZERO)
      status = ut_get_status()
      if(present(rc)) rc = status
   end subroutine are_convertible

   function cstring(s)
      character(len=*), intent(in) :: s
      character(kind=c_char, len=(len(s) + 1)) :: cstring

      cstring = s // c_null_char

   end function cstring

end module udunits2mod
!=================================== UNUSED ====================================

!subroutine set_cwrap_cptr(this, cptr)
!   class(Cwrap), intent(inout) :: this
!   type(c_ptr), intent(in) :: cptr

!   this % cptr_ = cptr

!end subroutine set_cwrap_cptr

!type(c_ptr) function get_cwrap_cptr(this)
!   class(Cwrap), intent(in) :: this

!   get_cwrap_cptr = this % cptr_

!end function get_cwrap_cptr


!=================================== CWRAP =====================================
!   type, abstract :: Cwrap
!      type(c_ptr) :: cptr_ = c_null_ptr
!   contains
!      procedure(cwrap_sub), public, pass(this), deferred :: destroy
!      procedure, public, pass(this) :: set => set_cwrap_cptr
!      procedure, public, pass(this) :: cptr => get_cwrap_cptr
!   end type Cwrap

!   interface
!       subroutine cwrap_sub(this)
!         import :: Cwrap
!         class(Cwrap), intent(inout) :: this
!      end subroutine cwrap_sub
!   end interface
!   logical function cwrap_is_null(this)
!      class(Cwrap), intent(in) :: this

!      cwrap_is_null = is_null(this % cptr())

!   end function cwrap_is_null

!   subroutine logical_to_integer(boolval)
!      logical, intent(in) :: boolval
!      integer, intent(inout) :: n

!      if(boolval) then
!         n = int(1, kind(n))
!      else
!         n = int(0, kind(n))
!      end if

!   end subroutine logical_to_integer

!   subroutine get_fstring(carray, fstring)
!      character(c_char), intent(in) :: carray(*)
!      character(len=*, kind=c_char), intent(out) :: fstring
!      integer :: i
!      character(c_char) :: ch

!      fstring = EMPTY_STRING
!      do i=1, len(fstring)
!         ch = carray(i)
!         if(ch == c_null_char) exit
!         fstring(i:i) = ch
!      end do

!   end subroutine get_fstring

!   function make_fstring(cptr) result(fstring)
!      interface
!         integer(c_size_t) function strlen(cptr) bind(c, name='strlen')
!            import :: c_ptr, c_size_t
!            type(c_ptr), value :: cptr
!         end function strlen
!      end interface
!      type(c_ptr), intent(in) :: cptr
!      character(len=:), allocatable :: fstring
!      character(len=:), pointer :: fptr
!      integer(c_size_t) :: clen

!      clen = strlen(cptr)
!      call c_f_pointer(cptr, fptr)
!      fstring = fptr(1:clen)

!   end function make_fstring

!   function get_ut_status_message(utstat) result(message)
!      integer(ut_status), intent(in) :: utstat
!      integer, parameter :: LL = 80
!      character(len=LL), parameter :: messages(16) = [character(len=LL) :: &
!         'UT_SUCCESS', & ! Success 
!         'UT_BAD_ARG', & ! An argument violates the function's contract 
!         'UT_EXISTS', & ! Unit, prefix, or identifier already exists 
!         'UT_NO_UNIT', & ! No such unit exists 
!         'UT_OS', & ! Operating-system error. See "errno". 
!         'UT_NOT_SAME_SYSTEM', & ! The units belong to different unit-systems 
!         'UT_MEANINGLESS', & ! The operation on the unit(s) is meaningless 
!         'UT_NO_SECOND', & ! The unit-system doesn't have a unit named "second" 
!         'UT_VISIT_ERROR', & ! An error occurred while visiting a unit 
!         'UT_CANT_FORMAT', & ! A unit can't be formatted in the desired manner 
!         'UT_SYNTAX', & ! string unit representation contains syntax error 
!         'UT_UNKNOWN', & ! string unit representation contains unknown word 
!         'UT_OPEN_ARG', & ! Can't open argument-specified unit database 
!         'UT_OPEN_ENV', & ! Can't open environment-specified unit database 
!         'UT_OPEN_DEFAULT', & ! Can't open installed, default, unit database 
!         'UT_PARSE_ERROR' ] ! Error parsing unit specification 
!      character(len=LL) :: message
!      integer :: message_index

!      message_index = utstat + 1

!      if(message_index < 1 .or. message_index > size(messages)) then
!         message = 'NOT FOUND'
!         return
!      end if

!      message = messages(message_index)

!   end function get_ut_status_message

!   function get_ut_status_message(utstat) result(message)
!      integer(ut_status), intent(in) :: utstat
!      integer, parameter :: LL = 80
!      character(len=LL) :: message

!      select case(utstat)
!         case(UT_SUCCESS)
!            message = 'UT_SUCCESS'
!         case(UT_BAD_ARG)
!            message = 'UT_BAD_ARG'
!         case(UT_EXISTS)
!            message = 'UT_EXISTS'
!         case(UT_NO_UNIT)
!            message = 'UT_NO_UNIT'
!         case(UT_OS)
!            message = 'UT_OS'
!         case(UT_NOT_SAME_SYSTEM)
!            message = 'UT_NOT_SAME_SYSTEM'
!         case(UT_MEANINGLESS)
!            message = 'UT_MEANINGLESS'
!         case(UT_NO_SECOND)
!            message = 'UT_NO_SECOND'
!         case(UT_VISIT_ERROR)
!            message = 'UT_VISIT_ERROR'
!         case(UT_CANT_FORMAT)
!            message = 'UT_CANT_FORMAT'
!         case(UT_SYNTAX)
!            message = 'UT_SYNTAX'
!         case(UT_UNKNOWN)
!            message = 'UT_UNKNOWN'
!         case(UT_OPEN_ARG)
!            message = 'UT_OPEN_ARG'
!         case(UT_OPEN_ENV)
!            message = 'UT_OPEN_ENV'
!         case(UT_OPEN_DEFAULT)
!            message = 'UT_OPEN_DEFAULT'
!         case(UT_PARSE_ERROR)
!            message = 'UT_PARSE_ERROR'
!         case default
!            message = '[UNKNOWN ERROR]'
!         end select

!   end function get_ut_status_message

!   function get_path_environment_variable(status) result(xmlpath)
!      integer, optional, intent(out) :: status
!      character(len=:), allocatable :: xmlpath
!      character(len=MAXPATHLEN) :: rawpath
!      character(len=*), parameter :: VARIABLE_NAME = 'UDUNITS2_XML_PATH'
!      integer, parameter :: ZERO_LENGTH = -2
!      ! These are the status codes for get_environment_variable:
!      ! -1: xmlpath is too short to contain value
!      !  0: environment variable does exist
!      !  1: environment variable does not exist
!      ! The status code is passed through, but if the length is 0, ZERO_LENGTH is returned.
!      integer :: length, status_

!      call get_environment_variable(name=VARIABLE_NAME, value=rawpath, length=length, status=status_)

!      xmlpath = EMPTY_STRING

!      if(length == 0) then
!         if(present(status)) status = ZERO_LENGTH
!         return
!      end if

!      if(status_ /= SUCCESS) then
!         if(present(status)) status = status_
!         return
!      endif

!      xmlpath = adjustl(rawpath)
!      if(present(status)) status = status_

!   end function get_path_environment_variable

!   type(c_ptr) function get_path_cptr(path)
!      character(len=*), optional, intent(in) :: path

!      get_path_cptr = c_null_ptr
!      if(present_nonempty(path)) get_path_cptr = character_cptr(path)

!   end function get_path_cptr

!   logical function present_nonempty(s)
!      character(len=*), optional, intent(in) :: s

!      present_nonempty = .FALSE.
!      if(present(s)) present_nonempty = (len_trim(s) > 0)

!   end function present_nonempty

!   type(c_ptr) function character_cptr(s, strip)
!      character(len=*), intent(in) :: s
!      logical, optional, intent(in) :: strip
!      character(kind=c_char, len=(len(s)+1)) :: scalar_char
!      logical :: do_strip

!      do_strip = merge(strip, .TRUE., present(strip))
!      character_cptr = c_null_ptr
!      if(do_strip) then
!         scalar_char = cstring(trim(adjustl((s))))
!      else
!         scalar_char = cstring(s)
!      end if

!      character_cptr = char_cptr(scalar_char)

!   end function character_cptr

!   type(c_ptr) function char_cptr(s)
!      character(kind=c_char), target, intent(in) :: s(*)

!      char_cptr = c_loc(s)

!   end function char_cptr

!   subroutine get_path_xml_path(path, xmlpath, rc)
!      character(len=*), intent(in) :: path
!      character(kind=c_char, len=MAXPATHLEN), intent(out) :: xmlpath
!      integer, optional, intent(out) :: rc
!      integer(ut_status) :: status
!      character(len=len(path)) :: path_
!      type(c_ptr) :: pathptr
!      integer(c_size_t) :: length

!      pathptr = ut_get_path_xml(path_, status)
!      length = strlen(path_)
!      if(length > MAXPATHLEN) then
!         xmlpath = EMPTY_STRING
!         if(present(rc)) rc = FAILURE
!      else
!         xmlpath = path_(1:length)
!         if(present(rc)) rc = status
!      end if

!   end subroutine get_path_xml_path

!   subroutine get_unit_path(pathin, path, status)
!      character(kind=c_char, len=*), optional, intent(in) :: pathin
!      character(kind=c_char, len=*), intent(out) :: path
!      integer(ut_status), optional, intent(out) :: status
!      integer(ut_status) :: status_
!      type(c_ptr) :: cptr

!      write(*, *)
!      if(present(pathin)) then
!         write(*, '(A)') 'get_unit_path: pathin in = "' // trim(pathin) // '"'
!         cptr = get_path_cptr(pathin)
!      else
!         write(*, '(A)') 'get_unit_path: no pathin in'
!         cptr = c_null_ptr
!      endif
!      path = ut_get_path_xml(cptr, status_)
!      if(present(status)) status = status_

!   end subroutine get_unit_path

!   type(c_ptr) function get_unit(system, identifier, encoding) result(utunit)
!      type(c_ptr), intent(in) :: system
!      character(len=*), intent(in) :: identifier
!      integer(ut_encoding), intent(in) :: encoding
!      character(kind=c_char, len=MAXPATHLEN) :: identifier_

!      identifier_ = cstring(trim(adjustl(identifier)))
!      utunit = ut_parse(system, identifier_, encoding)

!   end function get_unit

!   function ut_system_get_system(this) result(utsystem)
!      class(UT_System), intent(in) :: this
!      type(c_ptr) :: utsystem

!      if(this % has_system_set()) then
!         utsystem = this % utsystem
!      else
!         utsystem = c_null_ptr
!      end if

!   end function ut_system_get_system

!    subroutine set_ut_system(sw, utsystem)
!      type(UT_System), intent(inout) :: sw
!      type(c_ptr), optional, intent(in) :: utsystem

!      sw % previously_set = sw % has_system_set()

!      if(present(utsystem)) then
!         sw % utsystem = utsystem
!         sw % system_set = .TRUE.
!      else
!         sw % utsystem = c_null_ptr
!         sw % system_set = .FALSE.
!      end if

!   end subroutine set_ut_system

!   logical function ut_system_has_system_set(this)
!      class(UT_System), intent(in) :: this

!         ut_system_has_system_set = this % system_set

!   end function ut_system_has_system_set 

!   subroutine get_instance(instance, rc)
!      type(MAPL_Udunits_System), pointer, intent(out) :: instance
!      integer, optional, intent(out) :: rc
!      integer :: status

!      if(is_free(SYSTEM_INSTANCE)) then
!         instance => null()
!         status = FAILURE
!      else
!         instance => SYSTEM_INSTANCE
!         status = SUCCESS
!      end if

!      if(present(rc)) rc = status

!   end subroutine get_instance

!   type(c_ptr) function get_system_cptr() result(utsystem)

!      if(is_free(SYSTEM_INSTANCE)) then
!         utsystem = c_null_ptr
!      else
!         utsystem = SYSTEM_INSTANCE % cptr()
!      end if

!   end function get_system_cptr

!   subroutine free_utptr(utptr, utfreesub)
!      type(c_ptr), intent(inout) :: utptr
!      procedure(ut_ptr_sub) :: utfreesub

!      if(is_free(utptr)) return
!      call utfreesub(utptr)
!      utptr = c_null_ptr

!   end subroutine free_utptr

!   subroutine destroy_system(this)
!      class(MAPL_Udunits_System), intent(inout) :: this
!      type(c_ptr) :: utsystem 

!      utsystem = this % cptr()
!      write(*, *) 'c_associated(utsystem) ', c_associated(utsystem)
!      call free_ut_system(utsystem)
!      write(*, *) 'ut_system freed'
!      call this % set()
!      write(*, *) 'is_initialized: ', this % is_initialized()
!   end subroutine destroy_system

!   subroutine get(utsystem, rc, path)
!      type(c_ptr), intent(out)  :: utsystem
!      integer(ut_status), intent(out) :: rc
!      character(len=*), optional, intent(in) :: path
!
!      if(state % is_set()) then
!         utsystem = state % get()
!         rc = UT_SUCCESS
!      else
!         call initialize_state(state, utsystem, rc, path)
!      end if
!
!   end subroutine get
!   function construct_converter(cvconverter) result(conv)
!      type(c_ptr), intent(in) :: cvconverter
!      type(Converter) :: conv
!
!      call conv % set_cptr(cvconverter)
!
!   end function construct_converter

!   subroutine get_unit(system, identifier, encoding, utunit)
!      type(c_ptr), intent(in) :: system
!      character(len=*), intent(in) :: identifier
!      integer(ut_encoding), intent(in) :: encoding
!      type(c_ptr), intent(out) :: utunit
!      character(kind=c_char, len=MAXPATHLEN) :: identifier_
!
!      identifier_ = cstring(adjustl(identifier))
!      utunit = ut_parse(system, trim(identifier_), encoding)
!
!   end subroutine get_unit

!   subroutine initialize_ut_system(sw, path, rc)
!      type(UT_System), intent(inout) :: sw
!      character(len=*), optional, intent(in) :: path
!      integer, optional, intent(out) :: rc
!      integer :: status
!      integer(ut_status) :: utstatus
!      type(c_ptr) :: utsystem, previous
!      logical :: was_set
!
!      write(*, *) 'Entering initialize_ut_system'
!      was_set = sw % has_system_set()
!      if(was_set) then
!         previous = sw % get()
!         write(*, *) 'Reinitialize'
!      else
!         write(*, *) 'Initialize'
!         previous = c_null_ptr
!      end if
!
!      call read_xml(path, utsystem, rc=utstatus)
!      if(utsuccess(utstatus)) then
!         write(*, *) 'Got utsystem for UT_System'
!         call set_ut_system(sw, utsystem)
!         if(sw % has_system_set()) then
!            status = SUCCESS
!         else
!            if(was_set) sw % utsystem = previous
!            status = FAILURE
!         end if
!      else
!         write(*, *) 'Did not get utsystem for UT_System'
!         if(.not. was_set) call set_ut_system(sw)
!         status = FAILURE
!      end if
!
!      if(present(rc)) rc = status
!
!   end subroutine initialize_ut_system
!
!   subroutine initialize_ut_system(path, rc)
!      character(len=*), optional, intent(in) :: path
!      integer(ut_status), optional, intent(out) :: rc
!      integer(ut_status) :: status
!      type(c_ptr) :: utsystem, cptr
!      type(MAPL_Udunits_System), pointer :: instance
!
!      write(*, *) 'Entering initialize_ut_system.'
!      instance => SYSTEM_INSTANCE
!      if(instance % is_initialized()) then
!         write(*, *) 'Initialized'
!         status = UT_STATUS
!      else
!         write(*, *) 'Initializing'
!         call read_xml(path, utsystem, rc=status)
!         write(*, *) 'ut_status: ' // trim(get_ut_status_message(status)) // " ", status
!
!         if(utsuccess(status)) then
!            write(*, *) 'Setting instance ut_system'
!            call instance % set(utsystem)
!            write(*, *) 'is_initialized: ', instance % is_initialized()
!         else
!            write(*, *) 'Freeing utsystem'
!            call free_ut_system(utsystem)
!         end if
!      end if
!
!      if(present(rc)) rc = status
!
!   end subroutine initialize_ut_system

!   subroutine destroy_converter(this)
!      class(Converter), intent(inout) :: this
!
!      call free_cv_converter(this % get())
!      call this % set(c_null_ptr)
!
!   end subroutine destroy_converter
