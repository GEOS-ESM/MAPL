#if defined(TRIMALL)
#undef TRIMALL(S)
#endif
#define TRIMALL(S) trim(adjustl(S))

#if defined(MAXPATHLEN)
#undef MAXPATHLEN
#endif
#define MAXPATHLEN 1024

module udunits2mod

   use iso_c_binding, only: c_ptr, c_loc, c_associated, c_null_ptr, c_null_char, &
      c_char, c_int, c_float, c_double, c_size_t, c_f_pointer

   implicit none

   !private

   public :: MAPL_Udunits_Converter

!=========================== PARAMETERS (CONSTANTS) ============================
   character(len=*), parameter :: EMPTY_STRING = ''
!   integer, parameter :: MAXPATHLEN = 1024

!================================ ENUMERATORS ==================================
   include 'udunits2enumerators.h'
   integer(ut_encoding), parameter :: UT_ENCODING_DEFAULT = UT_ASCII

!================================ C INTERFACES =================================
   include "udunits2interfaces.h"

!=================================== CWRAP =====================================
   type, abstract :: Cwrap
      type(c_ptr) :: ptr = c_null_ptr
   contains
      procedure(Destroyer), public, pass(this), deferred :: destroy
      procedure, private, pass(this) :: set_cwrap
      procedure, private, pass(this) :: set_cwrap_null
      generic, public :: set => set_cwrap_null, set_cwrap
   end type Cwrap

   interface
       subroutine Destroyer(this)
         import :: Cwrap
         class(Cwrap), intent(inout) :: this
      end subroutine Destroyer
   end interface
!=========================== MAPL_UDUNITSCONVERTER =============================
   type, extends(Cwrap) :: MAPL_Udunits_Converter
   contains
      procedure, public, pass(this) :: destroy => destroy_converter
      procedure, public, pass(this) :: convert_double
      procedure, public, pass(this) :: convert_float
      procedure, public, pass(this) :: convert_doubles
      procedure, public, pass(this) :: convert_floats
!      generic :: convert => &
!         convert_double, convert_float, convert_doubles, convert_floats
   end type MAPL_Udunits_Converter

   interface MAPL_Udunits_Converter
      module procedure :: get_converter
   end interface MAPL_Udunits_Converter

!============================ MAPL_UDUNITS_SYSTEM ==============================
   type, extends(Cwrap) :: MAPL_Udunits_System
   contains
      procedure, public, pass(this) :: destroy => destroy_system
   end type MAPL_Udunits_System

   interface MAPL_Udunits_System
      module procedure :: get_system
   end interface MAPL_Udunits_System

   interface is_null
      module procedure :: is_c_null_ptr
      module procedure :: is_null_cwrap
   end interface is_null

   type(MAPL_Udunits_System), target :: SYSTEM_INSTANCE

!================================= PROCEDURES ==================================
contains

   logical function is_c_null_ptr(cptr)
      type(c_ptr), intent(in) :: cptr

      is_c_null_ptr = c_associated(cptr)

   end function is_c_null_ptr

   logical function is_null_cwrap(cw)
      class(Cwrap), intent(in) :: cw

      is_null_cwrap = is_null(cw % ptr)

   end function is_null_cwrap

   subroutine set_cwrap(this, cptr)
      class(Cwrap), intent(inout) :: this
      type(c_ptr), intent(in) :: cptr

      this % ptr = cptr

   end subroutine set_cwrap

   subroutine set_cwrap_null(this)
      class(Cwrap), intent(inout) :: this

      call this % set(c_null_ptr)

   end subroutine set_cwrap_null

   function get_system()
      type(MAPL_Udunits_System), pointer :: get_system

      get_system => SYSTEM_INSTANCE

   end function get_system

   type(MAPL_Udunits_Converter) function get_converter(from, to, path, encoding)
      character(len=*), intent(in) :: from, to
      character(len=*), optional, intent(in) :: path
      integer(ut_encoding), optional, intent(in) :: encoding
      type(c_ptr) :: ut_system_ptr, converter_ptr
      type(c_ptr) :: from_unit, to_unit
 
      ut_system_ptr = initialize(path)
      from_unit = ut_parse(ut_system_ptr, TRIMALL(from), get_encoding(encoding))
      to_unit = ut_parse(ut_system_ptr, TRIMALL(to), get_encoding(encoding))
      converter_ptr = ut_get_converter(from_unit, to_unit)
      call get_converter % set(converter_ptr)
      call destroy_ut_unit(from_unit)
      call destroy_ut_unit(from_unit)

   end function get_converter

   function convert_double(this, from) result(to)
      class(MAPL_Udunits_Converter), intent(in) :: this
      real(c_double), intent(in) :: from
      real(c_double) :: to
      type(c_ptr) :: cv_converter

      cv_converter = this % ptr
      to = cv_convert_double(cv_converter, from)

   end function convert_double

   function convert_float(this, from) result(to)
      class(MAPL_Udunits_Converter), intent(in) :: this
      real(c_float), intent(in) :: from
      real(c_float) :: to
      type(c_ptr) :: cv_converter

      cv_converter = this % ptr
      to = cv_convert_float(cv_converter, from)

   end function convert_float

   subroutine convert_doubles(this, from, to)
      class(MAPL_Udunits_Converter), intent(in) :: this
      real(c_double), intent(in) :: from(:)
      real(c_double), intent(out) :: to(:)
      type(c_ptr) :: cv_converter

      cv_converter = this % ptr
      call cv_convert_doubles(cv_converter, from, size(from), to)

   end subroutine convert_doubles

   subroutine convert_floats(this, from, to)
      class(MAPL_Udunits_Converter), intent(in) :: this
      real(c_float), intent(in) :: from(:)
      real(c_float), intent(out) :: to(:)
      type(c_ptr) :: cv_converter

      cv_converter = this % ptr
      call cv_convert_floats(cv_converter, from, size(from), to)

   end subroutine convert_floats

   function initialize(path)
      character(len=*), optional, intent(in) :: path
      type(c_ptr) :: initialize

      if(is_null(SYSTEM_INSTANCE)) SYSTEM_INSTANCE % ptr = get_ut_system(path)
      initialize = SYSTEM_INSTANCE % ptr

   end function initialize

   type(c_ptr) function get_ut_system(path)
      character(len=*), intent(in) :: path
!      type(c_ptr) :: path_pointer
      character(kind=c_char, len=(len_trim(path)+1)), target :: cpath
      type(c_ptr) :: cptr
      
      cpath = trim(path) // c_null_char
!      path_pointer = get_path_cptr(path)
!      if(is_null(path_pointer)) then
!         write(*, '(A)') 'get_ut_system: path_pointer is NULL.'
!      else
!         write(*, '(A)') 'get_ut_system: path_pointer is NOT NULL.'
!      end if
!      get_ut_system = ut_read_xml(path_pointer)

      cptr = c_loc(cpath)
      get_ut_system = ut_read_xml(cptr)
               
   end function get_ut_system

   subroutine destroy_ut_unit(ut_unit_ptr)
      type(c_ptr), intent(inout) :: ut_unit_ptr
      
      if(is_null(ut_unit_ptr)) return
      call ut_free(ut_unit_ptr)

   end subroutine destroy_ut_unit

   subroutine destroy_all()
      call SYSTEM_INSTANCE.destroy()
   end subroutine destroy_all

   subroutine destroy_system(this)
      class(MAPL_Udunits_System), intent(inout) :: this
      type(c_ptr) :: ut_system_ptr 
      
      ut_system_ptr = this % ptr
      if(.not. c_associated(ut_system_ptr)) return
      call ut_free_system(ut_system_ptr)
      call this % set()

   end subroutine destroy_system

   subroutine destroy_converter(this)
      class(MAPL_Udunits_Converter), intent(inout) :: this
      type(c_ptr) :: ptr

      if(is_null(this)) return
      ptr = this % ptr
      call cv_free(ptr)
      call this % set()

   end subroutine destroy_converter

   logical function are_convertible(unit1, unit2)
      type(c_ptr), intent(in) :: unit1, unit2
      integer(c_int), parameter :: ZERO = 0_c_int
      are_convertible = (ut_are_convertible(unit1, unit2) /= ZERO)
   end function are_convertible

   integer(ut_encoding) function get_encoding(encoding)
      integer(ut_encoding), optional, intent(in) :: encoding
      get_encoding = merge(encoding, UT_ENCODING_DEFAULT, present(encoding))
   end function get_encoding

!   subroutine get_unit_path(pathin, path, status)
!      character(kind=c_char, len=*), optional, intent(in) :: pathin
!      character(kind=c_char, len=*), intent(out) :: path
!      integer(ut_status), optional, intent(out) :: status
!      integer(ut_status) :: status_
!      type(c_ptr) :: cptr
!
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
!
!   end subroutine get_unit_path

   function get_ut_status_message(utstat) result(message)
      integer(ut_status), intent(in) :: utstat
      integer, parameter :: LL = 80
      character(len=LL), parameter :: messages(16) = [character(len=LL) :: &
         'UT_SUCCESS', & ! Success 
         'UT_BAD_ARG', & ! An argument violates the function's contract 
         'UT_EXISTS', & ! Unit, prefix, or identifier already exists 
         'UT_NO_UNIT', & ! No such unit exists 
         'UT_OS', & ! Operating-system error. See "errno". 
         'UT_NOT_SAME_SYSTEM', & ! The units belong to different unit-systems 
         'UT_MEANINGLESS', & ! The operation on the unit(s) is meaningless 
         'UT_NO_SECOND', & ! The unit-system doesn't have a unit named "second" 
         'UT_VISIT_ERROR', & ! An error occurred while visiting a unit 
         'UT_CANT_FORMAT', & ! A unit can't be formatted in the desired manner 
         'UT_SYNTAX', & ! string unit representation contains syntax error 
         'UT_UNKNOWN', & ! string unit representation contains unknown word 
         'UT_OPEN_ARG', & ! Can't open argument-specified unit database 
         'UT_OPEN_ENV', & ! Can't open environment-specified unit database 
         'UT_OPEN_DEFAULT', & ! Can't open installed, default, unit database 
         'UT_PARSE_ERROR' ] ! Error parsing unit specification 
      character(len=LL) :: message
      integer :: message_index

      message_index = utstat + 1

      if(message_index < 1 .or. message_index > size(messages)) then
         message = 'NOT FOUND'
         return
      end if

      message = messages(message_index)

      write(*, '(A)') 'message: "' // trim(message) // '"'

   end function get_ut_status_message

   function get_path_environment_variable(status) result(xmlpath)
      integer, optional, intent(out) :: status
      character(len=:), allocatable :: xmlpath
      character(len=MAXPATHLEN) :: rawpath
      character(len=*), parameter :: VARIABLE_NAME = 'UDUNITS2_XML_PATH'
      integer, parameter :: SUCCESS = 0
      integer, parameter :: ZERO_LENGTH = -2
      ! These are the status codes for get_environment_variable:
      ! -1: xmlpath is too short to contain value
      !  0: environment variable does exist
      !  1: environment variable does not exist
      ! The status code is passed through, but if the length is 0, ZERO_LENGTH is returned.
      integer :: length, status_

      call get_environment_variable(name=VARIABLE_NAME, value=rawpath, length=length, status=status_)

      if(status_ == SUCCESS) then
         if(length == 0) then
            xmlpath = EMPTY_STRING
            status_ = ZERO_LENGTH
         else
            write(*, *)
            write(*, '(A)') 'path is: "' // trim(xmlpath) // '"'
            write(*, '(A,1X,I4)') 'path length =', len_trim(xmlpath)
         end if
      end if

      if(status_ /= SUCCESS) xmlpath = EMPTY_STRING
      if(present(status)) status = status_

   end function get_path_environment_variable

   type(c_ptr) function get_path_cptr(path)
      character(len=*), intent(in) :: path
      character, target :: path_target(len_trim(path) + 1)

      if(len_trim(path) > 0) then
         write(*, '(A)') 'get_path_cptr: path = "' // trim(path) // '"'
         path_target = transfer(trim(path) // c_null_char, path_target)
         get_path_cptr = c_loc(path_target)
         return
      end if
      write(*, '(A)') 'get_path_cptr: NO PATH OR EMPTY PATH'
      get_path_cptr = c_null_ptr

   end function get_path_cptr

   type(c_ptr) function get_path_cptr_old(path)
      character(len=*), optional, intent(in) :: path

      if(present(path)) then
         if(len_trim(path) > 0) then
            write(*, '(A)') 'get_path_cptr_old: path = "' // trim(path) // '"'
            get_path_cptr_old = get_c_char_ptr(path)
            return
         end if
      end if
      write(*, '(A)') 'get_path_cptr_old: NO PATH OR EMPTY PATH'
      get_path_cptr_old = c_null_ptr

   end function get_path_cptr_old

   type(c_ptr) function get_c_char_ptr(s)
      character(len=*), intent(in) :: s
      character(len=len_trim(adjustl(s))+1), target :: s_

      s_ = trim(adjustl(s)) // c_null_char
      get_c_char_ptr = c_loc(s_)

   end function get_c_char_ptr

   subroutine get_fstring(carray, fstring)
      character(c_char), intent(in) :: carray(*)
      character(len=*, kind=c_char), intent(out) :: fstring
      integer :: i
      character(c_char) :: ch
      
      fstring = EMPTY_STRING
      do i=1, len(fstring)
         ch = carray(i)
         if(ch == c_null_char) exit
         fstring(i:i) = ch
      end do

   end subroutine get_fstring

   function make_fstring(cptr) result(fstring)
      interface
         integer(c_size_t) function strlen(cptr) bind(c, name='strlen')
            import :: c_ptr, c_size_t
            type(c_ptr), value :: cptr
         end function strlen
      end interface
      type(c_ptr), intent(in) :: cptr
      character(len=:), allocatable :: fstring
      character(len=:), pointer :: fptr
      integer(c_size_t) :: clen

      clen = strlen(cptr)
      call c_f_pointer(cptr, fptr)
      fstring = fptr(1:clen)

   end function make_fstring
      
end module udunits2mod
