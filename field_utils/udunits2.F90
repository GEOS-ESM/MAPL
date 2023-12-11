#if defined(TRIMALL)
#undef TRIMALL(S)
#endif
#define TRIMALL(S) trim(adjustl(S))

#if defined(LEN_TRIMALL)
#undef LEN_TRIMALL
#endif
#define LEN_TRIMALL(S) len_trim(adjustl(S))

module udunits2mod

!   use iso_c_binding, only: c_char, c_int, c_float, c_double, c_null_ptr, &
!      c_ptr, c_associated, c_null_char
   use iso_c_binding, only: c_ptr, c_loc, c_associated, c_null_ptr, c_null_char, &
      c_char, c_int, c_float, c_double

   implicit none

   !private

   public :: MAPL_Udunits_Converter

!================================== INCLUDE ====================================
   include 'udunits2enumerators.h'
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

   interface get_unit_database_path
      module procedure :: get_unit_database_path_
      module procedure :: get_unit_database_path_null
   end interface get_unit_database_path

   type(MAPL_Udunits_System), target :: SYSTEM_INSTANCE
   integer(ut_encoding) :: UT_ENCODING_DEFAULT = UT_ASCII

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
      character(len=*), optional, intent(in) :: path
      
      get_ut_system = ut_read_xml(get_path_pointer(path))
               
   end function get_ut_system

   type(c_ptr) function get_path_pointer(path)
      character(len=*), optional, intent(in) :: path

      get_path_pointer = c_null_ptr

      if(.not. present(path)) return
      if(len(path) == 0) return
      get_path_pointer = get_c_char_ptr(path)

   end function get_path_pointer

   type(c_ptr) function get_c_char_ptr(s)
      character(len=*), intent(in) :: s
      character(len=len_trim(adjustl(s))+1), target :: s_

      s_ = trim(adjustl(s)) // c_null_char
      get_c_char_ptr = c_loc(s_)

   end function get_c_char_ptr

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
!      if(is_null(this)) return
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

   type(c_ptr) function get_unit_database_path(path, status)
      character(len=*), optional, intent(in) :: path
      integer(c_int), intent(in) :: status

      get_unit_database_path = ut_get_path_xml(get_path_pointer(path), status, path)

   end function get_unit_database_path

   subroutine get_string_from_cptr(cptr, string)
      type(c_ptr), intent(in) :: cptr
      character(len=*), intent(out) :: string
      character(c_char) :: ca
      integer :: n, i

      do i = 1, len(string)
         

   function make_ut_status_messages() result(messages)
      character(len=32) :: messages(0:15)

      messages = [ &
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
         'UT_PARSE_ERROR' & ! Error parsing unit specification 
      ]

    end function make_ut_status_messages

   function get_ut_status_message(utstat) result(message)
      integer(ut_status), intent(in) :: utstat
      character(len=32) :: message
      character(len=32) :: messages(16)
      
      messages = make_ut_status_messages()
      if(utstat < 0) return
      if(utstat < size(messages)) then
         message = messages(utstat + 1)
         return
      end if

   end function get_ut_status_message

end module udunits2mod
