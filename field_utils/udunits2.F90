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

#if defined(MERGE_PRESENT)
#undef MERGE_PRESENT
#endif
#define MERGE_PRESENT(A, B) merge(A, B, present(A))

module udunits2mod

   use iso_c_binding
!   use iso_c_binding, only: c_ptr, c_loc, c_associated, c_null_ptr, &
!      c_null_char, c_char, c_int, c_float, c_double, c_size_t, c_f_pointer

   implicit none

   public :: MAPL_Udunits_Converter
   public :: destroy_all

   !private
!=========================== PARAMETERS (CONSTANTS) ============================
   character(len=*), parameter :: EMPTY_STRING = ''
!   integer, parameter :: MAXPATHLEN = 1024

!================================ ENUMERATORS ==================================
   include 'udunits2enumerators.h'
   integer(ut_encoding), parameter :: UT_ENCODING_DEFAULT = UT_ASCII

!================================ C INTERFACES =================================
   include "udunits2interfaces.h"

   interface is_free
      module procedure :: is_free_cptr
      module procedure :: is_free_cwrap
   end interface is_free

!   abstract interface
!
!      subroutine ut_ptr_sub(utptr)
!         import :: c_ptr
!         type(c_ptr) :: utptr
!      end subroutine ut_ptr_sub
!
!   end interface

!=================================== CWRAP =====================================
   type, abstract :: Cwrap
      type(c_ptr) :: cptr_ = c_null_ptr
   contains
      procedure(Destroyer), public, pass(this), deferred :: destroy
      procedure, public, pass(this) :: set => set_cwrap_cptr
      procedure, public, pass(this) :: cptr => get_cwrap_cptr
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
   end type MAPL_Udunits_Converter

   interface MAPL_Udunits_Converter
      module procedure :: get_converter
   end interface MAPL_Udunits_Converter

!============================ MAPL_UDUNITS_SYSTEM ==============================
   type, extends(Cwrap) :: MAPL_Udunits_System
   contains
      procedure, public, pass(this) :: destroy => destroy_system
      procedure, public, pass(this) :: is_initialized
   end type MAPL_Udunits_System

   type :: SystemWrapper
      private
      type(c_ptr) :: utsystem
      logical :: system_set = .FALSE.
   contains
      procedure, public, pass(this) :: has_system_set => system_wrapper_has_system_set
      procedure, public, pass(this) :: get_utsystem => system_wrapper_get_utsystem
      procedure, public, pass(this) :: shutdown => shutdown_system_wrapper
   end type SystemWrapper

   interface SystemWrapper
      module procedure :: set_system_wrapper
   end interface SystemWrapper

   type(SystemWrapper) :: TheSystemWrapper
   type(MAPL_Udunits_System), target :: SYSTEM_INSTANCE

!================================= PROCEDURES ==================================
contains

    function set_system_wrapper(utsystem) result(sw)
      type(c_ptr), optional, intent(in) :: utsystem
      type(SystemWrapper) :: sw
   
      if(present(utsystem)) then
         sw % utsystem = utsystem
         sw % system_set = .TRUE.
      else
         sw % utsystem = c_null_ptr
         sw % system_set = .FALSE.
      end if

   end function set_system_wrapper

   logical function system_wrapper_has_system_set(this)
      class(SystemWrapper), intent(in) :: this

         system_wrapper_has_system_set = this % system_set

   end function system_wrapper_has_system_set 

   subroutine shutdown_system_wrapper(this, is_shutdown)
      class(SystemWrapper), intent(in) :: this
      logical, intent(out) :: is_shutdown
      type(c_ptr) :: utsystem
       
      if(this % has_system_set) then
         utsystem = this % utsystem
         call ut_free_system(utsystem)
         this % system_set = .FALSE.
      end if

      is_shutdown = .not. this % system_set

   end subroutine shutdown_system_wrapper

   function system_wrapper_get_utsystem(this) result(utsystem)
      class(SystemWrapper), intent(in) :: this
      type(c_ptr) :: utsystem

      if(this % has_system_set) then
         utsystem = this % system_set
      else
         utsystem = c_null_ptr
      end if

   end function system_wrapper_get_utsystem

   logical function is_initialized(this)
      class(MAPL_Udunits_System), intent(in) :: this

      is_initialized = c_associated(this % cptr())

   end function is_initialized

   function get_converter(from, to, path, encoding, rc) result(converter)
      type(MAPL_Udunits_Converter) :: converter
      character(len=*), intent(in) :: from, to
      character(len=*), optional, intent(in) :: path
      integer(ut_encoding), optional, intent(in) :: encoding
      integer(ut_status), optional, intent(out) :: rc
      type(c_ptr) :: utsystem, cvconverter
      type(c_ptr) :: from_unit, to_unit
      integer(ut_status) :: status
      integer(ut_encoding) :: encoding_
      logical :: convertible
      type(MAPL_Udunits_System), pointer :: instance

!      write(*, *) 'Entering get_converter'
      instance => null()
      utsystem = c_null_ptr
      from_unit = c_null_ptr
      to_unit = c_null_ptr

      encoding_ = merge(encoding, UT_ENCODING_DEFAULT, present(encoding))

      !wdb fixme deleteme Should we check for null?
      call initialize_ut_system(path)
      status = ut_get_status()
!      write(*, *) 'initialize, ut_status: ' // trim(get_ut_status_message(status)) // " ", status

!      if(status == UT_SUCCESS) utsystem = get_system_cptr()
      if(status == UT_SUCCESS) call get_instance(instance, status)
!      write(*, *) 'get_instance, status: ', status

      if(status == SUCCESS) utsystem = instance % cptr()

      if(.not. is_free(utsystem)) call get_unit(utsystem, from, encoding_, from_unit)
      status = ut_get_status()
!      write(*, *) 'get from_unit, ut_status: ' // trim(get_ut_status_message(status)) // " ", status

      if(status == UT_SUCCESS) call get_unit(utsystem, to, encoding_, to_unit)
      status = ut_get_status()
!      write(*, *) 'get to_unit, ut_status: ' // trim(get_ut_status_message(status)) // " ", status
      
      if(status == UT_SUCCESS) then
         convertible = are_convertible(from_unit, to_unit)
         status = ut_get_status()
!         write(*, *) 'are_convertible, ut_status: ' // trim(get_ut_status_message(status)) // " ", status
!         write(*, *) 'are_convertible: ', convertible

         if((status == UT_SUCCESS) .and. convertible) then
!            write(*, *) 'Convertible'
            cvconverter = ut_get_converter(from_unit, to_unit)
            status = ut_get_status()
!            write(*, *) 'ut_get_converter, ut_status: ' // trim(get_ut_status_message(status)) // " ", status
         else
!            write(*, *) 'Not convertible'
         end if
      end if

!      write(*, *) 'Free from_unit'
      call free_ut_unit(from_unit)
!      write(*, *) 'Free to_unit'
      call free_ut_unit(to_unit)

!      write(*, *) 'Setting converter'
      if(status == UT_SUCCESS) then
!         write(*, *) 'Setting cvconverter'
         call converter % set(cvconverter)
      else
!         write(*, *) 'Freeing cvconverter'
         call destroy_all()
      end if

      if(present(rc)) rc = status
!      write(*, *) 'Exiting get_converter'

   end function get_converter

   function convert_double(this, from) result(to)
      class(MAPL_Udunits_Converter), intent(in) :: this
      real(c_double), intent(in) :: from
      real(c_double) :: to
      type(c_ptr) :: cv_converter

      cv_converter = this % cptr()
      to = cv_convert_double(cv_converter, from)

   end function convert_double

   function convert_float(this, from) result(to)
      class(MAPL_Udunits_Converter), intent(in) :: this
      real(c_float), intent(in) :: from
      real(c_float) :: to
      type(c_ptr) :: cv_converter

      cv_converter = this % cptr()
      to = cv_convert_float(cv_converter, from)

   end function convert_float

   subroutine convert_doubles(this, from, to)
      class(MAPL_Udunits_Converter), intent(in) :: this
      real(c_double), intent(in) :: from(:)
      real(c_double), intent(out) :: to(:)
      type(c_ptr) :: cv_converter

      cv_converter = this % cptr()
      call cv_convert_doubles(cv_converter, from, size(from), to)

   end subroutine convert_doubles

   subroutine convert_floats(this, from, to)
      class(MAPL_Udunits_Converter), intent(in) :: this
      real(c_float), intent(in) :: from(:)
      real(c_float), intent(out) :: to(:)
      type(c_ptr) :: cv_converter

      cv_converter = this % cptr()
      call cv_convert_floats(cv_converter, from, size(from), to)

   end subroutine convert_floats

   subroutine initialize_ut_system(path, rc)
      character(len=*), optional, intent(in) :: path
      integer(ut_status), optional, intent(out) :: rc
      integer(ut_status) :: status
      type(c_ptr) :: utsystem, cptr
      type(MAPL_Udunits_System), pointer :: instance

      write(*, *) 'Entering initialize_ut_system.'
      instance => SYSTEM_INSTANCE
      if(instance % is_initialized()) then
         write(*, *) 'Initialized'
         status = UT_STATUS
      else
         write(*, *) 'Initializing'
         call read_xml(path, utsystem, rc=status)
         write(*, *) 'ut_status: ' // trim(get_ut_status_message(status)) // " ", status

         if(status == UT_SUCCESS) then
            write(*, *) 'Setting instance ut_system'
            call instance % set(utsystem)
            write(*, *) 'is_initialized: ', instance % is_initialized()
         else
            write(*, *) 'Freeing utsystem'
            call free_ut_system(utsystem)
         end if
      end if

      if(present(rc)) rc = status

   end subroutine initialize_ut_system

   subroutine get_instance(instance, rc)
      type(MAPL_Udunits_System), pointer, intent(out) :: instance
      integer, optional, intent(out) :: rc
      integer :: status

      if(is_free(SYSTEM_INSTANCE)) then
         instance => null()
         status = FAILURE
      else
         instance => SYSTEM_INSTANCE
         status = SUCCESS
      end if

      if(present(rc)) rc = status

   end subroutine get_instance

   type(c_ptr) function get_system_cptr() result(utsystem)

      if(is_free(SYSTEM_INSTANCE)) then
         utsystem = c_null_ptr
      else
         utsystem = SYSTEM_INSTANCE % cptr()
      end if

   end function get_system_cptr

   subroutine read_xml(path, utsystem, rc)
      character(len=*), optional, intent(in) :: path
      type(c_ptr), intent(out) :: utsystem
      integer(ut_status), optional, intent(out) :: rc
      integer(ut_status) :: status
      character(kind=c_char, len=MAXPATHLEN) :: path_
      
      write(*, *) 'Entering read_xml'
      if(present(path)) then
         write(*, *) 'Path'
         path_ = cstring(path)
         utsystem = ut_read_xml(path_)
      else
         write(*, *) 'No path'
         utsystem = ut_read_xml_cptr(c_null_ptr)
      end if

      status = ut_get_status()
      if(status == UT_SUCCESS) then
         write(*, *) 'read_xml successful'
      else
         write(*, *) 'read_xml failed: ', status
      end if
      if(present(rc)) rc = status

   end subroutine read_xml

!   subroutine free_utptr(utptr, utfreesub)
!      type(c_ptr), intent(inout) :: utptr
!      procedure(ut_ptr_sub) :: utfreesub
!
!      if(is_free(utptr)) return
!      call utfreesub(utptr)
!      utptr = c_null_ptr
!
!   end subroutine free_utptr

   subroutine free_ut_system(utsystem)
      type(c_ptr), intent(in) :: utsystem

      if(is_free(utsystem)) then
         write(*, *) 'utsystem is already free'
         return
      end if
      call ut_free_system(utsystem)

   end subroutine free_ut_system

   subroutine free_ut_unit(utunit)
      type(c_ptr), intent(in) :: utunit

      if(is_free(utunit)) then
         write(*, *) 'ut_unit is already free'
         return
      end if
      call ut_free(utunit)

   end subroutine free_ut_unit

   subroutine free_cv_converter(cv)
      type(c_ptr), intent(in) :: cv

      write(*, *) 'Entering free_cv_converter'
      if(is_free(cv)) then
         write(*, *) 'cv_converter is already free'
         return
      end if
      write(*, *) 'Freeing cv_converter'
      call cv_free(cv)
      write(*, *) 'Exiting free_cv_converter'

   end subroutine free_cv_converter

   subroutine destroy_all()
      call SYSTEM_INSTANCE.destroy()
   end subroutine destroy_all

   subroutine destroy_system(this)
      class(MAPL_Udunits_System), intent(inout) :: this
      type(c_ptr) :: utsystem 
      
      utsystem = this % cptr()
      write(*, *) 'c_associated(utsystem) ', c_associated(utsystem)
      call free_ut_system(utsystem)
      write(*, *) 'ut_system freed'
      call this % set()
      write(*, *) 'is_initialized: ', this % is_initialized()
   end subroutine destroy_system

   subroutine destroy_converter(this)
      class(MAPL_Udunits_Converter), intent(inout) :: this
      type(c_ptr) :: ptr

      if(is_free(this)) return
      write(*, *) 'Destroying converter'
      ptr = this % cptr()
      call free_cv_converter(ptr)
      ptr = c_null_ptr
      call this % set()
      ptr = this % cptr()
      write(*, *) "destroyed: ", (.not. c_associated(ptr))

   end subroutine destroy_converter

   logical function are_convertible(unit1, unit2, rc)
      type(c_ptr), intent(in) :: unit1, unit2
      integer, optional, intent(out) :: rc
      integer(ut_status) :: status
      integer(c_int), parameter :: ZERO = 0_c_int
      are_convertible = (ut_are_convertible(unit1, unit2) /= ZERO)
      status = ut_get_status()
      if(present(rc)) rc = status
   end function are_convertible

   logical function is_free_cptr(cptr)
      type(c_ptr), intent(in) :: cptr

      is_free_cptr = .not. c_associated(cptr)

   end function is_free_cptr

   logical function is_free_cwrap(cw)
      class(Cwrap), intent(in) :: cw

      is_free_cwrap = is_free(cw % cptr())

   end function is_free_cwrap

   subroutine set_cwrap_cptr(this, cptr)
      class(Cwrap), intent(inout) :: this
      type(c_ptr), optional, intent(in) :: cptr
      type(c_ptr) :: cptr_ = c_null_ptr
      
      write(*, *) 'Entering set_cwrap_cptr'
      write(*, *) 'c_associated(cptr_):', c_associated(cptr_)
      write(*, *) 'present(cptr):', present(cptr)
      if(present(cptr)) cptr_ = cptr
      write(*, *) 'c_associated(cptr_):', c_associated(cptr_)
      this % cptr_ = cptr_
      write(*, *) 'c_associated(this % cptr_):', c_associated(this % cptr_)
      write(*, *) 'Exiting set_cwrap_cptr'

   end subroutine set_cwrap_cptr

   type(c_ptr) function get_cwrap_cptr(this)
      class(Cwrap), intent(in) :: this

      get_cwrap_cptr = this % cptr_

   end function get_cwrap_cptr

   subroutine get_unit(system, identifier, encoding, utunit)
      type(c_ptr), intent(in) :: system
      character(len=*), intent(in) :: identifier
      integer(ut_encoding), intent(in) :: encoding
      type(c_ptr), intent(out) :: utunit
      character(kind=c_char, len=MAXPATHLEN) :: identifier_

      identifier_ = cstring(adjustl(identifier))
      utunit = ut_parse(system, identifier_, encoding) !wdb fixme deleteme trim(identifier_)?

   end subroutine get_unit

   function cstring(s)
      character(len=*), intent(in) :: s
      character(kind=c_char, len=(len(s) + 1)) :: cstring

      cstring = s // c_null_char

   end function cstring

!=================================== UNUSED ====================================
!   logical function cwrap_is_null(this)
!      class(Cwrap), intent(in) :: this
!
!      cwrap_is_null = is_null(this % cptr())
!
!   end function cwrap_is_null

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
!
!      message_index = utstat + 1
!
!      if(message_index < 1 .or. message_index > size(messages)) then
!         message = 'NOT FOUND'
!         return
!      end if
!
!      message = messages(message_index)
!
!   end function get_ut_status_message
      
   function get_ut_status_message(utstat) result(message)
      integer(ut_status), intent(in) :: utstat
      integer, parameter :: LL = 80
      character(len=LL) :: message
      
      select case(utstat)
         case(UT_SUCCESS)
            message = 'UT_SUCCESS'
         case(UT_BAD_ARG)
            message = 'UT_BAD_ARG'
         case(UT_EXISTS)
            message = 'UT_EXISTS'
         case(UT_NO_UNIT)
            message = 'UT_NO_UNIT'
         case(UT_OS)
            message = 'UT_OS'
         case(UT_NOT_SAME_SYSTEM)
            message = 'UT_NOT_SAME_SYSTEM'
         case(UT_MEANINGLESS)
            message = 'UT_MEANINGLESS'
         case(UT_NO_SECOND)
            message = 'UT_NO_SECOND'
         case(UT_VISIT_ERROR)
            message = 'UT_VISIT_ERROR'
         case(UT_CANT_FORMAT)
            message = 'UT_CANT_FORMAT'
         case(UT_SYNTAX)
            message = 'UT_SYNTAX'
         case(UT_UNKNOWN)
            message = 'UT_UNKNOWN'
         case(UT_OPEN_ARG)
            message = 'UT_OPEN_ARG'
         case(UT_OPEN_ENV)
            message = 'UT_OPEN_ENV'
         case(UT_OPEN_DEFAULT)
            message = 'UT_OPEN_DEFAULT'
         case(UT_PARSE_ERROR)
            message = 'UT_PARSE_ERROR'
         case default
            message = '[UNKNOWN ERROR]'
         end select

   end function get_ut_status_message

   function get_path_environment_variable(status) result(xmlpath)
      integer, optional, intent(out) :: status
      character(len=:), allocatable :: xmlpath
      character(len=MAXPATHLEN) :: rawpath
      character(len=*), parameter :: VARIABLE_NAME = 'UDUNITS2_XML_PATH'
      integer, parameter :: ZERO_LENGTH = -2
      ! These are the status codes for get_environment_variable:
      ! -1: xmlpath is too short to contain value
      !  0: environment variable does exist
      !  1: environment variable does not exist
      ! The status code is passed through, but if the length is 0, ZERO_LENGTH is returned.
      integer :: length, status_

      call get_environment_variable(name=VARIABLE_NAME, value=rawpath, length=length, status=status_)

      xmlpath = EMPTY_STRING

      if(length == 0) then
         if(present(status)) status = ZERO_LENGTH
         return
      end if

      if(status_ /= SUCCESS) then
         if(present(status)) status = status_
         return
      endif
      
      xmlpath = adjustl(rawpath)
      if(present(status)) status = status_

   end function get_path_environment_variable

   type(c_ptr) function get_path_cptr(path)
      character(len=*), optional, intent(in) :: path

      get_path_cptr = c_null_ptr
      if(present_nonempty(path)) get_path_cptr = character_cptr(path)

   end function get_path_cptr

   logical function present_nonempty(s)
      character(len=*), optional, intent(in) :: s

      present_nonempty = .FALSE.
      if(present(s)) present_nonempty = (len_trim(s) > 0)

   end function present_nonempty

   type(c_ptr) function character_cptr(s, strip)
      character(len=*), intent(in) :: s
      logical, optional, intent(in) :: strip
      character(kind=c_char, len=(len(s)+1)) :: scalar_char
      logical :: do_strip
      
      do_strip = merge(strip, .TRUE., present(strip))
      character_cptr = c_null_ptr
      if(do_strip) then
         scalar_char = cstring(trim(adjustl((s))))
      else
         scalar_char = cstring(s)
      end if

      character_cptr = char_cptr(scalar_char)

   end function character_cptr

   type(c_ptr) function char_cptr(s)
      character(kind=c_char), target, intent(in) :: s(*)
      
      char_cptr = c_loc(s)

   end function char_cptr

   subroutine get_path_xml_path(path, xmlpath, rc)
      character(len=*), intent(in) :: path
      character(kind=c_char, len=MAXPATHLEN), intent(out) :: xmlpath
      integer, optional, intent(out) :: rc
      integer(ut_status) :: status
      character(len=len(path)) :: path_
      type(c_ptr) :: pathptr
      integer(c_size_t) :: length

      pathptr = ut_get_path_xml(path_, status)
      length = strlen(path_)
      if(length > MAXPATHLEN) then
         xmlpath = EMPTY_STRING
         if(present(rc)) rc = FAILURE
      else
         xmlpath = path_(1:length)
         if(present(rc)) rc = status
      end if

   end subroutine get_path_xml_path

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

!   type(c_ptr) function get_unit(system, identifier, encoding) result(utunit)
!      type(c_ptr), intent(in) :: system
!      character(len=*), intent(in) :: identifier
!      integer(ut_encoding), intent(in) :: encoding
!      character(kind=c_char, len=MAXPATHLEN) :: identifier_
!
!      identifier_ = cstring(trim(adjustl(identifier)))
!      utunit = ut_parse(system, identifier_, encoding)
!
!   end function get_unit

end module udunits2mod
