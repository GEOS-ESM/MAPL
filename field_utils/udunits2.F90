#if defined(MAXPATHLEN)
#undef MAXPATHLEN
#endif
#define MAXPATHLEN 1024

#if defined(_RUN_RC_)
#undef _RUN_RC_
#endif

#if defined(_RUN_SUB_RC_)
#undef _RUN_SUB_RC_
#endif

#if defined(_RUN_SUB_RC)
#undef _RUN_SUB_RC
#endif

#if defined(_RUN_FUNC_RC_)
#undef _RUN_FUNC_RC_
#endif

#if defined(_RUN_RC)
#undef _RUN_RC
#endif

#if defined(_RUN_FUNC_RC)
#undef _RUN_FUNC_RC
#endif

#define _RUN_RC_(rc, status, COMMAND) rc=status); COMMAND; _VERIFY(status
#define _RUN_RC(COMMAND) _RUN_RC_(rc, status, COMMAND)
#define _RUN_SUB_RC_(rc, status, SUB, args...) \
   _RUN_RC_(rc, status, call SUB(args))
#define _RUN_SUB_RC(SUB, args...) _RUN_RC_(rc, status, call SUB(args))
#define _RUN_FUNC_RC_(rc, status, FUNC, RVAL, args...) \
   _RUN_RC_(rc, status, RVAL = FUNC(args))
#define _RUN_FUNC_RC(FUNC, RVAL, args...) \
   _RUN_FUNC_RC_(rc, status, FUNC, RVAL, args)

#include "MAPL_Generic.h"
module udunits2mod

   use iso_c_binding, only: c_ptr, c_loc, c_associated, c_null_ptr, c_null_char, c_char, c_int, c_float, c_double, c_size_t, c_f_pointer
   use udunits2interfaces
   use udunits2status
   use udunits2encoding
   use MAPL_ExceptionHandling

   implicit none

   public :: Converter
   public :: get_converter
   public :: initialize
   public :: finalize

   !private
!=========================== PARAMETERS (CONSTANTS) ============================
   character(len=*), parameter :: EMPTY_STRING = ''

!================================ ENUMERATORS ==================================
   integer(ut_encoding), parameter :: UT_ENCODING_DEFAULT = UT_ASCII

   type, abstract :: CptrWrapper
      private
      type(c_ptr) :: cptr = c_null_ptr
      !wdb fixme deleteme may not need if c_associated works
      logical :: is_set_ = .FALSE.
   contains
      procedure, public, pass(this) :: get_cptr
      procedure, public, pass(this) :: cptr_is_set !wdb fixme deleteme use c_associated?
      procedure, private, pass(this) :: set_cptr
      procedure, private, pass(this) :: unset_cptr
      procedure, public, pass(this) :: free
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
      private
   contains
      procedure, public, pass(this) :: free_space => free_cv_converter
      procedure, private, pass(this) :: convert_double
      procedure, private, pass(this) :: convert_float
      procedure, private, pass(this) :: convert_doubles
      procedure, private, pass(this) :: convert_floats
      generic :: convert => convert_double, convert_doubles, convert_float, convert_floats
   end type Converter

   interface Converter
      module procedure :: construct_converter
   end interface Converter

!=============================== SYSTEMWRAPPER =================================
   type, extends(CptrWrapper) :: SystemWrapper
      private
      integer(ut_encoding) :: encoding = UT_ENCODING_DEFAULT
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
   type(SystemWrapper), protected :: SYSTEM_INSTANCE
   !type(SystemWrapper), private :: SYSTEM_INSTANCE !wdb fixme deleteme 

   interface true
      module procedure :: ctrue
      module procedure :: ftrue
   end interface true

   interface successful
      module procedure :: csuccessful
      module procedure :: fsuccessful
   end interface successful

contains

!================================= PROCEDURES ==================================

   logical function ftrue(n)
      integer, intent(in) :: n
      
      ftrue = (n /= 0)

   end function ftrue

   logical function fsuccessful(rc)
      integer, intent(in) :: rc

      fsuccessful = (rc == 0)

   end function fsuccessful

   integer(c_int) function ctrue(b)
      logical, intent(in) :: b

      ctrue = merge(1_c_int, 0_c_int, b)

   end function ctrue

   integer(c_int) function csuccessful(b)
      logical, intent(in) :: b

      csuccessful = merge(0_c_int, 1_c_int, b)

   end function csuccessful

   type(c_ptr) function get_cptr(this)
      class(CptrWrapper), intent(in) :: this

      get_cptr = this % cptr

   end function get_cptr

   !wdb fixme deleteme check c_associated instead
   logical function cptr_is_set(this)
      class(CptrWrapper), intent(in) :: this
         
      cptr_is_set = this % is_set_

   end function cptr_is_set

   subroutine set_cptr(this, cptr)
      class(CptrWrapper), intent(inout) :: this
      type(c_ptr), intent(in) :: cptr

      this % cptr = cptr
      this % is_set_ = .TRUE.

   end subroutine set_cptr

   subroutine unset_cptr(this)
      class(CptrWrapper), intent(inout) :: this

      this % cptr = c_null_ptr
      this % is_set_ = .FALSE.

   end subroutine unset_cptr

   subroutine free(this)
      class(CptrWrapper), intent(inout) :: this

      if(this % cptr_is_set()) call this % free_space()
      call this % unset_cptr()

   end subroutine free

   function construct_converter(from_unit, to_unit) result(converter)
      type(Converter) :: converter
      type(UnitWrapper), intent(in) :: from_unit
      type(UnitWrapper), intent(in) :: to_unit
      type(c_ptr) :: cvconverter
      logical :: convertible
      integer(ut_status) :: status

!      call converter % unset_cptr() 
      if(.not. from_unit % cptr_is_set()) return
      if(.not. to_unit % cptr_is_set()) return

      call are_convertible(from_unit, to_unit, convertible, rc=status)
      status = ut_get_status()
      if(.not. utsuccess(status)) return
      if(.not. convertible) return

      cvconverter = c_null_ptr
      cvconverter = ut_get_converter(from_unit % get_cptr(), to_unit % get_cptr())
      status = ut_get_status()

      if(utsuccess(status)) then
         call converter % set_cptr(cvconverter)
      else
         if(c_associated(cvconverter)) call cv_free(cvconverter)
      end if

   end function construct_converter

   function construct_system(path, encoding) result(wrapper)
      type(SystemWrapper) :: wrapper
      character(len=*), optional, intent(in) :: path
      integer(ut_encoding), optional, intent(in) :: encoding
      type(c_ptr) :: utsystem
      integer(ut_status) :: status

      call read_xml(path, utsystem, rc = status)
      
      if(.not. utsuccess(status)) then
         if(c_associated(utsystem)) call ut_free_system(utsystem)
         call wrapper % unset_cptr()
         return
      end if

      call wrapper % set_cptr(utsystem)
      if(present(encoding)) wrapper % encoding = encoding

   end function construct_system

   function construct_unit(syswrapper, identifier, encoding) result(wrapper)
      type(UnitWrapper) :: wrapper
      class(SystemWrapper), intent(in) :: syswrapper
      character(len=*), intent(in) :: identifier
      integer(ut_encoding), optional, intent(in) :: encoding
      character(kind=c_char, len=:), allocatable :: identifier_
      integer(ut_status) :: status
      type(c_ptr) :: utunit

      identifier_ = cstring(identifier)
      if(present(encoding)) encoding_ = encoding
      utunit = ut_parse(syswrapper % get_cptr(), identifier_, syswrapper % encoding)
      status = ut_get_status()

      if(utsuccess(status)) then
         call wrapper % set_cptr(utunit)
      else
         if(c_associated(utunit)) call ut_free(utunit)
         call wrapper % unset_cptr()
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
      rc = (conv % cptr_is_set())
      if(conv % cptr_is_set()) then
         status = UT_SUCCESS
      else
         status = UT_FAILURE
      end if

      _RETURN(status)

   end subroutine get_converter

   function get_converter_function(from, to, path, encoding) result(conv)
      type(Converter) :: conv
      character(len=*), intent(in) :: from, to
      character(len=*), optional, intent(in) :: path
      integer(ut_encoding), optional, intent(in) :: encoding
      type(UnitWrapper) :: from_unit
      type(UnitWrapper) :: to_unit

!      call conv % unset_cptr()
      call initialize_system(SYSTEM_INSTANCE, path)
      if(.not. SYSTEM_INSTANCE % cptr_is_set()) return

      from_unit = UnitWrapper(SYSTEM_INSTANCE, from, encoding)
      to_unit = UnitWrapper(SYSTEM_INSTANCE, to, encoding)

      if(from_unit % cptr_is_set() .and. to_unit % cptr_is_set()) conv = Converter(from_unit, to_unit)

      call from_unit % free()
      call to_unit % free()

   end function get_converter_function

   function convert_double(this, from) result(to)
      class(Converter), intent(in) :: this
      real(c_double), intent(in) :: from
      real(c_double) :: to
      type(c_ptr) :: cv_converter

      cv_converter = this % get_cptr()
      to = cv_convert_double(cv_converter, from)

   end function convert_double

   function convert_float(this, from) result(to)
      class(Converter), intent(in) :: this
      real(c_float), intent(in) :: from
      real(c_float) :: to
      type(c_ptr) :: cv_converter

      cv_converter = this % get_cptr()
      to = cv_convert_float(cv_converter, from)

   end function convert_float

   subroutine convert_doubles(this, from, to)
      class(Converter), intent(in) :: this
      real(c_double), intent(in) :: from(:)
      real(c_double), intent(out) :: to(:)
      type(c_ptr) :: cv_converter

      cv_converter = this % get_cptr()
      call cv_convert_doubles(cv_converter, from, size(from), to)

   end subroutine convert_doubles

   subroutine convert_floats(this, from, to)
      class(Converter), intent(in) :: this
      real(c_float), intent(in) :: from(:)
      real(c_float), intent(out) :: to(:)
      type(c_ptr) :: cv_converter

      cv_converter = this % get_cptr()
      call cv_convert_floats(cv_converter, from, size(from), to)

   end subroutine convert_floats

   logical function utsuccess(utstatus)
      integer(ut_status) :: utstatus

      utsuccess = (utstatus == UT_SUCCESS)

   end function utsuccess

   subroutine read_xml(path, utsystem, rc)
      character(len=*), optional, intent(in) :: path
      type(c_ptr), intent(out) :: utsystem
      integer(ut_status), intent(out) :: rc

      if(present(path)) then
         utsystem = ut_read_xml(cstring(path))
      else
         utsystem = ut_read_xml_cptr(c_null_ptr)
      end if
      rc = ut_get_status()

   end subroutine read_xml

   subroutine initialize(path, encoding, rc)
      character(len=*), optional, intent(in) :: path
      integer(ut_encoding), optional, intent(in) :: encoding
      integer, optional, intent(in) :: rc

      if(instance_is_initialized()) return
      call initialize_system(SYSTEM_INSTANCE, path, encoding) 
      _RETURN(successful(SYSTEM_INSTANCE % cptr_is_set()))

   end subroutine initialize

   subroutine initialize_system(system, path, encoding)
      type(SystemWrapper), intent(inout) :: system
      character(len=*), optional, intent(in) :: path
      integer(ut_encoding), optional, intent(in) :: encoding
      integer(ut_status) :: status
      type(c_ptr) :: utsystem

      if(.not. system % cptr_is_set()) system = SystemWrapper(path, encoding)

   end subroutine initialize_system

   logical function instance_is_initialized()
      
      instance_is_initialized = SYSTEM_INSTANCE % cptr_is_set()
      
   end function instance_is_initialized

   subroutine free_ut_system(this)
      class(SystemWrapper), intent(in) :: this
      type(c_ptr) :: cptr
        
      cptr = this % get_cptr()
      if(c_associated(cptr)) call ut_free_system(cptr)

   end subroutine free_ut_system

   subroutine free_ut_unit(this)
      class(UnitWrapper), intent(in) :: this
      type(c_ptr) :: cptr

      cptr = this % get_cptr()
      if(c_associated(cptr)) call ut_free(cptr)

   end subroutine free_ut_unit

   subroutine free_cv_converter(this)
      class(Converter), intent(in) :: this
      type(c_ptr) :: cptr 

      cptr = this % get_cptr()
      if(c_associated(cptr)) call cv_free(cptr)

   end subroutine free_cv_converter

   subroutine finalize()

      if(SYSTEM_INSTANCE % cptr_is_set()) call SYSTEM_INSTANCE % free()

   end subroutine finalize

   subroutine are_convertible(unit1, unit2, convertible, rc)
      type(UnitWrapper), intent(in) :: unit1, unit2
      logical, intent(out) :: convertible
      integer, optional, intent(out) :: rc
      integer(ut_status) :: status
      integer(c_int), parameter :: ZERO = 0_c_int
      type(c_ptr) :: utunit1, utunit2
      
      utunit1 = unit1 % get_cptr()
      utunit2 = unit2 % get_cptr()
      convertible = (ut_are_convertible(utunit1, utunit2)  /= ZERO)
      status = ut_get_status()
      if(present(rc)) rc = status
   end subroutine are_convertible

   function cstring(s)
      character(len=*), intent(in) :: s
      character(kind=c_char, len=(len(s) + 1)) :: cstring

      cstring = adjustl(trim(s)) // c_null_char

   end function cstring

end module udunits2mod
