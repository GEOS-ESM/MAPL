#if defined(MAXPATHLEN)
#undef MAXPATHLEN
#endif
#define MAXPATHLEN 1024

#include "MAPL_Generic.h"
module udunits2mod

   use iso_c_binding, only: c_ptr, c_loc, c_associated, c_null_ptr, c_null_char, c_char, c_int, c_float, c_double, c_size_t, c_f_pointer

   implicit none

   public :: MAPL_UDUNITS_Converter
   public :: Get_MAPL_UDUNITS_Converter
   public :: SystemWrapper
   public :: UnitWrapper

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
   type, extends(CptrWrapper) :: MAPL_UDUNITS_Converter
   contains
      procedure, public, pass(this) :: free_space => free_cv_converter
      procedure, public, pass(this) :: convert_double
      procedure, public, pass(this) :: convert_float
      procedure, public, pass(this) :: convert_doubles
      procedure, public, pass(this) :: convert_floats
   end type MAPL_UDUNITS_Converter

   interface MAPL_UDUNITS_Converter
      module procedure :: construct_converter
   end interface MAPL_UDUNITS_Converter

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
      type(MAPL_UDUNITS_Converter) :: converter
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

   subroutine Get_MAPL_UDUNITS_Converter(conv, from, to, path, encoding, rc)
      type(MAPL_UDUNITS_Converter), intent(inout) :: conv
      character(len=*), intent(in) :: from, to
      character(len=*), optional, intent(in) :: path
      integer(ut_encoding), optional, intent(in) :: encoding
      integer(ut_status), optional, intent(out) :: rc
      integer(ut_status) :: status

      conv = get_converter_function(from, to, path, encoding)
      
      if(conv % is_set()) then
         status = UT_SUCCESS
      else
         status = _FAILURE
      end if

      if(present(rc)) rc = status

   end subroutine Get_MAPL_UDUNITS_Converter

   function get_converter_function(from, to, path, encoding) result(conv)
      type(MAPL_UDUNITS_Converter) :: conv
      character(len=*), intent(in) :: from, to
      character(len=*), optional, intent(in) :: path
      integer(ut_encoding), optional, intent(in) :: encoding
      type(UnitWrapper) :: from_unit
      type(UnitWrapper) :: to_unit

      call conv % set()
      call initialize_system(SYSTEM_INSTANCE, path)
      if(.not. SYSTEM_INSTANCE % is_set()) return

      from_unit = UnitWrapper(SYSTEM_INSTANCE, from, encoding)
      to_unit = UnitWrapper(SYSTEM_INSTANCE, to, encoding)

      if(from_unit % is_set() .and. to_unit % is_set()) conv = MAPL_UDUNITS_Converter(from_unit, to_unit)

      call from_unit % shutdown()
      call to_unit % shutdown()

   end function get_converter_function

   function convert_double(this, from) result(to)
      class(MAPL_UDUNITS_Converter), intent(in) :: this
      real(c_double), intent(in) :: from
      real(c_double) :: to
      type(c_ptr) :: cv_converter

      cv_converter = this % get()
      to = cv_convert_double(cv_converter, from)

   end function convert_double

   function convert_float(this, from) result(to)
      class(MAPL_UDUNITS_Converter), intent(in) :: this
      real(c_float), intent(in) :: from
      real(c_float) :: to
      type(c_ptr) :: cv_converter

      cv_converter = this % get()
      to = cv_convert_float(cv_converter, from)

   end function convert_float

   subroutine convert_doubles(this, from, to)
      class(MAPL_UDUNITS_Converter), intent(in) :: this
      real(c_double), intent(in) :: from(:)
      real(c_double), intent(out) :: to(:)
      type(c_ptr) :: cv_converter

      cv_converter = this % get()
      call cv_convert_doubles(cv_converter, from, size(from), to)

   end subroutine convert_doubles

   subroutine convert_floats(this, from, to)
      class(MAPL_UDUNITS_Converter), intent(in) :: this
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
      character(kind=c_char, len=MAXPATHLEN) :: path_

      if(present(path)) then
         path_ = cstring(path)
         utsystem = ut_read_xml(path_)
      else
         utsystem = ut_read_xml_cptr(c_null_ptr)
      end if

      if(present(rc)) rc = ut_get_status()

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
      class(MAPL_UDUNITS_Converter), intent(in) :: this
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
