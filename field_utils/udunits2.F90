#include "MAPL_Generic.h"
module udunits2mod

   use iso_c_binding, only: c_ptr, c_associated, c_null_ptr, c_null_char
   use iso_c_binding, only: c_char, c_int, c_float, c_double
   use udunits2interfaces
   use udunits2encoding
   use udunits2status
   use MAPL_ExceptionHandling

   implicit none

   public :: Converter
   public :: get_converter
   public :: initialize
   public :: finalize

! Normally, only the procedures and derived type above are public.
! The private line following this block enforces that. For full testing,
! comment the private line.
   private

!================================ CPTRWRAPPER ==================================
   type, abstract :: CptrWrapper
      private
      type(c_ptr) :: cptr_ = c_null_ptr
   contains
      procedure, public, pass(this) :: cptr
      procedure, public, pass(this) :: is_free
      procedure, public, pass(this) :: free
      procedure(CptrWrapperSub), private, deferred, pass(this) :: free_space
   end type CptrWrapper

   abstract interface

      subroutine CptrWrapperSub(this)
         import :: CptrWrapper
         class(CptrWrapper), intent(in) :: this
      end subroutine CptrWrapperSub

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
      generic :: convert => convert_double, convert_float
      generic :: convert_array => convert_doubles, convert_floats
   end type Converter

   interface Converter
      module procedure :: construct_converter
   end interface Converter

!=============================== UDSYSTEM =================================
   type, extends(CptrWrapper) :: UDSystem
      private
      integer(ut_encoding) :: encoding = UT_ASCII
   contains
      procedure, public, pass(this) :: free_space => free_ut_system
   end type UDSystem

   interface UDSystem
      module procedure :: construct_system
   end interface UDSystem

!=================================== UDUNIT ====================================
   type, extends(CptrWrapper) :: UDUnit
   contains
      procedure, public, pass(this) :: free_space => free_ut_unit
   end type UDUnit

   interface UDUnit
      module procedure :: construct_unit
   end interface UDUnit

!============================= INSTANCE VARIABLES ==============================
   type(UDSystem), private :: SYSTEM_INSTANCE

contains

   logical function success(utstatus)
      integer(ut_status) :: utstatus

      success = (utstatus == UT_SUCCESS)

   end function success

   type(c_ptr) function cptr(this)
      class(CptrWrapper), intent(in) :: this

      cptr = this % cptr_

   end function cptr

   logical function is_free(this)
      class(CptrWrapper), intent(in) :: this

      is_free = .not. c_associated(this % cptr_)

   end function is_free

   subroutine free(this)
      class(CptrWrapper), intent(inout) :: this

      if(this % is_free()) return
      call this % free_space()
      this % cptr_ = c_null_ptr

   end subroutine free

   function construct_system(path, encoding) result(instance)
      type(UDsystem) :: instance
      character(len=*), optional, intent(in) :: path
      integer(ut_encoding), optional, intent(in) :: encoding
      type(c_ptr) :: utsystem
      integer(ut_status) :: status

      call read_xml(path, utsystem, status)
      
      if(success(status)) then
         instance % cptr_ = utsystem
         if(present(encoding)) instance % encoding = encoding
         return
      end if
         
      if(c_associated(utsystem)) call ut_free_system(utsystem)

   end function construct_system

   function construct_unit(identifier) result(instance)
      type(UDUnit) :: instance
      character(len=*), intent(in) :: identifier
      character(kind=c_char, len=:), allocatable :: cchar_identifier
      type(c_ptr) :: utunit1

      if(instance_is_uninitialized()) return

      cchar_identifier = cstring(identifier)
      utunit1 = ut_parse(SYSTEM_INSTANCE % cptr(), cchar_identifier, SYSTEM_INSTANCE % encoding)

      if(success(ut_get_status())) then
         instance % cptr_ = utunit1
      else
         if(c_associated(utunit1)) call ut_free(utunit1)
      end if

   end function construct_unit

   function construct_converter(from_unit, to_unit) result(conv)
      type(Converter) :: conv
      type(UDUnit), intent(in) :: from_unit
      type(UDUnit), intent(in) :: to_unit
      type(c_ptr) :: cvconverter1
      logical :: convertible

      if(from_unit % is_free() .or. to_unit % is_free()) return
      if(.not. are_convertible(from_unit, to_unit)) return

      cvconverter1 = ut_get_converter(from_unit % cptr(), to_unit % cptr())

      if(success(ut_get_status())) then
         conv % cptr_ = cvconverter1
      else
         if(c_associated(cvconverter1)) call cv_free(cvconverter1)
      end if

   end function construct_converter

   subroutine get_converter(conv, from, to, rc)
      type(Converter), intent(inout) :: conv
      character(len=*), intent(in) :: from, to
      integer(ut_status), optional, intent(out) :: rc
      integer(ut_status) :: status

      conv = get_converter_function(from, to)
      status = merge(_FAILURE, UT_SUCCESS, conv % is_free())
      _RETURN(status)

   end subroutine get_converter

   function get_converter_function(from, to) result(conv)
      type(Converter) :: conv
      character(len=*), intent(in) :: from, to
      type(UDUnit) :: from_unit
      type(UDUnit) :: to_unit

      if(instance_is_uninitialized()) return

      from_unit = UDUnit(from)
      if(from_unit % is_free()) return
      to_unit = UDUnit(to)
      if(to_unit % is_free()) then
         call from_unit % free()
         return
      end if

      conv = Converter(from_unit, to_unit)

      call from_unit % free()
      call to_unit % free()

   end function get_converter_function

   impure elemental function convert_double(this, from) result(to)
      class(Converter), intent(in) :: this
      real(c_double), intent(in) :: from
      real(c_double) :: to

      to = cv_convert_double(this % cptr(), from)

   end function convert_double

   impure elemental function convert_float(this, from) result(to)
      class(Converter), intent(in) :: this
      real(c_float), intent(in) :: from
      real(c_float) :: to

      to = cv_convert_float(this % cptr(), from)

   end function convert_float

   subroutine convert_doubles(this, from, to)
      class(Converter), intent(in) :: this
      real(c_double), intent(in) :: from(:)
      real(c_double), intent(out) :: to(:)

      call cv_convert_doubles(this % cptr(), from, size(from), to)

   end subroutine convert_doubles

   subroutine convert_floats(this, from, to)
      class(Converter), intent(in) :: this
      real(c_float), intent(in) :: from(:)
      real(c_float), intent(out) :: to(:)

      call cv_convert_floats(this % cptr(), from, size(from), to)

   end subroutine convert_floats

   subroutine read_xml(path, utsystem, status)
      character(len=*), optional, intent(in) :: path
      character(kind=c_char, len=:), allocatable :: cchar_path
      type(c_ptr), intent(out) :: utsystem
      integer(ut_status), intent(out) :: status

      if(present(path)) then
         cchar_path = cstring(path)
         utsystem = ut_read_xml(cchar_path)
      else
         utsystem = ut_read_xml_cptr(c_null_ptr)
      end if
      status = ut_get_status()

   end subroutine read_xml

   subroutine initialize(path, encoding, rc)
      character(len=*), optional, intent(in) :: path
      integer(ut_encoding), optional, intent(in) :: encoding
      integer, optional, intent(out) :: rc
      integer :: status

      _ASSERT(instance_is_uninitialized(), 'UDUNITS is already initialized.')
      call initialize_system(SYSTEM_INSTANCE, path, encoding, rc=status)
      if(status /= _SUCCESS) then
         call finalize()
         _FAIL('Failed to initialize UDUNITS')
      end if
      _ASSERT(.not. SYSTEM_INSTANCE % is_free(), 'UDUNITS is not initialized.')
      _RETURN(_SUCCESS)

   end subroutine initialize

   subroutine initialize_system(system, path, encoding, rc)
      type(UDSystem), intent(inout) :: system
      character(len=*), optional, intent(in) :: path
      integer(ut_encoding), optional, intent(in) :: encoding
      integer, optional, intent(out) :: rc
      integer :: status
      type(c_ptr) :: utsystem

      _ASSERT(system % is_free(), 'UDUNITS system is already initialized.')
      system = UDSystem(path, encoding)
      _RETURN(_SUCCESS)

   end subroutine initialize_system

   logical function instance_is_uninitialized()
      
      instance_is_uninitialized = SYSTEM_INSTANCE % is_free()
      
   end function instance_is_uninitialized

   subroutine free_ut_system(this)
      class(UDSystem), intent(in) :: this
        
      if(this % is_free()) return
      call ut_free_system(this % cptr())

   end subroutine free_ut_system

   subroutine free_ut_unit(this)
      class(UDUnit), intent(in) :: this

      if(this % is_free()) return
      call ut_free(this % cptr())

   end subroutine free_ut_unit

   subroutine free_cv_converter(this)
      class(Converter), intent(in) :: this
      type(c_ptr) :: cvconverter1 

      if(this % is_free()) return
      call cv_free(this % cptr())

   end subroutine free_cv_converter

   subroutine finalize()

      if(SYSTEM_INSTANCE % is_free()) return
      call SYSTEM_INSTANCE % free()

   end subroutine finalize

   logical function are_convertible(unit1, unit2, rc)
      type(UDUnit), intent(in) :: unit1, unit2
      integer, optional, intent(out) :: rc
      integer :: status
      integer(ut_status) :: utstatus
      logical :: convertible
      integer(c_int), parameter :: ZERO = 0_c_int
      
      convertible = (ut_are_convertible(unit1 % cptr(), unit2 % cptr())  /= ZERO)
      utstatus = ut_get_status()
      
      if(convertible) are_convertible = success(utstatus)
      status = merge(_SUCCESS, utstatus, convertible)

      if(present(rc)) rc = status

   end function are_convertible

   function cstring(s) result(cs)
      character(len=*), intent(in) :: s
      character(kind=c_char, len=:), allocatable :: cs

      cs = adjustl(trim(s)) // c_null_char

   end function cstring

end module udunits2mod
