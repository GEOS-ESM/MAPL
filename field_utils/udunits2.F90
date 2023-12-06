#if defined(TRIMALL)
#undef TRIMALL(S)
#endif
#define TRIMALL(S) trim(adjustl(S))

module udunits2mod

   use iso_c_binding, only: c_char, c_int, c_float, c_double, c_null_ptr, &
      c_ptr, c_associated, c_null_char

   implicit none

   private

   public :: MAPL_UDUNITS_CONVERTER

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
      logical function Destroyer(this)
         import :: Cwrap
         class(Cwrap), intent(inout) :: this
      end function Destroyer
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
      type(c_ptr) :: ut_system_ptr
      type(c_ptr) :: from_unit, to_unit
      logical :: from_destroyed, to_destroyed
 
      ut_system_ptr = initialize(path)
      from_unit = ut_parse(ut_system_ptr, TRIMALL(from), get_encoding(encoding))
      to_unit = ut_parse(ut_system_ptr, TRIMALL(to), get_encoding(encoding))
      call get_converter % set(ut_get_converter(from_unit, to_unit))
      from_destroyed = destroy_ut_unit(from_unit)
      to_destroyed = destroy_ut_unit(from_unit)

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

      if(present(path)) then
         get_ut_system = ut_read_xml(TRIMALL(path) // c_null_char)
      else
         get_ut_system = ut_read_xml(c_null_char)
      end if
               
   end function get_ut_system

   logical function destroy_ut_unit(ut_unit_ptr) result(destroyed)
      type(c_ptr), intent(inout) :: ut_unit_ptr
      
      destroyed = .TRUE.
      if(is_null(ut_unit_ptr)) return
      call ut_free(ut_unit_ptr)
      destroyed = is_null(ut_unit_ptr)

   end function destroy_ut_unit

   logical function destroy_all() result(destroyed)
      destroyed = .TRUE.
      destroyed = SYSTEM_INSTANCE.destroy()
   end function destroy_all

   logical function destroy_system(this) result(destroyed)
      class(MAPL_Udunits_System), intent(inout) :: this
      type(c_ptr) :: ut_system_ptr 

      destroyed = .TRUE.
      if(is_null(this)) return
      ut_system_ptr = this % ptr
      call ut_free_system(ut_system_ptr)
      call this % set()
      destroyed = is_null(ut_system_ptr)

   end function destroy_system

   logical function destroy_converter(this) result(destroyed)
      class(MAPL_Udunits_Converter), intent(inout) :: this
      type(c_ptr) :: ptr

      destroyed = .TRUE.
      if(is_null(this)) return
      ptr = this % ptr
      call cv_free(ptr)
      call this % set()
      destroyed = is_null(ptr)

   end function destroy_converter

   logical function are_convertible(unit1, unit2)
      type(c_ptr), intent(in) :: unit1, unit2
      integer(c_int), parameter :: ZERO = 0_c_int
      are_convertible = (ut_are_convertible(unit1, unit2) /= ZERO)
   end function are_convertible

   integer(ut_encoding) function get_encoding(encoding)
      integer(ut_encoding), optional, intent(in) :: encoding
      get_encoding = merge(encoding, UT_ENCODING_DEFAULT, present(encoding))
   end function get_encoding

end module udunits2mod
