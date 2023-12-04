#if defined TRIMALL(S)
#undef TRIMALL(S)
#endif
#define TRIMALL(S) trim(adjustl(S))

module udunits2mod

   use iso_c_binding, only: c_char, c_int, c_float, c_double, c_ptr, c_null_ptr

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
      procedure, public, deferred, pass(this) :: destroy
      generic, public :: operator(==) => equals_c_ptr
   end type Cwrap

!=========================== MAPL_UDUNITSCONVERTER =============================
   type, extends(Cwrap) :: MAPL_Udunits_Converter
   contains
      procedure, public, pass(this) :: destroy => destroy_converter
      procedure, public, pass(this) :: convert_double
      procedure, public, pass(this) :: convert_float
      procedure, public, pass(this) :: convert_doubles
      procedure, public, pass(this) :: convert_floats
      generic :: convert => &
         convert_double, convert_float, convert_doubles, convert_floats
   end type MAPL_Udunits_Converter

   interface MAPL_Udunits_Converter
      module procedure :: get_converter
   end interface MAPL_Udunits_Converter

!============================ MAPL_UDUNITS_SYSTEM ==============================
   type, extends(Cwrap) :: MAPL_Udunits_System
      procedure, public, pass(this) :: destroy => destroy_system
   end type MAPL_Udunits_System

!================================= OPERATORS ===================================
   interface operator(=)
      module procedure :: assign_from_cwrap
      module procedure :: assign_to_cwrap
   end interface

   type(MAPL_Udunits_System) :: SYSTEM_INSTANCE

!================================= PROCEDURES ==================================
contains

   subroutine assign_to_cwrap(cwrap_, ptr)
      class(Cwrap), intent(inout) :: cwrap_
      type(c_ptr), intent(in) :: ptr

      cwrap_ % ptr = ptr

   end subroutine assign_to_cwrap_ptr

   type(c_ptr) function assign_from_cwrap(cwrap_)
      class(Cwrap), intent(in) :: cwrap_

      assign_from_cwrap = cwrap_ % ptr

   end subroutine assign_from_cwrap

   logical function cwrap_equals_c_ptr(this, ptr)
      class(Cwrap), intent(in) :: cwrap_
      type(c_ptr), intent(in) :: ptr

      cwrap_equals_c_ptr = (cwrap_ % ptr == ptr)
      
   end function cwrap_equals_c_ptr

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
      get_converter = ut_get_converter(from_unit, to_unit)
      from_destroyed = destroy_ut_unit(from_unit)
      to_destroyed = destroy_ut_unit(from_unit)

   end function get_converter

   function convert_double(this, from) result(to)
      type(MAPL_Udunits_Converter), intent(in) :: this
      real(c_double), intent(in) :: from
      real(c_double) :: to
      type(c_ptr) :: cv_converter

      cv_converter = this

      to = cv_convert_double(cv_converter, from)

   end function convert_double

   function convert_float(this, from) result(to)
      type(MAPL_Udunits_Converter), intent(in) :: this
      real(c_float), intent(in) :: from
      real(c_float) :: to
      type(c_ptr) :: cv_converter

      cv_converter = this

      to = cv_convert_float(cv_converter, from)

   end function convert_float

   subroutine convert_doubles(this, from) result(to)
      type(MAPL_Udunits_Converter), intent(in) :: this
      real(c_double), intent(in) :: from(:)
      real(c_double), intent(out) :: to(size(from))
      type(c_ptr) :: cv_converter

      cv_converter = this

      call cv_convert_doubles(cv_converter, from, size(from), to)

   end subroutine convert_doubles

   function convert_floats(this, from) result(to)
      type(MAPL_Udunits_Converter), intent(in) :: this
      real(c_float), intent(in) :: from(:)
      real(c_float) :: to(:)
      type(c_ptr) :: cv_converter

      cv_converter = this

      call cv_convert_floats(cv_converter, from, size(from), to)

   end function convert_floats

   function initialize(path)
      character(len=*), optional, intent(in) :: path
      type(c_ptr) :: initialize

      if(SYSTEM_INSTANCE == c_null_ptr) then
         SYSTEM_INSTANCE = get_ut_system(path)
      end if
      initialize = SYSTEM_INSTANCE

   end function initialize

   type(c_ptr) function get_ut_system(path)
      character(len=*), optional, intent(in) :: path

      if(present(path)) then
         get_ut_system = ut_read_xml(TRIMALL(path) // c_null_ptr)
      else
         get_ut_system = ut_read_xml(c_null_ptr)
      end if
               
   end function get_ut_system

   logical function destroy_ut_unit(ut_unit_ptr) result(destroyed)
      type(c_ptr), intent(in) :: ut_unit_ptr
      
      destroyed = .TRUE.
      if(ut_unit_ptr == c_null_ptr) return
      call ut_free(ut_unit_ptr)
      destroyed=(ut_unit_ptr == c_null_ptr)

   end function destroy_ut_unit

   logical function destroy_all() result(destroyed)
      destroyed = .TRUE.
      destroyed = SYSTEM_INSTANCE.destroy()
   end function destroy_all

   logical function destroy_system(this) result(destroyed)
      type(MAPL_Udunits_System), intent(in) :: this
      type(c_ptr) :: ut_system_ptr 

      destroyed = .TRUE.
      if(this == c_null_ptr) return
      ut_system_ptr = this
      call ut_free_system(ut_system_ptr)
      destroyed = (ut_system_ptr == c_null_ptr) 

   end function destroy_ut_system

   logical function destroy_converter(conv) result(destroyed)
      type(MAPL_Udunits_Converter), intent(in) :: conv
      type(c_ptr) :: ptr

      destroyed = .TRUE.
      if(conv == c_null_ptr) return
      ptr = conv
      call cv_free(ptr)
      destroyed = (conv == c_null_ptr)

   end function destroy_converter

   logical are_convertible(unit1, unit2)
      type(c_ptr), intent(in) :: unit1, unit2
      integer(c_int), parameter :: ZERO = 0_c_int
      are_convertible = (ut_are_convertible(unit1, unit2) /= ZERO)
   end function are_convertible

   integer(ut_encoding) function get_encoding(encoding)
      integer(ut_encoding), optional, intent(in) :: encoding
      get_encoding = merge(encoding, UT_ENCODING_DEFAULT, present(encoding))
   end function get_encoding

end module udunits2mod
