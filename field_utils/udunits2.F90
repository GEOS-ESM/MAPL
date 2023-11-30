module udunits2mod

   use, intrinsic :: iso_fortran_env, only: R64 => real64, R32 => real32
   use iso_c_binding, only:   c_char, c_int, c_float, c_double, c_ptr

   implicit none

   public :: udunits2initialize => initialize
   public :: udunits2converter => get_converter
   !private

   include 'udunits2enumerators.h'

!=================================== TYPES =====================================

   type, abstract :: CPT
      type(c_ptr) :: ptr_ = c_null_ptr
   contains
      procedure, public, pass(this) :: is_null => cpt_is_null
      procedure, public, pass(this) :: ptr => cpt_ptr
      procedure, public, deferred, pass(this) :: finalize
   end type CPT
   
!=================== TYPE: UT_UNIT - type to wrap C union ut_unit ==============
   type, extends(CPT) :: ut_unit
   contains
      procedure, public, pass(this) :: finalize => finalize_ut_unit
   end type ut_unit
   
   interface ut_unit
      module procedure :: construct_ut_unit_from_string
   end interface ut_unit
!================================ END UT_UNIT ==================================

!============== TYPE: CV_CONVERTER - type to wrap C union cv_converter =========
   type, extends(CPT) :: cv_converter
   contains
      procedure, private, pass(this) :: finalize => finalize_cv_converter
   end type cv_converter
   
   interface cv_converter
      procedure, public, pass(this) :: construct_cv_converter
   end interface cv_converter
!============================== END CV_CONVERTER ===============================

!================================= TYPE: UT_SYSTEM =============================
! unit system
   type, extends(CPT) :: ut_system
   contains
      procedure, public, pass(this) :: finalize => finalize_ut_system
      procedure, public, pass(this) :: is_initialized => &
         ut_system_is_initialized
   end type ut_system

   interface ut_system
      module procedure :: construct_ut_system_path
      module procedure :: construct_ut_system_no_path
   end interface ut_system
!=============================== END UT_SYSTEM =================================

!================================= CONVERTER ===================================
   type :: Converter
      private
      type(cv_converter) :: conv_
      logical :: is_null_
   contains
      procedure, public, pass(this) :: is_null
      procedure, public, pass(this) :: convert_double
      procedure, public, pass(this) :: convert_float
      procedure, public, pass(this) :: convert_doubles
      procedure, public, pass(this) :: convert_floats
      generic :: convert => convert_double, convert_float, convert_doubles, convert_floats
   end type Converter

   interface Converter
      module procedure :: construct_null_converter
   end interface Converter
!============================== END - CONVERTER ================================

!================================= END TYPES ===================================

include "udunits2interfaces.h"

   type(ut_system) :: unit_system = ut_system(c_null_ptr)

   interface get_converter
      module procedure :: get_converter_from_strings
   end interface get_converter
      
   interface convert
      module procedure :: convertR64
      module procedure :: convertR32
   end interface convert

   integer, parameter :: SUCCESS = 0
   integer, parameter :: FAILURE = SUCCESS - 1
   integer(ut_encoding), parameter :: UT_ENCODING_DEFAULT = UT_ASCII
   character(len=*), parameter :: EMPTY = ''

contains

   logical function cpt_is_null(this)
      type(CPT), intent(in) :: this
      cpt_is_null = (this % ptr() == c_null_ptr)
   end function cpt_is_null

   type(ptr) function cpt_ptr(this)
      type(CPT), intent(in) :: this
      cpt_ptr = this % ptr_
   end function cpt_ptr

   subroutine finalize_ut_unit(this)
      type(ut_unit), intent(in) :: this
      call ut_free(this % ptr())
   end subroutine finalize_ut_unit

   subroutine finalize_cv_converter(this)
      type(cv_converter), intent(in) :: this
      call cv_free(this % ptr())
   end subroutine finalize_cv_converter

   subroutine finalize_ut_system(this)
      type(ut_system), intent(in) :: this
      call ut_free_system(this % ptr())
   end subroutine finalize_ut_system

   subroutine initialize(path)
      character(len=*), optional, intent(in) :: path
      character(len=len(path)) :: path_

      if(unit_system.is_null()) then
         if(present(path)) then
            path_ = path
         else
            path_ = EMPTY
         end if
         unit_system = ut_system(path_)
      end if

   end subroutine initialize

   function construct_cv_converter(usfrom, usto) result(conv)
      character(len=*), intent(in) :: usfrom, usto
      type(cv_converter) :: conv
      type(c_ptr) :: from, to
      type(ut_unit) :: fromunit, tounit

      fromunit = ut_unit
      conv = cv_converter(ut_get_converter(from, to))

   end function construct_cv_converter

   function construct_ut_system_path(path) result(usys)
      character(len=*), intent(in) :: path
      type(ut_system) :: usys

      usys = ut_system(ut_read_xml(trim(adjustl(path)) // c_null_ptr))

   end function construct_ut_system_path

   function construct_ut_system_no_path() result(usys)
      type(ut_system) :: usys

      usys = ut_system(ut_read_xml(c_null_ptr))

   end function construct_ut_system_no_path

   function construct_ut_unit(usys, string, encoding) result(uwrap)
      type(ut_system), intent(in) :: usys
      character(len=*), intent(in) :: string
      integer(ut_encoding), optional, intent(in) :: encoding
      type(ut_unit) :: uwrap
      integer(ut_encoding) :: encoding_

      encoding_ = merge(encoding, UT_ENCODING_DEFAULT)
      uwrap = ut_unit(ut_parse(usys % ptr(), &
         trim(adjustl(string)) // c_null_ptr, encoding_))

   end function construct_ut_unit

   integer function status(condition)
      logical, intent(in) :: condition
      status = merge(SUCCESS, ut_get_status(), condition)
   end function status

   logical are_convertible(unit1, unit2)
      type(ut_unit), intent(in) :: unit1, unit2
      are_convertible = c_true(ut_are_convertible(unit1 % ptr(), unit2 % ptr()))
   end function are_convertible

   logical function c_true(n)
      integer(c_int), intent(in) :: n
      true = (n /= 0)
   end function c_true

   elemental real(R64) function convertR64(from, conv, path)
      real(R64), intent(in) :: from
      type(cv_converter), intent(in) :: conv
      character(len=*), optional, intent(in) :: path
      
      convertR64 = cv_convert_double(conv, from)

   end function convertR64

   elemental real(R32) function convertR32(from, conv, path)
      real(R32), intent(in) :: from
      type(cv_converter), intent(in) :: conv
      character(len=*), optional, intent(in) :: path
      
      convertR32 = cv_convert_float(conv, from)

   end function convertR32

   type(Converter) function construct_converter() result(conv)
      conv = Converter(cv_converter(c_null_ptr), .TRUE.)
   end function construct_converter

   type(Converter) function get_converter_from_strings(u1string, u2string, path) result(convtr)
      character(len=*), intent(in) :: u1string, u2string
      character(len=*), optional, intent(in) :: path
   end function get_converter_from_strings

   logical function is_null(this)
      type(Converter), intent(in) :: this
      is_null = this % is_null_
   end function is_null

end module udunits2mod
