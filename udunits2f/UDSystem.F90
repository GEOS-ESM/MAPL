#include "error_handling.h"

module ud2f_UDSystem
   use ud2f_CptrWrapper
   use ud2f_interfaces
   use ud2f_encoding
   use ud2f_status_codes
   use iso_c_binding, only: c_ptr, c_associated, c_null_ptr, c_null_char
   use iso_c_binding, only: c_char, c_int, c_float, c_double, c_loc
   implicit none
   private

   public :: Converter
   public :: get_converter
   public :: initialize
   public :: finalize

   public :: UDUnit
   public :: are_convertible
   public :: UDSystem
   public :: cstring
   public :: read_xml
   public :: ut_free_system

!================================= CONVERTER ===================================
! Converter object to hold convert functions for an (order) pair of units
   type, extends(CptrWrapper) :: Converter
      private
   contains
      procedure :: free_memory => free_cv_converter
      procedure, private :: convert_float_0d
      procedure, private :: convert_float_1d
      procedure, private :: convert_float_2d
      procedure, private :: convert_float_3d
      procedure, private :: convert_float_4d
      procedure, private :: convert_float_5d
      procedure, private :: convert_double_0d
      procedure, private :: convert_double_1d
      procedure, private :: convert_double_2d
      procedure, private :: convert_double_3d
      procedure, private :: convert_double_4d
      procedure, private :: convert_double_5d

      generic :: convert => convert_float_0d
      generic :: convert => convert_float_1d
      generic :: convert => convert_float_2d
      generic :: convert => convert_float_3d
      generic :: convert => convert_float_4d
      generic :: convert => convert_float_5d
      generic :: convert => convert_double_0d
      generic :: convert => convert_double_1d
      generic :: convert => convert_double_2d
      generic :: convert => convert_double_3d
      generic :: convert => convert_double_4d
      generic :: convert => convert_double_5d
   end type Converter

   interface Converter
      module procedure :: construct_converter
   end interface Converter

!=============================== UDSYSTEM =================================
! udunits2 unit system: encoding is the encoding for unit names and symbols.
   type, extends(CptrWrapper) :: UDSystem
      private
      integer(ut_encoding) :: encoding = UT_ASCII
   contains
      procedure, public, pass(this) :: free_memory => free_ut_system
   end type UDSystem

   interface UDSystem
      module procedure :: construct_system
   end interface UDSystem

!=================================== UDUNIT ====================================
! measurement unit in udunits2 system
   type, extends(CptrWrapper) :: UDUnit
   contains
      procedure, public, pass(this) :: free_memory => free_ut_unit
   end type UDUnit

   interface UDUnit
      module procedure :: construct_unit
   end interface UDUnit

   interface are_convertible
      procedure :: are_convertible_udunit
      procedure :: are_convertible_str
   end interface are_convertible

!============================= INSTANCE VARIABLES ==============================
! Single instance of units system. There is one system in use, only.
   type(UDSystem), private :: SYSTEM_INSTANCE

contains

   ! Check the status for the last udunits2 call
   logical function success(utstatus)
      integer(ut_status) :: utstatus

      success = (utstatus == UT_SUCCESS)

   end function success

   function construct_system(path, encoding) result(instance)
      type(UDsystem) :: instance
      character(len=*), optional, intent(in) :: path
      integer(ut_encoding), optional, intent(in) :: encoding
      type(c_ptr) :: utsystem
      integer(ut_status) :: status

      ! Read in unit system from path
      call read_xml(path, utsystem, status)
      
      if(success(status)) then
         call instance%set_cptr(utsystem)
         if(present(encoding)) instance%encoding = encoding
         return
      end if
         
      ! Free memory in the case of failure
      if(c_associated(utsystem)) call ut_free_system(utsystem)

   end function construct_system

   function construct_unit(identifier) result(instance)
      type(UDUnit) :: instance
      character(len=*), intent(in) :: identifier
      character(kind=c_char, len=:), allocatable :: cchar_identifier
      type(c_ptr) :: utunit1

      ! Unit system must be initialized (instantiated).
      if(instance_is_uninitialized()) return

      cchar_identifier = cstring(identifier)
      utunit1 = ut_parse(SYSTEM_INSTANCE%get_cptr(), cchar_identifier, SYSTEM_INSTANCE%encoding)

      if(success(ut_get_status())) then
         call instance%set_cptr(utunit1)
      else
         ! Free memory in the case of failure
         if(c_associated(utunit1)) call ut_free(utunit1)
      end if

   end function construct_unit

   function construct_converter(from_unit, to_unit) result(conv)
      type(Converter) :: conv
      type(UDUnit), intent(in) :: from_unit
      type(UDUnit), intent(in) :: to_unit
      type(c_ptr) :: cvconverter1
      logical :: convertible

      ! Must supply units that are initialized and convertible
      if(from_unit%is_free() .or. to_unit%is_free()) return
      if(.not. are_convertible(from_unit, to_unit)) return

      cvconverter1 = ut_get_converter(from_unit%get_cptr(), to_unit%get_cptr())

      if(success(ut_get_status())) then
         call conv%set_cptr(cvconverter1)
      else
         ! Free memory in the case of failure
         if(c_associated(cvconverter1)) call cv_free(cvconverter1)
      end if

   end function construct_converter

   ! Get Converter object based on unit names or symbols
   subroutine get_converter(conv, from, to, rc)
      type(Converter),intent(inout) :: conv
      character(len=*), intent(in) :: from, to
      integer(ut_status), optional, intent(out) :: rc
      integer(ut_status) :: status

      conv = get_converter_function(from, to)
      _ASSERT(.not. conv%is_free(), UTF_CONVERTER_NOT_INITIALIZED)

      _RETURN(UT_SUCCESS)
   end subroutine get_converter

   ! Get converter object
   function get_converter_function(from, to) result(conv)
      type(Converter) :: conv
      character(len=*), intent(in) :: from, to
      type(UDUnit) :: from_unit
      type(UDUnit) :: to_unit

      ! Unit system must be initialized (instantiated).
      if(instance_is_uninitialized()) return

      ! Get units based on strings. Free memory on fail.
      from_unit = UDUnit(from)
      if(from_unit%is_free()) return
      to_unit = UDUnit(to)
      if(to_unit%is_free()) then
         call from_unit%free()
         return
      end if

      conv = Converter(from_unit, to_unit)

      ! Units are no longer needed
      call from_unit%free()
      call to_unit%free()

   end function get_converter_function

   function convert_float_0d(this, from) result(to)
      class(Converter), intent(in) :: this
      real(c_float), intent(in) :: from
      real(c_float) :: to
      to = cv_convert_float(this%get_cptr(), from)
   end function convert_float_0d

   function convert_float_1d(this, from) result(to)
      class(Converter), intent(in) :: this
      real(c_float), intent(in) :: from(:)
      real(c_float) :: to(size(from))
      call cv_convert_floats(this%get_cptr(), from, size(from), to)
   end function convert_float_1d

   function convert_float_2d(this, from) result(to)
      class(Converter), intent(in) :: this
      real(c_float), intent(in) :: from(:,:)
      real(c_float) :: to(size(from,1), size(from,2))
      call cv_convert_floats(this%get_cptr(), from, size(from), to)
   end function convert_float_2d

   function convert_float_3d(this, from) result(to)
      class(Converter), intent(in) :: this
      real(c_float), intent(in) :: from(:,:,:)
      real(c_float) :: to(size(from,1), size(from,2), size(from,3))
      call cv_convert_floats(this%get_cptr(), from, size(from), to)
   end function convert_float_3d

   function convert_float_4d(this, from) result(to)
      class(Converter), intent(in) :: this
      real(c_float), intent(in) :: from(:,:,:,:)
      real(c_float) :: to(size(from,1), size(from,2), size(from,3), size(from,4))
      call cv_convert_floats(this%get_cptr(), from, size(from), to)
   end function convert_float_4d

   function convert_float_5d(this, from) result(to)
      class(Converter), intent(in) :: this
      real(c_float), intent(in) :: from(:,:,:,:,:)
      real(c_float) :: to(size(from,1), size(from,2), size(from,3), size(from,4), size(from,5))
      call cv_convert_floats(this%get_cptr(), from, size(from), to)
   end function convert_float_5d

   function convert_double_0d(this, from) result(to)
      class(Converter), intent(in) :: this
      real(c_double), intent(in) :: from
      real(c_double) :: to
      to = cv_convert_double(this%get_cptr(), from)
   end function convert_double_0d

   function convert_double_1d(this, from) result(to)
      class(Converter), intent(in) :: this
      real(c_double), intent(in) :: from(:)
      real(c_double) :: to(size(from))
      call cv_convert_doubles(this%get_cptr(), from, size(from), to)
   end function convert_double_1d

   function convert_double_2d(this, from) result(to)
      class(Converter), intent(in) :: this
      real(c_double), intent(in) :: from(:,:)
      real(c_double) :: to(size(from,1), size(from,2))
      call cv_convert_doubles(this%get_cptr(), from, size(from), to)
   end function convert_double_2d

   function convert_double_3d(this, from) result(to)
      class(Converter), intent(in) :: this
      real(c_double), intent(in) :: from(:,:,:)
      real(c_double) :: to(size(from,1), size(from,2), size(from,3))
      call cv_convert_doubles(this%get_cptr(), from, size(from), to)
   end function convert_double_3d

   function convert_double_4d(this, from) result(to)
      class(Converter), intent(in) :: this
      real(c_double), intent(in) :: from(:,:,:,:)
      real(c_double) :: to(size(from,1), size(from,2), size(from,3), size(from,4))
      call cv_convert_doubles(this%get_cptr(), from, size(from), to)
   end function convert_double_4d

   function convert_double_5d(this, from) result(to)
      class(Converter), intent(in) :: this
      real(c_double), intent(in) :: from(:,:,:,:,:)
      real(c_double) :: to(size(from,1), size(from,2), size(from,3), size(from,4), size(from,5))
      call cv_convert_doubles(this%get_cptr(), from, size(from), to)
   end function convert_double_5d

   ! Read unit database from XML
   subroutine read_xml(path, utsystem, status)
      character(len=*), optional, intent(in) :: path
      type(c_ptr), intent(out) :: utsystem
      integer(ut_status), intent(out) :: status

      character(kind=c_char, len=:), target, allocatable :: cchar_path

      if(present(path)) then
         cchar_path = cstring(path)
         utsystem = ut_read_xml_cptr(c_loc(cchar_path))
      else
         utsystem = ut_read_xml_cptr(c_null_ptr)
      end if
      status = ut_get_status()

   end subroutine read_xml

   ! Initialize unit system instance
   subroutine initialize(path, encoding, rc)
      character(len=*), optional, intent(in) :: path
      integer(ut_encoding), optional, intent(in) :: encoding
      integer, optional, intent(out) :: rc
      integer :: status

      _RETURN_UNLESS(instance_is_uninitialized())
      ! System must be once and only once.
      _ASSERT(instance_is_uninitialized(), UTF_DUPLICATE_INITIALIZATION)

      ! Disable error messages from udunits2
      call disable_ut_error_message_handler()

      call initialize_system(SYSTEM_INSTANCE, path, encoding, rc=status)
      if(status /= UT_SUCCESS) then
         ! On failure, free memory
         call finalize()
         _RETURN(UTF_INITIALIZATION_FAILURE)
      end if
      _ASSERT(.not. SYSTEM_INSTANCE%is_free(), UTF_NOT_INITIALIZED)
      _RETURN(UT_SUCCESS)

   end subroutine initialize

   subroutine initialize_system(system, path, encoding, rc)
      type(UDSystem), intent(inout) :: system
      character(len=*), optional, intent(in) :: path
      integer(ut_encoding), optional, intent(in) :: encoding
      integer, optional, intent(out) :: rc
      integer :: status
      type(c_ptr) :: utsystem

      ! A system can be initialized only once.
      _ASSERT(system%is_free(), UTF_DUPLICATE_INITIALIZATION)

      system = UDSystem(path, encoding)
      _RETURN(UT_SUCCESS)
   end subroutine initialize_system

   ! Is the instance of the unit system initialized?
   logical function instance_is_uninitialized()
      
      instance_is_uninitialized = SYSTEM_INSTANCE%is_free()
      
   end function instance_is_uninitialized

   ! Free memory for unit system
   subroutine free_ut_system(this)
      class(UDSystem), intent(in) :: this
        
      if(this%is_free()) return
      call ut_free_system(this%get_cptr())

   end subroutine free_ut_system

   ! Free memory for unit
   subroutine free_ut_unit(this)
      class(UDUnit), intent(in) :: this

      if(this%is_free()) return
      call ut_free(this%get_cptr())

   end subroutine free_ut_unit

   ! Free memory for converter
   subroutine free_cv_converter(this)
      class(Converter), intent(in) :: this
      type(c_ptr) :: cvconverter1 

      if(this%is_free()) return
      call cv_free(this%get_cptr())

   end subroutine free_cv_converter

   ! Free memory for unit system instance
   subroutine finalize()

      if(SYSTEM_INSTANCE%is_free()) return
      call SYSTEM_INSTANCE%free()

   end subroutine finalize

   ! Check if units are convertible
   function are_convertible_udunit(unit1, unit2, rc) result(convertible)
      logical :: convertible
      type(UDUnit), intent(in) :: unit1, unit2
      integer, optional, intent(out) :: rc
      integer :: status
      integer(c_int), parameter :: ZERO = 0_c_int
      
      convertible = (ut_are_convertible(unit1%get_cptr(), unit2%get_cptr())  /= ZERO)
      status = ut_get_status()
      _ASSERT(success(status), status)

      _RETURN(UT_SUCCESS)
   end function are_convertible_udunit

   ! Check if units are convertible
   function are_convertible_str(from, to, rc) result(convertible)
      logical :: convertible
      character(*), intent(in) :: from, to
      integer, optional, intent(out) :: rc

      integer :: status
      type(UDUnit) :: unit1, unit2

      unit1 = UDUnit(from)
      unit2 = UDUnit(to)
      convertible = are_convertible_udunit(unit1, unit2, _RC)

      _RETURN(UT_SUCCESS)
   end function are_convertible_str

   ! Create C string from Fortran string
   function cstring(s) result(cs)
      character(len=*), intent(in) :: s
      character(kind=c_char, len=:), allocatable :: cs

      cs = adjustl(trim(s)) // c_null_char

   end function cstring

   ! Set udunits2 error handler to ut_ignore which does nothing
   subroutine disable_ut_error_message_handler(is_set)
      logical, optional, intent(out) :: is_set
      logical, save :: handler_set = .FALSE.

      if(.not. handler_set) call ut_set_ignore_error_message_handler()
      handler_set = .TRUE.
      if(present(is_set)) is_set = handler_set
   end subroutine disable_ut_error_message_handler

end module ud2f_UDSystem
