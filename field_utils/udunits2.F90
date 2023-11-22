module udunits2mod

   ! The kinds and derived types that follow are needed for the following include files.
   use iso_c_binding, only:   c_char, c_int, c_short, c_float, c_double
   use iso_c_binding, only:   c_size_t, c_null_char, c_null_ptr
   use iso_c_binding, only:   c_ptr, c_funptr 
   implicit none

!================================ ENUMERATORS ==================================

   enum, bind(c)
      enumerator :: ENUM_TYPE = 0
   end enum

!=========================== UT_STATUS - ENUMERATOR ============================
! ut_status is actually an integer kind for enumerators
   enum, bind(c)
       enumerator :: &
       UT_SUCCESS = 0, & ! Success 
       UT_BAD_ARG, & ! An argument violates the function's contract 
       UT_EXISTS, & ! Unit, prefix, or identifier already exists 
       UT_NO_UNIT, & ! No such unit exists 
       UT_OS, & ! Operating-system error. See "errno". 
       UT_NOT_SAME_SYSTEM, & ! The units belong to different unit-systems 
       UT_MEANINGLESS, & ! The operation on the unit(s) is meaningless 
       UT_NO_SECOND, & ! The unit-system doesn't have a unit named "second" 
       UT_VISIT_ERROR, & ! An error occurred while visiting a unit 
       UT_CANT_FORMAT, & ! A unit can't be formatted in the desired manner 
       UT_SYNTAX, & ! string unit representation contains syntax error 
       UT_UNKNOWN, & ! string unit representation contains unknown word 
       UT_OPEN_ARG, & ! Can't open argument-specified unit database 
       UT_OPEN_ENV, & ! Can't open environment-specified unit database 
       UT_OPEN_DEFAULT, & ! Can't open installed, default, unit database 
       UT_PARSE_ERROR ! Error parsing unit specification 
   end enum
   integer, parameter :: ut_status = kind(ENUM_TYPE)
!============================== END - UT_STATUS ================================

!=========================== UT_ENCODING - ENUMERATOR ===========================
! UT_ENCODING is actually an integer kind for enumerators.
   enum, bind(c)
      enumerator :: UT_ASCII = 0
      enumerator :: UT_ISO_8859_1 = 1
      enumerator :: UT_LATIN1 = UT_ISO_8859_1
      enumerator :: UT_UTF8 = 2
   end enum
   integer, parameter :: ut_encoding = kind(ENUM_TYPE)
!=============================== END UT_ENCODING ================================

!=========================== UNITTYPE - ENUMERATOR =============================
! UnitType is actually an integer parameter = integer kind of enumerators
! So the type is: integer(UnitType)

   enum, bind(c)
      enumerator :: BASIC, PRODUCT_, GALILEAN, LOG_, TIMESTAMP
   end enum
   integer, parameter :: UnitType = kind(ENUM_TYPE)
!================================ END UnitType =================================

!============================== END ENUMERATORS ================================

!=================================== TYPES =====================================

!=================== TYPE: UT_UNIT - type to wrap C union ut_unit ==============
   type, bind(c) :: ut_unit
       type(c_ptr) :: ptr
   end type ut_unit
!================================ END UT_UNIT ==================================

!============== TYPE: CV_CONVERTER - type to wrap C union cv_converter =========
   type, bind(c) :: cv_converter
      type(c_ptr) :: ptr
   end type cv_converter
!============================== END CV_CONVERTER ===============================

!================================= TYPE: UT_SYSTEM =============================
! unit system
!   type, bind(c) :: ut_system 
!      type(ut_unit)  :: second
!      type(ut_unit)  :: one
!      integer(UnitType) :: basicUnits(:)
!      type(c_int) :: basicCount
!   end type ut_system
   type, bind(c) :: ut_system
      type(c_ptr) :: ptr
   end type ut_system
!=============================== END UT_SYSTEM =================================

!================================== TYPE: UNITOPTS =============================
! unit operations
!   type, bind(c) :: UnitOps
!      type(c_funptr) :: getProduct ! ProductUnit* :: (const ut_unit*)
!      type(c_funptr) :: clone ! ut_unit* :: (ut_unit*)
!      type(c_funptr) :: free ! void :: (ut_unit*)
!      type(c_funptr) :: compare ! int :: (ut_unit*, ut_unit*)
!      type(c_funptr) :: multiply ! ut_unit* :: (const ut_unit*, const ut_unit*)
!      type(c_funptr) :: raise ! ut_unit* :: (const ut_unit*, const int power)
!      type(c_funptr) :: root ! ut_unit* :: (const ut_unit*, const int root)
!      type(c_funptr) :: initConverterToProduct ! int :: (ut_unit*)
!      type(c_funptr) :: initConverterFromProduct ! int :: (ut_unit*)
!      type(c_funptr) :: acceptVisitor ! ut_status :: (const ut_unit*, const ut_visitor*, void*)
!   end type UnitOps
!================================ END UNITOPS ==================================

!================================== TYPE: COMMON_ ==============================
! COMMON_ is used instead of COMMON to avoid collision with Fortran "common"
!   type, bind(c) :: Common_
!       type(ut_system)  :: system
!       type(UnitOps)   :: ops
!       integer(UnitType)  :: type_ ! type_ is used to avoid collision
!       type(cv_converter)   :: toProduct
!       type(cv_converter)   :: fromProduct
!   end type Common_
!================================ END COMMON_ ==================================

!============================== TYPE: BASICUNIT ================================
! common__ is used to avoid collision with derived type Command_
!   type, bind(c) :: BasicUnit
!       type(Common_) :: common__
!       type(ProductUnit)    :: product_
!       type(c_int) :: index_
!       type(c_int) :: isDimensionless
!   end type BasicUnit
!=============================== END BASICUNIT =================================

!============================= TYPE: PRODUCTUNIT ===============================
! common__ is used to avoid collision with derived type Command_
!   type, bind(c) :: ProductUnit
!       type(Common_) :: common__
!       type(c_short) :: indexes(:)
!       type(c_short) :: powers(:)
!       type(c_int) :: count_
!   end type ProductUnit
!============================== END PRODUCTUNIT ================================
   
!============================= TYPE: GALILEANUNIT ==============================
! common__ is used to avoid collision with derived type Command_
!   type, bind(c) :: GalileanUnit
!       type(Common_) :: common__
!       type(ut_unit)   :: unit_
!       type(c_double) :: scale_
!       type(c_double) :: offset_
!   end type GalileanUnit
!============================= END GALILEANUNIT ================================

!============================ TYPE: TIMESTAMPUNIT ==============================
! common__ is used to avoid collision with derived type Command_
!   type, bind(c) :: TimestampUnit
!       type(Common_) :: common__
!       type(ut_unit)   :: unit_
!       type(c_double) :: origin
!   end type TimestampUnit
!============================= END TIMESTAMPUNIT ===============================
    
!=============================== TYPE: LOGUNIT =================================
! common__ is used to avoid collision with derived type Command_
!   type, bind(c) :: LogUnit
!       type(Common_) :: common__
!       type(ut_unit)   :: reference
!       type(c_double)   :: base
!   end type LogUnit
!================================ END LOGUNIT ==================================

!================================= END TYPES ===================================

!============================ PROCEDURE INTERFACES =============================

   interface

      ! Get last status
      integer(ut_status) function ut_get_status() &
         bind(c, name='ut_get_status')
         import :: ut_status
      end function ut_get_status
      
      ! Return non-zero value if unit1 can be converted to unit2, otherwise 0
      ! Use ut_get_status to check error condition. 
      ! UT_SUCCESS indicates that the function ran successfully, not that the units are convertible
      integer(c_int) function ut_are_convertible(unit1, unit2) &
         bind(c, name='ut_are_convertible')
         import :: c_int, ut_unit
         type(ut_unit), intent(in) :: unit1, unit2
      end function ut_are_convertible

      ! Return pointer wrapper for converter, NULL if error.
      ! Use ut_get_status to check error condition. 
      type(cv_converter) function ut_get_converter(from, to) &
         bind(c, name='ut_get_converter')
         import :: cv_converter, ut_unit
         type(ut_unit), intent(in) :: from, to
      end function ut_get_converter

      ! Use converter to convert value_
      real(c_float) function cv_convert_float(converter, value_) bind(c)
         import :: cv_converter, c_float
         type(cv_converter), intent(in) :: converter
         real(c_float), intent(in) :: value_
      end function cv_convert_float

      ! Use converter to convert value_
      real(c_double) function cv_convert_double(converter, value_) bind(c)
         import :: cv_converter, c_double
         type(cv_converter), intent(in) :: converter
         real(c_double), intent(in) :: value_
      end function cv_convert_double

      ! Use converter to convert in_ and put it in out_.
      subroutine cv_convert_doubles(converter, in_, count_, out_) &
         bind(c, name='cv_convert_doubles')
         import :: cv_converter, c_double, c_int, c_ptr
         type(cv_converter), intent(in) :: converter
         real(c_double), intent(in) :: in_(*)
         integer(c_int), intent(in) :: count_
         real(c_double), intent(out) :: out_(count_)
!         real(c_double) :: cv_convert_doubles(count_)
      end subroutine cv_convert_doubles

      ! Use converter to convert in_ and put it in out_.
      subroutine cv_convert_floats(converter, in_, count_, out_) &
         bind(c, name='cv_convert_floats')
         import :: cv_converter, c_float, c_int
         type(cv_converter), intent(in) :: converter
         real(c_float), intent(in) :: in_(*)
         integer(c_int), intent(in) :: count_
         real(c_float), intent(out) :: out_(count_)
!         real(c_float) :: cv_convert_floats(count_)
      end subroutine cv_convert_floats

      ! Use ut_get_status to check error condition. 
      type(ut_system) function ut_read_xml(path) bind(c, name='ut_read_xml')
         import :: ut_system, c_char, c_ptr
         type(c_ptr), intent(in) :: path
      end function ut_read_xml

      ! Use ut_get_status to check error condition. 
      type(ut_unit) function ut_parse(system, string, encoding) bind(c, name='ut_parse')
         import :: ut_unit, ut_system, ut_encoding, c_char
         type(ut_system), intent(in) :: system
         character(c_char), intent(in) ::  string
         integer(ut_encoding), intent(in) :: encoding
      end function ut_parse

!      subroutine ut_free(unit_) bind(c, name='ut_free')
!         import :: ut_unit
!         type(ut_unit), intent(inout) :: unit_
!      end subroutine ut_free

!      subroutine ut_free_system(system) bind(c, name='ut_free_system')
!         import :: ut_system
!         type(ut_system), intent(inout) :: system
!      end subroutine ut_free_system

!      type(ut_status) function ut_set_second(second) bind(c, name='ut_set_second')
!         import :: ut_status, ut_unit
!         type(ut_unit), intent(inout) :: second
!      end function ut_second_second
!
!      subroutine cv_free(conv) bind(c, name='cv_free')
!         import :: cv_converter
!         type(cv_converter), intent(inout) :: conv
!      end subroutine cv_free

!      type(ut_unit) function ut_get_unit_by_name(system, name_) bind(c, name='ut_get_unit_by_name')
!         import :: ut_unit, ut_system, c_char
!         type(ut_system), intent(in) :: system
!         character(kind=c_char, len=MAXLEN), intent(in) :: name_
!      end function ut_get_unit_by_name

!      type(ut_unit) function ut_get_unit_by_symbol(system, symbol) bind(c, name='ut_get_unit_by_symbol')
!         import :: ut_unit, ut_system, c_char
!         type(ut_system), intent(in) :: system
!         character(kind=c_char, len=MAXLEN), intent(in) :: symbol
!      end function ut_get_unit_by_symbol

!      type(ut_unit) function ut_get_dimensionless_unit_one(system) bind(c, name='ut_get_dimensionless_unit_one')
!         import :: ut_unit, ut_system
!         type(ut_system), intent(in) :: system
!      end function ut_get_dimensionless_unit_one

   end interface

!========================== END PROCEDURE INTERFACES ===========================

end module udunits2mod
