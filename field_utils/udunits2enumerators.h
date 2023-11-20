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
       UT_PARSE ! Error parsing unit specification 
   end enum
   integer, parameter :: ut_status = kind(ENUM_TYPE)
!============================== END - UT_STATUS ================================

!=========================== UTENCODING - ENUMERATOR ===========================
! utEncoding is actually an integer kind for enumerators.
   enum, bind(c)
      enumerator :: UT_ASCII = 0
      enumerator :: UT_ISO_8859_1 = 1
      enumerator :: UT_LATIN1 = UT_ISO_8859_1
      enumerator :: UT_UTF8 = 2
   end enum
   integer, parameter :: utEncoding = kind(ENUM_TYPE)
!=============================== END UTENCODING ================================

!=========================== UNITTYPE - ENUMERATOR =============================
! UnitType is actually an integer parameter = integer kind of enumerators
! So the type is: integer(UnitType)

   enum, bind(c)
      enumerator :: BASIC, PRODUCT_, GALILEAN, LOG_, TIMESTAMP
   end enum
   integer, parameter :: UnitType = kind(ENUM_TYPE)
!================================ END UnitType =================================

!============================== END ENUMERATORS ================================
! vim: filetype=fortran
