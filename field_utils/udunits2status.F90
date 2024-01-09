module udunits2status

   implicit none

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
       UT_PARSE_ERROR, & ! Error parsing unit specification
       UT_FAILURE = UT_SUCCESS - 1
   end enum
   integer, parameter :: ut_status = kind(UT_SUCCESS)

end module udunits2status
