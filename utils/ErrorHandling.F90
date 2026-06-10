#include "MAPL.h"

module mapl_ErrorHandling_mod

   use mapl_Throw_mod

   implicit none
   private

   public :: MAPL_Assert
   public :: MAPL_Verify
   public :: MAPL_Return
   public :: MAPL_Deprecated
   public :: MAPL_SetFailOnDeprecated
   ! Legacy
   public :: MAPL_Abort
   public :: MAPL_set_abort_handler

   abstract interface
      subroutine abort_handler_interface()
      end subroutine abort_handler_interface
   end interface

   procedure(abort_handler_interface), pointer :: abort_handler => null()

   public :: MAPL_SUCCESS

   public :: MAPL_UNKNOWN_ERROR
   public :: MAPL_NO_SUCH_PROPERTY
   public :: MAPL_NO_SUCH_VARIABLE
   public :: MAPL_TYPE_MISMATCH
   public :: MAPL_UNSUPPORTED_TYPE

   public :: MAPL_VALUE_NOT_SUPPORTED
   public :: MAPL_NO_DEFAULT_VALUE
   public :: MAPL_DUPLICATE_KEY
   public :: MAPL_STRING_TOO_SHORT

   enum, bind(c)
      enumerator :: MAPL_SUCCESS       = 0

      ! 001-005
      enumerator :: MAPL_UNKNOWN_ERROR
      enumerator :: MAPL_NO_SUCH_PROPERTY
      enumerator :: MAPL_NO_SUCH_VARIABLE
      enumerator :: MAPL_TYPE_MISMATCH
      enumerator :: MAPL_UNSUPPORTED_TYPE

      ! 006-010
      enumerator :: MAPL_VALUE_NOT_SUPPORTED
      enumerator :: MAPL_NO_DEFAULT_VALUE
      enumerator :: MAPL_DUPLICATE_KEY
      enumerator :: MAPL_STRING_TOO_SHORT
   end enum

   interface MAPL_Assert
      module procedure MAPL_Assert_condition
      module procedure MAPL_Assert_return_code
   end interface MAPL_Assert

   logical, save :: FAIL_ON_DEPRECATED = .false.

contains

   logical function MAPL_Assert_condition(condition, message, return_code, filename, line, rc) result(fail)
      logical, intent(in) :: condition
      character(*), intent(in) :: message
      integer, intent(in) :: return_code
      character(*), intent(in) :: filename
      integer, intent(in) :: line
      integer, optional, intent(out) :: rc ! Not present in MAIN

      fail = .not. condition

      if (fail) then
         !$omp critical (MAPL_ErrorHandling1)
         call MAPL_throw_exception(filename, line, message=message)
         !$omp end critical (MAPL_ErrorHandling1)
         if (present(rc)) rc = return_code
      end if
   end function MAPL_Assert_Condition

   logical function MAPL_Assert_return_code(condition, return_code, filename, line, rc) result(fail)
      logical, intent(in) :: condition
      integer, intent(in) :: return_code
      character(*), intent(in) :: filename
      integer, intent(in) :: line
      integer, optional, intent(out) :: rc ! Not present in MAIN
      character(:), allocatable :: message

      fail = .not. condition

      if (fail) then
         message = get_error_message(return_code)
         !$omp critical (MAPL_ErrorHandling2)
         call MAPL_throw_exception(filename, line, message=message)
         !$omp end critical (MAPL_ErrorHandling2)
         if (present(rc)) rc = return_code
      end if
   end function MAPL_Assert_return_code

   logical function MAPL_Verify(status, filename, line, rc) result(fail)
      integer, intent(in) :: status
      character(*), intent(in) :: filename
      integer, intent(in) :: line
      integer, optional, intent(out) :: rc ! Not present in MAIN

      logical :: condition
      character(:), allocatable :: message
      character(16) :: status_string

      condition = (status == 0)
      fail = .not. condition

      if (fail) then
         write(status_string,'(i0)') status
         message = 'status=' // trim(status_string)
         !$omp critical (MAPL_ErrorHandling3)
         call MAPL_throw_exception(filename, line, message=message)
         !$omp end critical (MAPL_ErrorHandling3)
         if (present(rc)) rc = status
      end if
   end function MAPL_Verify

   subroutine MAPL_Return(status, filename, line, rc)
      integer, intent(in) :: status
      character(*), intent(in) :: filename
      integer, intent(in) :: line
      integer, intent(out), optional :: rc

      logical :: condition, fail
      character(:), allocatable :: message

      condition = (status == 0)
      fail = .not. condition

      if (fail) then
         message = get_error_message(status)
         !$omp critical (MAPL_ErrorHandling4)
         call MAPL_throw_exception(filename, line, message=message)
         !$omp end critical (MAPL_ErrorHandling4)
      end if
      ! Regardless of error:
      if (present(rc)) rc = status
   end subroutine MAPL_Return

   subroutine MAPL_Deprecated(file_name, module_name, procedure_name, rc)
      use, intrinsic :: iso_fortran_env, only: ERROR_UNIT
      character(*), intent(in) :: file_name
      character(*), intent(in) :: module_name
      character(*), intent(in) :: procedure_name
      integer, optional, intent(out) :: rc

      integer :: status

      write(ERROR_UNIT,*,iostat=status) "Invoking deprecated procedure: ", procedure_name
      _VERIFY(status)
      write(ERROR_UNIT,*,iostat=status) "    ...             in module: ", module_name
      _VERIFY(status)
      write(ERROR_UNIT,*,iostat=status) "    ...               in file: ", file_name
      _VERIFY(status)

      _ASSERT(.not. FAIL_ON_DEPRECATED, "    ... aborting.")
      _RETURN(_SUCCESS)
   end subroutine MAPL_Deprecated

   subroutine MAPL_SetFailOnDeprecated(flag)
      logical, optional, intent(in) :: flag

      logical :: flag_
      flag_ = .true.
      if (present(flag)) flag_ = flag

      FAIL_ON_DEPRECATED = flag_
   end subroutine MAPL_SetFailOnDeprecated

   subroutine MAPL_set_abort_handler(handler)
      procedure(abort_handler_interface) :: handler
      abort_handler => handler
   end subroutine MAPL_set_abort_handler

   subroutine MAPL_abort()
      if (associated(abort_handler)) then
         call abort_handler()
      else
         error stop 'MAPL_abort: fatal error'
      end if
   end subroutine MAPL_abort

   function get_error_message(error_code) result(description)

      use gFTL_IntegerStringMap
      character(:), allocatable :: description
      integer, intent(in) :: error_code

      type(IntegerStringMap), save :: error_messages
      logical, save :: initialized = .false.

      call initialize_err()

      if (error_messages%count(error_code) > 0) then
         description = error_messages%at(error_code)
      else
         description = error_messages%at(MAPL_UNKNOWN_ERROR)
      end if

   contains

      subroutine initialize_err()
         if (.not. initialized) then
            initialized = .true.
            call error_messages%insert(MAPL_UNKNOWN_ERROR, 'unknown error')
            call error_messages%insert(MAPL_SUCCESS, 'success')

            call error_messages%insert(MAPL_NO_SUCH_PROPERTY, 'no such property')
            call error_messages%insert(MAPL_NO_SUCH_VARIABLE, 'no such variable')
            call error_messages%insert(MAPL_TYPE_MISMATCH,    'passed argument does not match expected type')
            call error_messages%insert(MAPL_UNSUPPORTED_TYPE, 'provided data type is not supported by this subclass')
            call error_messages%insert(MAPL_VALUE_NOT_SUPPORTED, 'provided value is not supported by this subclass')

            call error_messages%insert(MAPL_NO_DEFAULT_VALUE, 'no default value has been provided for this property')
            call error_messages%insert(MAPL_DUPLICATE_KEY, 'map container already has the specified key')
            call error_messages%insert(MAPL_STRING_TOO_SHORT, 'fixed length string is not long enough to contain requested data')
         end if
      end subroutine initialize_err

   end function get_error_message

end module mapl_ErrorHandling_mod
