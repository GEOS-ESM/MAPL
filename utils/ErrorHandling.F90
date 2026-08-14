#include "MAPL.h"

module mapl_ErrorHandling_mod
   use, intrinsic :: iso_fortran_env, only: ERROR_UNIT
   use MAPL_Constants
   use mapl_Throw_mod
   implicit none
   private

   public :: MAPL_Assert
   public :: MAPL_AssertCode
   public :: MAPL_AssertCodeContext
   public :: MAPL_Verify
   public :: MAPL_Return
   public :: MAPL_Deprecated
   public :: MAPL_SetFailOnDeprecated
   public :: MAPL_abort
   public :: MAPL_set_abort_handler

   abstract interface
      subroutine abort_handler_interface()
      end subroutine abort_handler_interface
   end interface

   procedure(abort_handler_interface), pointer :: abort_handler => null()
   logical, save :: FAIL_ON_DEPRECATED = .false.

   interface MAPL_Assert
      module procedure MAPL_Assert_condition
      module procedure MAPL_Assert_return_code
   end interface MAPL_Assert

contains

   logical function MAPL_Assert_condition(condition, message, return_code, filename, line, rc) result(fail)
      logical, intent(in) :: condition
      character(*), intent(in) :: message
      integer, intent(in) :: return_code
      character(*), intent(in) :: filename
      integer, intent(in) :: line
      integer, optional, intent(out) :: rc

      fail = .not. condition
      if (fail) then
         !$omp critical (MAPL_ErrorHandling1)
         call MAPL_throw_exception(filename, line, message=message)
         !$omp end critical (MAPL_ErrorHandling1)
         if (present(rc)) rc = return_code
      end if
   end function MAPL_Assert_condition

   logical function MAPL_Assert_return_code(condition, return_code, filename, line, rc) result(fail)
      logical, intent(in) :: condition
      integer, intent(in) :: return_code
      character(*), intent(in) :: filename
      integer, intent(in) :: line
      integer, optional, intent(out) :: rc

      fail = .not. condition
      if (fail) then
         !$omp critical (MAPL_ErrorHandling2)
         call MAPL_throw_exception(filename, line, message=render_message(return_code, ''))
         !$omp end critical (MAPL_ErrorHandling2)
         if (present(rc)) rc = return_code
      end if
   end function MAPL_Assert_return_code

   logical function MAPL_AssertCode(condition, error_code, filename, line, rc) result(fail)
      logical, intent(in) :: condition
      integer, intent(in) :: error_code
      character(*), intent(in) :: filename
      integer, intent(in) :: line
      integer, optional, intent(out) :: rc

      fail = MAPL_AssertCodeContext(condition, error_code, '', filename, line, rc)
   end function MAPL_AssertCode

   logical function MAPL_AssertCodeContext(condition, error_code, context, filename, line, rc) result(fail)
      logical, intent(in) :: condition
      integer, intent(in) :: error_code
      character(*), intent(in) :: context, filename
      integer, intent(in) :: line
      integer, optional, intent(out) :: rc

      fail = .not. condition
      if (fail) then
         !$omp critical (MAPL_ErrorHandlingCode)
         call MAPL_throw_exception(filename, line, &
              message=render_message(error_code, context))
         !$omp end critical (MAPL_ErrorHandlingCode)
         if (present(rc)) rc = error_code
      end if
   end function MAPL_AssertCodeContext

   logical function MAPL_Verify(status, filename, line, rc) result(fail)
      integer, intent(in) :: status
      character(*), intent(in) :: filename
      integer, intent(in) :: line
      integer, optional, intent(out) :: rc
      character(32) :: status_string
      character(:), allocatable :: message

      fail = status /= MAPL_SUCCESS
      if (fail) then
         write(status_string, '(i0)') status
         message = 'status=' // trim(status_string)
         write(ERROR_UNIT, '(a)') render_message(MAPL_ERROR_VERIFY, message)
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
      integer, optional, intent(out) :: rc

      if (status /= MAPL_SUCCESS) then
         !$omp critical (MAPL_ErrorHandling4)
         call MAPL_throw_exception(filename, line, message=render_message(status, ''))
         !$omp end critical (MAPL_ErrorHandling4)
      end if
      if (present(rc)) rc = status
   end subroutine MAPL_Return

   subroutine MAPL_Deprecated(file_name, module_name, procedure_name, rc)
      character(*), intent(in) :: file_name, module_name, procedure_name
      integer, optional, intent(out) :: rc
      integer :: status

      write(ERROR_UNIT, *, iostat=status) 'Invoking deprecated procedure: ', procedure_name
      _VERIFY(status)
      write(ERROR_UNIT, *, iostat=status) '    ...             in module: ', module_name
      _VERIFY(status)
      write(ERROR_UNIT, *, iostat=status) '    ...               in file: ', file_name
      _VERIFY(status)
      _ASSERT(.not. FAIL_ON_DEPRECATED, '    ... aborting.')
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

   function render_message(error_code, context) result(message)
      integer, intent(in) :: error_code
      character(*), intent(in) :: context
      character(:), allocatable :: message
      integer :: index
      character(32) :: code_string
      character(:), allocatable :: template, name

      write(code_string, '(i0)') error_code
      index = find_error_index(error_code)
      if (index == 0) then
         message = 'MAPL_UNKNOWN_ERROR(code=' // trim(code_string) // ')'
         if (len_trim(context) > 0) message = message // ': ' // trim(context)
         return
      end if

      name = trim(MAPL_ERROR_NAMES(index))
      template = trim(MAPL_ERROR_TEMPLATES(index))
      if (len_trim(context) > 0) then
         message = replace_first_field(template, trim(context))
      else
         message = template
      end if
      message = name // '(code=' // trim(code_string) // '): ' // trim(message)
   end function render_message

   integer function find_error_index(error_code) result(index)
      integer, intent(in) :: error_code
      integer :: i

      index = 0
      do i = 1, MAPL_ERROR_CODE_COUNT
         if (MAPL_ERROR_CODES(i) == error_code) then
            index = i
            return
         end if
      end do
   end function find_error_index

   function replace_first_field(template, context) result(message)
      character(*), intent(in) :: template, context
      character(:), allocatable :: message
      integer :: left, right

      left = index(template, '{')
      if (left == 0) then
         message = trim(template) // ': ' // trim(context)
         return
      end if
      right = index(template(left:), '}')
      if (right == 0) then
         message = trim(template) // ': ' // trim(context)
         return
      end if
      right = left + right - 1
      if (left == 1) then
         message = trim(context) // template(right+1:)
      else if (right == len_trim(template)) then
         message = template(:left-1) // trim(context)
      else
         message = template(:left-1) // trim(context) // template(right+1:)
      end if
   end function replace_first_field

end module mapl_ErrorHandling_mod
