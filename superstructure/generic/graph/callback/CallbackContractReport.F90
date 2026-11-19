#include "MAPL.h"

!------------------------------------------------------------------------------
! CallbackContractReport: formats one callback State's collected
! CallbackContractViolation list (CallbackContractViolation.F90) into one
! report message - a header line naming the interface and its version,
! the Handler component and the callback State, followed by one
! indented line per violation, so every violation found for one
! callback State is reported together instead of failing on the first.
!
! A free function rather than a method on CallbackContractViolation (or a
! bespoke violation-list type) - the violations for one report come from
! an ordinary allocatable array (the same convention
! find_mismatched_characteristics already uses for a list of problems
! found), and the header's four facts (interface name and version,
! Handler, callback State) belong to the caller's checking context, not
! to any one violation.
!------------------------------------------------------------------------------
module mapl_CallbackContractReport_mod
   use mapl_CallbackContractViolation_mod, only: CallbackContractViolation
   use mapl_StringUtilities_mod, only: to_string
   implicit none(type, external)
   private

   public :: format_callback_contract_report

contains

   ! Header reads "callback contract violation: interface '<name>'
   ! (version <v>), Handler '<component>', callback State '<state>':"
   ! followed by one line per violation, indented two spaces.
   function format_callback_contract_report(interface_name, interface_version, handler_name, &
        callback_state_name, violations) result(message)
      character(*), intent(in) :: interface_name
      integer, intent(in) :: interface_version
      character(*), intent(in) :: handler_name
      character(*), intent(in) :: callback_state_name
      type(CallbackContractViolation), intent(in) :: violations(:)
      character(:), allocatable :: message

      integer :: i

      message = "callback contract violation: interface '" // interface_name // &
           "' (version " // to_string(interface_version) // "), Handler '" // handler_name // &
           "', callback State '" // callback_state_name // "':"

      do i = 1, size(violations)
         message = message // NEW_LINE('a') // '  ' // violations(i)%get_text()
      end do

   end function format_callback_contract_report

end module mapl_CallbackContractReport_mod
