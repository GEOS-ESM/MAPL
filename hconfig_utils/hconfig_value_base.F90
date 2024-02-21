module hconfig_value_base

   use esmf

   implicit none

   type, abstract :: HConfigValue
      type(ESMF_HConfig), allocatable :: hconfig_
      character(len=:), allocatable :: keystring_
      integer, allocatable :: last_status_
      character(len=:), allocatable :: typestring_
      logical, allocatable :: value_equals_default_
   contains
      procedure(ValueSetter), deferred :: set_from_default
      procedure(ValueSetter), deferred :: set_from_hconfig
      procedure(StateChecker), deferred :: value_equals_default
      procedure(StringGetter), deferred :: get_valuestring
   end type HConfigValue

   abstract interface

      subroutine ValueSetter(this)
         import HConfigValue
         class(HConfigValue), intent(inout) :: this
      end subroutine ValueSetter

      logical function StateChecker(this) result(lval)
         import HConfigValue
         class(HConfigValue), intent(in) :: this
      end function StateChecker

      subroutine StringGetter(this, string)
         import HConfigValue
         class(HConfigValue), intent(inout) :: this
         character(len=:), allocatable, intent(out) :: string
      end subroutine StringGetter

   end interface

end module hconfig_value_base
