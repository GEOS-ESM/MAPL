module hconfig_value_base

   use esmf, only: ESMF_HConfig

   implicit none

   abstract interface

      subroutine ValueSetter(this)
         class(HConfigValue), intent(inout) :: this
      end subroutine ValueSetter

      logical function StateChecker(this) result(lval)
         class(HConfigValue), intent(in) :: this
      end function StateChecker

      subroutine StringGetter(this, string)
         class(HConfigValue), intent(inout) :: this
         character(len=:), allocatable, intent(out) :: string
      end subroutine StringGetter

   end abstract interface

   type, abstract :: HConfigValue
      type(ESMF_HConfig) :: hconfig_
      character(len=:), allocatable :: keystring_
      integer :: last_status_ = 0
      character(len=:), allocatable :: typestring_
   contains
      procedure(ValueSetter), deferred :: set_from_default
      procedure(ValueSetter), deferred :: set_from_hconfig
      procedure(StateChecker), deferred :: value_equals_default
      procedure(StringGetter), deferred :: get_valuestring
   end type HConfigValue

end module hconfig_value_base
