module hconfig_value_base

   implicit none

   abstract interface

      function StringGetter(this) result(string)
         character(len=:), allocatable :: string
         class(HConfigValue), intent(inout) :: this
      end function StringGetter

      integer function IntGetter(this)
         class(HConfigValue), intent(inout) :: this
      end function IntGetter

      subroutine StringSetter(this, string, rc)
         class(HConfigValue), intent(in) :: this
         character(len=*), intent(out) :: string
         integer, intent(out) :: rc
      end subroutine StringSetter

      subroutine StateSetterRC(this, rc)
         class(HConfigValue), intent(inout) :: this
         integer, intent(out) :: rc
      end subroutine StateSetterRC

      subroutine StateSetter(this)
         class(HConfigValue), intent(inout) :: this
      end subroutine StateSetter

      logical function LogicalGetter(this)
         class(HConfigValue), intent(in) :: this
      end function LogicalGetter

      subroutine StateEvaluator(this, hconfig, keystring, rc)
         class(HConfigValue), intent(inout) :: this
         type(ESMF_HConfig) :: hconfig
         character(len=*), intent(in) :: keystring
         integer, intent(out) :: rc
      end subroutine StateEvaluator

   end abstract interface

   type, abstract :: HConfigValue 
   contains
      private
      procedure(StringSetter), deferred :: set_valuestring
      procedure(StateSetterRC), deferred :: set_from_hconfig
      procedure(StateSetter), deferred :: set_from_default
      procedure(LogicalGetter), deferred :: check_value_equals_default
      procedure(LogicalGetter), deferred :: has_default 
      procedure(IntGetter), deferred :: last_status
      procedure(StateSetterRC), public, deferred :: set_value
      procedure(LogicalGetter), public, deferred :: value_equals_default
      procedure(LogicalGetter), public, deferred :: value_is_set
      procedure(StringGetter), public, deferred :: typestring
      procedure(StringGetter), public, deferred :: valuestring
      procedure(LogicalGetter), public, deferred :: found
   end type HConfigValue

end module hconfig_value_base
