#include "MAPL.h"

!------------------------------------------------------------------------------
! CallbackArgumentOptions: the per-argument options a callback method may
! attach to one argument's access declaration, beside the access mode.
! One option is recognized, put_flavour, with the legal values "value" or
! "delta". Option names and values arrive as strings (for example from a
! registration file), so set_option is the single place that rejects an
! unrecognized name or an illegal value.
!------------------------------------------------------------------------------
module mapl_CallbackArgumentOptions_mod
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: CallbackArgumentOptions
   public :: CallbackPutFlavour
   public :: operator(==)
   public :: operator(/=)

   public :: CALLBACK_PUT_FLAVOUR_VALUE
   public :: CALLBACK_PUT_FLAVOUR_DELTA

   character(*), parameter :: OPTION_PUT_FLAVOUR = 'put_flavour'

   type :: CallbackPutFlavour
      private
      integer :: value = -1
   contains
      procedure :: is_known => flavour_is_known
      procedure :: to_string => flavour_to_string
   end type CallbackPutFlavour

   type(CallbackPutFlavour), parameter :: &
        CALLBACK_PUT_FLAVOUR_VALUE = CallbackPutFlavour(0)
   type(CallbackPutFlavour), parameter :: &
        CALLBACK_PUT_FLAVOUR_DELTA = CallbackPutFlavour(1)

   interface operator(==)
      module procedure flavour_equal
   end interface operator(==)

   interface operator(/=)
      module procedure flavour_not_equal
   end interface operator(/=)

   ! The default value carries no options.
   type :: CallbackArgumentOptions
      private
      type(CallbackPutFlavour) :: put_flavour
   contains
      procedure :: set_option => options_set_option
      procedure :: has_put_flavour => options_has_put_flavour
      procedure :: get_put_flavour => options_get_put_flavour
   end type CallbackArgumentOptions

contains

   pure logical function flavour_equal(left, right) result(equal)
      type(CallbackPutFlavour), intent(in) :: left, right

      equal = left%value == right%value
   end function flavour_equal

   pure logical function flavour_not_equal(left, right) result(not_equal)
      type(CallbackPutFlavour), intent(in) :: left, right

      not_equal = .not. (left == right)
   end function flavour_not_equal

   ! False for a default-initialized (never assigned) value.
   pure logical function flavour_is_known(this) result(is_known)
      class(CallbackPutFlavour), intent(in) :: this

      is_known = this%value == 0 .or. this%value == 1
   end function flavour_is_known

   pure function flavour_to_string(this) result(name)
      class(CallbackPutFlavour), intent(in) :: this
      character(:), allocatable :: name

      select case (this%value)
      case (0); name = 'value'
      case (1); name = 'delta'
      case default; name = 'unknown'
      end select
   end function flavour_to_string

   ! Sets one named option from its text form. An unrecognized option name
   ! or an illegal value is rejected and leaves the options unchanged.
   subroutine options_set_option(this, option_name, option_value, rc)
      class(CallbackArgumentOptions), intent(inout) :: this
      character(*), intent(in) :: option_name
      character(*), intent(in) :: option_value
      integer, optional, intent(out) :: rc

      character(:), allocatable :: msg

      msg = 'CallbackArgumentOptions: unrecognized option name: ' &
           // option_name
      _ASSERT(option_name == OPTION_PUT_FLAVOUR, msg)

      msg = 'CallbackArgumentOptions: put_flavour must be value or ' &
           // 'delta, got: ' // option_value
      select case (option_value)
      case ('value')
         this%put_flavour = CALLBACK_PUT_FLAVOUR_VALUE
      case ('delta')
         this%put_flavour = CALLBACK_PUT_FLAVOUR_DELTA
      case default
         _FAIL(msg)
      end select

      _RETURN(_SUCCESS)
   end subroutine options_set_option

   logical function options_has_put_flavour(this) result(has_flavour)
      class(CallbackArgumentOptions), intent(in) :: this

      has_flavour = this%put_flavour%is_known()
   end function options_has_put_flavour

   ! Not known when no put_flavour was declared; check has_put_flavour first.
   function options_get_put_flavour(this) result(put_flavour)
      class(CallbackArgumentOptions), intent(in) :: this
      type(CallbackPutFlavour) :: put_flavour

      put_flavour = this%put_flavour
   end function options_get_put_flavour

end module mapl_CallbackArgumentOptions_mod
