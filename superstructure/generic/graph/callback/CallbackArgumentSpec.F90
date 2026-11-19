#include "MAPL.h"

!------------------------------------------------------------------------------
! CallbackArgumentSpec: a CallbackInterface's named argument declaration
! (spec/15-callbacks.md) - a name plus a required expected
! MAPL_StateItem_Flag kind (e.g. the PassiveTracer example's "tracers,
! ESMF FieldBundle", §15.3). Carries no AccessSpec of its own - access is
! declared per-method by CallbackMethodSpec (one shared
! argument may have different access in different methods); putting
! AccessSpec here as well, mirroring ArgumentSpec exactly, would invite a
! second, unused source of truth.
!
! Unlike ArgumentSpec's optional expected_kind, expected_kind is
! required here: every callback example states one, and there is no
! unconstrained-callback-argument use case in the spec to support.
!
! The remaining attributes are optional and default to the meaning of a
! name-and-kind-only declaration: no gridded dims, required, not
! extensive, not the weight of another argument. A field argument must
! declare its gridded dims ('xyz' or 'xy'). weight_for is stored as a
! name only; checking that it names a real argument needs the whole
! interface. Read as "this argument is the weight for" the named one
! (not "this argument's weight is"), matching how every other attribute
! here describes the argument itself.
!------------------------------------------------------------------------------
module mapl_CallbackArgumentSpec_mod
   use mapl_StateItemFlag_mod, only: MAPL_StateItem_Flag, MAPL_STATEITEM_FIELD, operator(==)
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: CallbackArgumentSpec

   type :: CallbackArgumentSpec
      private
      character(:), allocatable :: name
      type(MAPL_StateItem_Flag) :: expected_kind
      character(:), allocatable :: dims
      logical :: optional = .false.
      logical :: extensive = .false.
      character(:), allocatable :: weight_for
   contains
      procedure :: get_name => argspec_get_name
      procedure :: get_expected_kind => argspec_get_expected_kind
      procedure :: has_dims => argspec_has_dims
      procedure :: get_dims => argspec_get_dims
      procedure :: is_optional => argspec_is_optional
      procedure :: is_extensive => argspec_is_extensive
      procedure :: has_weight_for => argspec_has_weight_for
      procedure :: get_weight_for => argspec_get_weight_for
   end type CallbackArgumentSpec

   interface CallbackArgumentSpec
      module procedure new_CallbackArgumentSpec
   end interface CallbackArgumentSpec

contains

   function new_CallbackArgumentSpec(name, expected_kind, dims, optional, extensive, weight_for, rc) result(spec)
      character(*), intent(in) :: name
      type(MAPL_StateItem_Flag), intent(in) :: expected_kind
      character(*), optional, intent(in) :: dims
      logical, optional, intent(in) :: optional
      logical, optional, intent(in) :: extensive
      character(*), optional, intent(in) :: weight_for
      integer, optional, intent(out) :: rc
      type(CallbackArgumentSpec) :: spec

      spec%name = name
      spec%expected_kind = expected_kind

      if (present(dims)) then
         _ASSERT(dims == 'xyz' .or. dims == 'xy', 'CallbackArgumentSpec: dims must be xyz or xy for argument ' // name)
         spec%dims = dims
      end if
      if (expected_kind == MAPL_STATEITEM_FIELD) then
         _ASSERT(present(dims), 'CallbackArgumentSpec: field argument must declare dims: ' // name)
      end if

      if (present(optional)) spec%optional = optional
      if (present(extensive)) spec%extensive = extensive
      if (present(weight_for)) spec%weight_for = weight_for

      _RETURN(_SUCCESS)
   end function new_CallbackArgumentSpec

   function argspec_get_name(this) result(name)
      class(CallbackArgumentSpec), intent(in) :: this
      character(:), allocatable :: name

      name = this%name
   end function argspec_get_name

   function argspec_get_expected_kind(this) result(expected_kind)
      class(CallbackArgumentSpec), intent(in) :: this
      type(MAPL_StateItem_Flag) :: expected_kind

      expected_kind = this%expected_kind
   end function argspec_get_expected_kind

   logical function argspec_has_dims(this) result(has_dims)
      class(CallbackArgumentSpec), intent(in) :: this

      has_dims = allocated(this%dims)
   end function argspec_has_dims

   ! Returns '' when no dims were declared.
   function argspec_get_dims(this) result(dims)
      class(CallbackArgumentSpec), intent(in) :: this
      character(:), allocatable :: dims

      dims = ''
      if (allocated(this%dims)) dims = this%dims
   end function argspec_get_dims

   logical function argspec_is_optional(this) result(is_optional)
      class(CallbackArgumentSpec), intent(in) :: this

      is_optional = this%optional
   end function argspec_is_optional

   logical function argspec_is_extensive(this) result(is_extensive)
      class(CallbackArgumentSpec), intent(in) :: this

      is_extensive = this%extensive
   end function argspec_is_extensive

   logical function argspec_has_weight_for(this) result(has_weight_for)
      class(CallbackArgumentSpec), intent(in) :: this

      has_weight_for = allocated(this%weight_for)
   end function argspec_has_weight_for

   ! Returns '' when this argument is not the weight of another.
   function argspec_get_weight_for(this) result(weight_for)
      class(CallbackArgumentSpec), intent(in) :: this
      character(:), allocatable :: weight_for

      weight_for = ''
      if (allocated(this%weight_for)) weight_for = this%weight_for
   end function argspec_get_weight_for

end module mapl_CallbackArgumentSpec_mod
