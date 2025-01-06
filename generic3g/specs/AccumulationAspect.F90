#include "MAPL_Generic.h"
module mapl3g_AccumulationAspect
   use mapl3g_StateItemAspect
   use mapl3g_AccumulatorActionInterface
   use esmf
   implicit none
   private

   public :: AccumulationAspect

   type, extends(StateItemAspect) :: AccumulationAspect
      character(len=:), allocatable :: accumulation_type
      type(ESMF_TimeInterval) :: run_dt
      logical :: is_valid = .FALSE.
   contains
      procedure :: matches
      procedure :: supports_conversion_general
      procedure :: supports_conversion_specific
      procedure :: make_action
   end type AccumulationAspect

   interface AccumulationAspect
      module procedure :: construct_accumulation_aspect
   end interface AccumulationAspect

   type(ESMF_TimeInterval) :: ti_zero
   logical :: ti_zero_is_unset = .TRUE.

contains

   function construct_accumulation_aspect(ccumulation_type, run_dt) result(aspect)
      type(AccumulationAspect) :: aspect
      character(len=*), optional, intent(in) :: accumulation_type
      type(ESMF_TimeInterval), optional, intent(in) :: run_dt

      aspect%run_dt = get_ti_zero()
      aspect%accumulation_type = 'NO_ACCUMULATION'
      if(.not. (present(run_dt) .and. present(accumulation_type))) return
      if(.not. accumulation_is_valid(accumulation_type)) return
      if(interval_is_zero(run_dt)) return
      aspect%accumulation_type = accumulation_type
      aspect%run_dt = run_dt
      aspect%is_valid = .TRUE.
      
   end function construct_accumulation_aspect

   logical function matches(this, spec) result(matches)
      import :: StateItemAspect
      class(AccumulationAspect), intent(in) :: this
      class(StateItemAspect), intent(in) :: spec
   end function matches

   function make_action(this, spec, rc) result(action)
      use mapl3g_ExtensionAction
      class(ExtensionAction), allocatable :: action
      class(AccumulationAspect), intent(in) :: this
      class(StateItemAspect), intent(in) :: spec
      integer, optional, intent(out) :: rc
      integer :: state

      select type(spec)
      class is (AccumulationAspect)
      class default
         allocate(action, source=NullAction())
         _FAIL('AccumulationAspect cannot convert from other class.')
      end select
      _RETURN(_SUCCESS)

   end function make_action

   logical function supports_conversion_general(this) result(supports_conversion)
      class(StateItemAspect), intent(in) :: this

      supports_conversion = .TRUE.

   end function supports_conversion_general

   logical function supports_conversion_specific(this, spec) result(supports_conversion)
      class(AccumulationAspect), intent(in) :: this
      class(StateItemAspect), intent(in) :: spec
   end function supports_conversion_specific

   function get_ti_zero() result(tz0)
      type(ESMF_TimeInterval) :: tz0

      if(ti_zero_is_unset) call ESMF_TimeIntervalSet(ti_zero, s=0)
      ti_zero_is_unset = .FALSE.
      tz0 = ti_zero

   end function get_ti_zero

   logical function interval_is_zero(ti) result(lval)
      type(ESMF_TimeInterval), intent(in) :: ti

      lval = (ti == get_ti_zero())

   end function interval_is_zero

   logical function is_factorized_by(base, factor) result(lval)
      type(ESMF_TimeInterval), intent(in) :: base
      type(ESMF_TimeInterval), intent(in) :: factor

      lval = .not. (interval_is_zero(base) .or. interval_is_zero(multiple))
      if(lval) lval = interval_is_zero(mod(base, factor))

   end function is_factorized_by

end module mapl3g_AccumulationAspect
