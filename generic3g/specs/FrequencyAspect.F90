#include "MAPL_Generic.h"
module mapl3g_FrequencyAspect
   use mapl3g_StateItemAspect
   use mapl3g_AccumulatorActionInterface
   use esmf
   implicit none
   private

   public :: FrequencyAspect

   type, extends(StateItemAspect) :: FrequencyAspect
      type(ESMF_TimeInterval) :: dt
      character(len=:), allocatable :: accumulation_type
   contains
      procedure :: matches
      procedure :: supports_conversion_general
      procedure :: supports_conversion_specific
      procedure :: make_action
      procedure :: set_dt
      procedure :: set_accumulation_type
   end type FrequencyAspect

   interface FrequencyAspect
      module procedure :: construct_frequency_aspect
   end interface FrequencyAspect

   interface operator(.divides.)
      module procedure :: divides
   end interface operator(.divides.)

contains

   function construct_frequency_aspect(dt, accumulation_type) result(aspect)
      type(FrequencyAspect) :: aspect
      type(ESMF_TimeInterval), optional, intent(in) :: dt
      character(len=*), optional, intent(in) :: accumulation_type

      aspect%accumulation_type = NO_ACCUMULATION
      call ESMF_TimeIntervalSet(aspect%dt, ns=0)
      if(present(accumulation_type)) then
         call aspect%set_accumulation_type(accumulation_type)
      end if
      if(present(dt)) then
         call aspect%set_dt(dt)
      end if
      aspect%mirror = .FALSE.
      aspect%time_dependent = .FALSE.
      
   end function construct_frequency_aspect

   subroutine set_dt(this, dt)
      class(FrequencyAspect), intent(inout) :: this
      type(ESMF_TimeInterval), intent(in) :: dt

      this%run_dt = dt

   end subroutine set_dt

   subroutine set_accumulation_type(this, accumulation_type)
      class(FrequencyAspect), intent(inout) :: this
      character(len=*), intent(in) :: accumulation_type

      if(accumulation_type_is_valid(accumulation_type)) this%accumulation_type = accumulation_type

   end subroutine set_accumulation_type

   logical function matches(this, aspect) result(does_match)
      class(FrequencyAspect), intent(in) :: this
      class(StateItemAspect), intent(in) :: aspect

      does_match = .FALSE.
      if(is_zero(this%dt)) return
      select type(StateItemAspect)
      class is (FrequencyAspect)
         if(is_zero(aspect%dt)) return
         does_match = this%dt == aspect%dt
      end select

   end function matches

   function make_action(this, aspect, rc) result(action)
      use mapl3g_ExtensionAction
      class(ExtensionAction), allocatable :: action
      class(FrequencyAspect), intent(in) :: this
      class(StateItemAspect), intent(in) :: aspect
      integer, optional, intent(out) :: rc
      integer :: status

      allocate(action, source=NullAction())
      select type(aspect)
      class is (FrequencyAspect)
         call get_accumulator_action(aspect%accumulation_type, ESMF_TYPEKIND_R4, action, _RC) 
         _ASSERT(allocated(action), 'Unable to allocate action')
      class default
         _FAIL('FrequencyAspect cannot convert from other class.')
      end select
      _RETURN(_SUCCESS)

   end function make_action

   logical function supports_conversion_general(this) result(supports)
      class(FrequencyAspect), intent(in) :: this

      supports = .not. is_zero(this%dt)

   end function supports_conversion_general

   logical function supports_conversion_specific(this, aspect) result(supports)
      class(FrequencyAspect), intent(in) :: this
      class(StateItemAspect), intent(in) :: aspect

      supports = .FALSE.
      if(is_zero(this%dt)) return
      select type(aspect)
      class is (FrequencyAspect)
         if(is_zero(aspect%dt)) return
         if(.not. accumulation_type_is_valid(aspect%accumulation_type)) return 
         supports = aspect .divides. this
      end select

   end function supports_conversion_specific

   elemental function are_nonzero(ti) 
      logical :: are_nonzero
      type(ESMF_TimeInterval), intent(in) :: ti
      type(ESMF_TimeInterval), save :: zero
      logical :: uninitialized :: .TRUE.

      if(uninitialized) then
         call ESMF_TimeIntervalSet(zero, ns=0)
         uninitialized = .FALSE.
      end if
      are_nonzero = ti > zero

   end function are_nonzero

   logical function is_zero(ti)
      type(ESMF_TimeInterval), intent(in) :: ti

      is_zero = .not. are_nonzero(ti)

   end function is_zero

   logical function divides(factor, base) result(lval)
      class(FrequencyAspect), intent(in) :: factor
      class(FrequencyAspect), intent(in) :: base

      lval = .FALSE.
      if(all(are_nonzero([base%dt, factor%dt]))) then
         lval = is_zero(mod(base%dt, factor%dt))
      end if

   end function divides

end module mapl3g_FrequencyAspect
