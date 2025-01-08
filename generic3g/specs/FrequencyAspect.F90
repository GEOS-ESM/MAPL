#include "MAPL_Generic.h"
#include "unused_dummy.h"
module mapl3g_FrequencyAspect
   use mapl3g_StateItemAspect
   use mapl3g_AccumulatorActionInterface
   use mapl_KeywordEnforcer
   use esmf
   implicit none
   private

   public :: FrequencyAspect

   type, extends(StateItemAspect) :: FrequencyAspect
      private
      type(ESMF_TimeInterval) :: timestep_
      character(len=:), allocatable :: accumulation_type_
   contains
      procedure :: matches
      procedure :: supports_conversion_general
      procedure :: supports_conversion_specific
      procedure :: make_action
      procedure :: timestep => get_timestep
      procedure :: set_timestep
      procedure :: accumulation_type => get_accumulation_type
      procedure :: set_accumulation_type
      procedure, private :: set_timestep_scalar
   end type FrequencyAspect

   interface FrequencyAspect
      module procedure :: construct_frequency_aspect
   end interface FrequencyAspect

   interface operator(.divides.)
      module procedure :: aspect_divides
   end interface operator(.divides.)

   ! This value should not be accessed directly. Use zero() instead.
   ! There is no constructor for ESMF_TimeInterval, so the value cannot be initialized
   ! at construction. The zero() function initializes the value the first time
   ! and returns a pointer to the value.
   type(ESMF_TimeInterval), target :: ZERO_TI

contains

   function construct_frequency_aspect(timestep, accumulation_type) result(aspect)
      type(FrequencyAspect) :: aspect
      type(ESMF_TimeInterval), optional, intent(in) :: timestep
      character(len=*), optional, intent(in) :: accumulation_type

      aspect%mirror = .FALSE.
      aspect%time_dependent = .FALSE.
      aspect%accumulation_type_ = INSTANTANEOUS
      aspect%set_timestep_scalar(ns=0)

      if(present(accumulation_type)) then
         call aspect%set_accumulation_type(accumulation_type)
      end if

      if(present(timestep)) then
         call aspect%set_timestep(timestep)
      end if
      
   end function construct_frequency_aspect

   function get_timestep(this) result(ts)
      type(ESMF_TimeInterval) :: ts
      class(FrequencyAspect), intent(in) :: this

      ts = this%timestep_

   end function get_timestep

   subroutine set_timestep(this, timestep)
      class(FrequencyAspect), intent(inout) :: this
      type(ESMF_TimeInterval), intent(in) :: timestep

      this%timestep_ = timestep

   end subroutine set_timestep

   subroutine set_timestep_scalar(this, unusable, s, ns)
      class(FrequencyAspect), intent(inout) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: s
      integer, optional, intent(in) :: ns

      if(present(s)) then
         call ESMF_TimeIntervalSet(this%timestep_, s=s)
         return
      end if
      if(present(ns)) then
         call ESMF_TimeIntervalSet(this%timestep_, ns=ns)
         return
      end if
      call ESMF_TimeIntervalSet(this%timestep_, ns=0)
      _UNUSED(unusable)

   end subroutine set_timestep_scalar

   function get_accumulation_type(this) result(at)
      character(len=:), allocatable :: at
      class(FrequencyAspect), intent(in) :: this

      at = ''
      if(allocated(this%accumulation_type_)) at = this%accumulation_type_

   end function get_accumulation_type

   subroutine set_accumulation_type(this, accumulation_type)
      class(FrequencyAspect), intent(inout) :: this
      character(len=*), intent(in) :: accumulation_type

      if(accumulation_type == INSTANTANEOUS .or. accumulation_type_is_valid(accumulation_type)) then
         thisaccumulation_type_ = accumulation_type
      end if

   end subroutine set_accumulation_type

   logical function matches(src, dst) result(does_match)
      class(FrequencyAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst

      does_match = .TRUE.
      if(src%timestep() == zero()) return
      select type(StateItemAspect)
      class is (FrequencyAspect)
         if(dst%timestep() == zero()) return
         if(.not. accumulation_type_is_valid(dstaccumulation_type_)) return
         does_match = src%timestep() == dst%timestep()
      end select

   end function matches

   function make_action(src, dst, rc) result(action)
      use mapl3g_ExtensionAction
      class(ExtensionAction), allocatable :: action
      class(FrequencyAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst
      integer, optional, intent(out) :: rc
      integer :: status

      select type(dst)
      class is (FrequencyAspect)
         call get_accumulator_action(dstaccumulation_type_, ESMF_TYPEKIND_R4, action, _RC) 
         _ASSERT(allocated(action), 'Unable to allocate action')
      class default
         _FAIL('FrequencyAspect cannot convert from other class.')
      end select
      _RETURN(_SUCCESS)

   end function make_action

   logical function supports_conversion_general(this) result(supports)
      class(FrequencyAspect), intent(in) :: this

      supports = .TRUE.

   end function supports_conversion_general

   logical function supports_conversion_specific(src, dst) result(supports)
      class(FrequencyAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst

      select type(dst)
      class is (FrequencyAspect)
         supports = src .divides. dst
      end select

   end function supports_conversion_specific

   logical function aspect_divides(factor, base)
      class(FrequencyAspect), intent(in) :: factor
      class(FrequencyAspect), intent(in) :: base

      aspect_divides = interval_divides(factor%timestep(), base%timestep())

   end function aspect_divides

   logical function interval_divides(factor, base) result(lval)
      type(ESMF_TimeInterval), intent(in) :: factor
      type(ESMF_TimeInterval), intent(in) :: base

      lval = .FALSE.
      if(factor == zero()) return
      lval = mod(base, factor) == zero()

   end function interval_divides

   function zero()
      type(ESMF_TimeInterval), pointer :: zero
      logical, save :: zero_is_uninitialized = .TRUE.

      if(zero_is_uninitialized) then
         call ESMF_TimeIntervalSet(ZERO_TI, ns=0)
         zero_is_uninitialized = .FALSE.
      end if
      zero => ZERO_TI

   end function zero

end module mapl3g_FrequencyAspect
