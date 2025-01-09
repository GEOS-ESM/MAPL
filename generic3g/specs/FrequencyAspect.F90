#include "MAPL_Generic.h"
#include "unused_dummy.H"
module mapl3g_FrequencyAspect
   use mapl3g_StateItemAspect
   use mapl3g_AccumulatorActionInterface
   use esmf
   implicit none
   private

   public :: FrequencyAspect

   type, extends(StateItemAspect) :: FrequencyAspect
      private
      type(ESMF_TimeInterval) :: timestep_
      character(len=:), allocatable :: accumulation_type_
   contains
      ! These are implementations of extended derived type.
      procedure :: matches
      procedure :: supports_conversion_general
      procedure :: supports_conversion_specific
      procedure :: make_action
      ! These are specific to FrequencyAspect.
      procedure :: timestep
      procedure :: set_timestep
      procedure :: accumulation_type
      procedure :: set_accumulation_type
      procedure, private :: zero_timestep
   end type FrequencyAspect

   interface FrequencyAspect
      module procedure :: construct_frequency_aspect_noargs
      module procedure :: construct_frequency_aspect_timestep
      module procedure :: construct_frequency_aspect_accumulation_type
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

   function construct_frequency_aspect_noargs() result(aspect)
      type(FrequencyAspect) :: aspect

      call aspect%set_mirror(.FALSE.)
      call aspect%set_time_dependent(.FALSE.)
      call aspect%set_accumulation_type(INSTANTANEOUS)
      call aspect%zero_timestep()

   end function construct_frequency_aspect_noargs

   function construct_frequency_aspect_timestep(timestep) result(aspect)
      type(FrequencyAspect) :: aspect
      type(ESMF_TimeInterval), intent(in) :: timestep

      aspect = FrequencyAspect()
      call aspect%set_timestep(timestep)
      
   end function construct_frequency_aspect_timestep

   function construct_frequency_aspect_accumulation_type(accumulation_type) result(aspect)
      type(FrequencyAspect) :: aspect
      character(len=*), intent(in) :: accumulation_type
      
      aspect = FrequencyAspect()
      call aspect%set_accumulation_type(accumulation_type)

   end function construct_frequency_aspect_accumulation_type

   function construct_frequency_aspect(timestep, accumulation_type) result(aspect)
      type(FrequencyAspect) :: aspect
      type(ESMF_TimeInterval), intent(in) :: timestep
      character(len=*), intent(in) :: accumulation_type

      aspect = FrequencyAspect()
      call aspect%set_accumulation_type(accumulation_type)
      call aspect%set_timestep(timestep)
      
   end function construct_frequency_aspect

   function timestep(this) result(ts)
      type(ESMF_TimeInterval) :: ts
      class(FrequencyAspect), intent(in) :: this

      ts = this%timestep_

   end function timestep

   subroutine set_timestep(this, timestep)
      class(FrequencyAspect), intent(inout) :: this
      type(ESMF_TimeInterval), intent(in) :: timestep

      this%timestep_ = timestep

   end subroutine set_timestep

   subroutine zero_timestep(this)
      class(FrequencyAspect), intent(inout) :: this

      call ESMF_TimeIntervalSet(this%timestep_, ns=0)

   end subroutine zero_timestep

   function accumulation_type(this) result(at)
      character(len=:), allocatable :: at
      class(FrequencyAspect), intent(in) :: this

      at = ''
      if(allocated(this%accumulation_type_)) at = this%accumulation_type_

   end function accumulation_type

   subroutine set_accumulation_type(this, accumulation_type)
      class(FrequencyAspect), intent(inout) :: this
      character(len=*), intent(in) :: accumulation_type

      if(accumulation_type == INSTANTANEOUS .or. accumulation_type_is_valid(accumulation_type)) then
         this%accumulation_type_ = accumulation_type
      end if

   end subroutine set_accumulation_type

   logical function matches(src, dst) result(does_match)
      class(FrequencyAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst
      type(ESMF_TimeInterval) :: timestep

      does_match = .TRUE.
      if(src%timestep() == zero()) return
      select type(dst)
      class is (FrequencyAspect)
         timestep = dst%timestep()
         if(timestep == zero()) return
         if(.not. accumulation_type_is_valid(dst%accumulation_type())) return
         does_match = timestep == src%timestep()
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
         call get_accumulator_action(dst%accumulation_type(), ESMF_TYPEKIND_R4, action, _RC) 
         _ASSERT(allocated(action), 'Unable to allocate action')
      class default
         _FAIL('FrequencyAspect cannot convert from other class.')
      end select
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(src)

   end function make_action

   logical function supports_conversion_general(src) result(supports)
      class(FrequencyAspect), intent(in) :: src

      supports = .TRUE.
      _UNUSED_DUMMY(src)

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
