#include "MAPL_Generic.h"
#include "unused_dummy.H"
module mapl3g_FrequencyAspect
   use mapl3g_AspectId
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
      procedure :: make_action2
      procedure :: connect_to_export
      procedure, nopass :: get_aspect_id
      ! These are specific to FrequencyAspect.
      procedure :: get_timestep
      procedure :: set_timestep
      procedure :: get_accumulation_type
      procedure :: set_accumulation_type
      procedure, private :: zero_timestep
   end type FrequencyAspect

   interface FrequencyAspect
      module procedure :: construct_frequency_aspect
   end interface FrequencyAspect

   interface operator(.divides.)
      module procedure :: aspect_divides
   end interface operator(.divides.)

   ! This value should not be accessed directly. Use get_zero() instead.
   ! There is no constructor for ESMF_TimeInterval, so the value cannot be initialized
   ! at construction. The get_zero() function initializes the value the first time
   ! and returns a pointer to the value.
   type(ESMF_TimeInterval), target :: ZERO_TI

contains

   function construct_frequency_aspect(timestep, accumulation_type) result(aspect)
      type(FrequencyAspect) :: aspect
      type(ESMF_TimeInterval), optional, intent(in) :: timestep
      character(len=*), optional, intent(in) :: accumulation_type

      call aspect%set_mirror(.FALSE.)
      call aspect%set_time_dependent(.FALSE.)
      call aspect%set_accumulation_type(INSTANTANEOUS)
      call aspect%zero_timestep()
      if(present(timestep)) aspect%timestep_ = timestep
      if(present(accumulation_type)) aspect%accumulation_type_ = accumulation_type
      
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

   subroutine zero_timestep(this)
      class(FrequencyAspect), intent(inout) :: this

      call ESMF_TimeIntervalSet(this%timestep_, ns=0)

   end subroutine zero_timestep

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
         this%accumulation_type_ = accumulation_type
      end if

   end subroutine set_accumulation_type

   logical function matches(src, dst) result(does_match)
      class(FrequencyAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst
      type(ESMF_TimeInterval) :: src_timestep, dst_timestep
      type(ESMF_TimeInterval), pointer :: zero

      does_match = .TRUE.
      zero => get_zero()
      src_timestep = src%get_timestep()
      if(src_timestep == zero) return
      select type(dst)
      class is (FrequencyAspect)
         dst_timestep = dst%get_timestep()
         if(dst_timestep == zero) return
         if(.not. accumulation_type_is_valid(dst%get_accumulation_type())) return
         does_match = dst_timestep == src_timestep
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
         call get_accumulator_action(dst%get_accumulation_type(), ESMF_TYPEKIND_R4, action, _RC) 
         _ASSERT(allocated(action), 'Unable to allocate action')
      class default
         _FAIL('FrequencyAspect cannot convert from other class.')
      end select
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(src)

   end function make_action

    function make_action2(src, dst, other_aspects, rc) result(action)
      class(ExtensionAction), allocatable :: action
      class(FrequencyAspect), intent(in) :: src
      class(StateItemAspect), intent(in)  :: dst
      type(AspectMap), target, intent(in)  :: other_aspects
      integer, optional, intent(out) :: rc

      integer :: status

      select type(dst)
      class is (FrequencyAspect)
         call get_accumulator_action(dst%get_accumulation_type(), ESMF_TYPEKIND_R4, action, _RC) 
         _ASSERT(allocated(action), 'Unable to allocate action')
      class default
         allocate(action,source=NullAction())
         _FAIL('FrequencyAspect cannot convert from other class.')
      end select

      _RETURN(_SUCCESS)
   end function make_action2

   subroutine connect_to_export(this, export, rc)
      class(FrequencyAspect), intent(inout) :: this
      class(StateItemAspect), intent(in) :: export
      integer, optional, intent(out) :: rc

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(export)
   end subroutine connect_to_export

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

      aspect_divides = interval_divides(factor%get_timestep(), base%get_timestep())

   end function aspect_divides

   logical function interval_divides(factor, base) result(lval)
      type(ESMF_TimeInterval), intent(in) :: factor
      type(ESMF_TimeInterval), intent(in) :: base
      type(ESMF_TimeInterval), pointer :: zero

      lval = .FALSE.
      zero => get_zero()
      if(factor == zero) return
      lval = mod(base, factor) == zero

   end function interval_divides

   function get_zero() result(zero)
      type(ESMF_TimeInterval), pointer :: zero
      logical, save :: zero_is_uninitialized = .TRUE.

      if(zero_is_uninitialized) then
         call ESMF_TimeIntervalSet(ZERO_TI, ns=0)
         zero_is_uninitialized = .FALSE.
      end if
      zero => ZERO_TI

   end function get_zero

   function get_aspect_id() result(aspect_id)
      type(AspectId) :: aspect_id
      aspect_id = FREQUENCY_ASPECT_ID
   end function get_aspect_id


end module mapl3g_FrequencyAspect
