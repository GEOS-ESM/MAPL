#include "MAPL_Generic.h"
#include "unused_dummy.H"
module mapl3g_FrequencyAspect
   use mapl3g_ActualConnectionPt
   use mapl3g_AspectId
   use mapl3g_StateItemAspect
   use mapl3g_AccumulatorActionInterface
   use mapl3g_ESMF_Time_Utilities, only: intervals_and_offset_are_compatible, zero_time_interval
   use esmf
   implicit none
   private

   public :: FrequencyAspect

   type, extends(StateItemAspect) :: FrequencyAspect
      private
      type(ESMF_TimeInterval) :: timestep
      type(ESMF_TimeInterval) :: refTime_offset
      character(len=:), allocatable :: accumulation_type
   contains
      ! These are implementations of extended derived type.
      procedure :: matches
      procedure :: supports_conversion_general
      procedure :: supports_conversion_specific
      procedure :: make_action
      procedure :: connect_to_export
      procedure, nopass :: get_aspect_id
      ! These are specific to FrequencyAspect.
      procedure :: get_timestep
      procedure :: get_accumulation_type
      procedure :: get_reference_time_offset
   end type FrequencyAspect

   interface FrequencyAspect
      module procedure :: new_FrequencyAspect
   end interface FrequencyAspect

contains

   function new_FrequencyAspect(timeStep, refTime_offset, accumulation_type) result(aspect)
      type(FrequencyAspect) :: aspect
      type(ESMF_TimeInterval), optional, intent(in) :: timeStep
      type(ESMF_TimeInterval), optional, intent(in) :: refTime_offset
      character(len=*), optional, intent(in) :: accumulation_type
      integer :: status

      call aspect%set_mirror(.FALSE.)
      call aspect%set_time_dependent(.FALSE.)
      call set_accumulation_type(aspect, INSTANTANEOUS)
      call zero_timestep(aspect, rc=status)
      call zero_interval(aspect%refTime_offset, rc=status) 
      if(present(timeStep)) aspect%timestep = timeStep
      if(present(refTime_offset)) aspect%refTime_offset = refTime_offset
      if(present(accumulation_type)) call set_accumulation_type(aspect, accumulation_type)
      
   end function new_FrequencyAspect

   function get_timestep(this) result(ts)
      type(ESMF_TimeInterval) :: ts
      class(FrequencyAspect), intent(in) :: this

      ts = this%timestep

   end function get_timestep

   function get_reference_time_offset(this) result(off)
      type(ESMF_TimeInterval) :: off
      class(FrequencyAspect), intent(in) :: this

      off = this%refTime_offset

   end function get_reference_time_offset

   subroutine zero_timestep(aspect, rc)
      class(FrequencyAspect), intent(inout) :: aspect
      integer, intent(out) :: rc

      call zero_interval(aspect%timestep, rc=rc)

   end subroutine zero_timestep

   subroutine zero_interval(interval, rc)
      type(ESMF_TimeInterval), intent(inout) :: interval
      integer, intent(out) :: rc
      integer :: status

      call ESMF_TimeIntervalSet(interval, ns=0, rc=rc)

   end subroutine zero_interval

   function get_accumulation_type(this) result(at)
      character(len=:), allocatable :: at
      class(FrequencyAspect), intent(in) :: this

      at = ''
      if(allocated(this%accumulation_type)) at = this%accumulation_type

   end function get_accumulation_type

   subroutine set_accumulation_type(aspect, accumulation_type)
      class(FrequencyAspect), intent(inout) :: aspect
      character(len=*), intent(in) :: accumulation_type

      if(accumulation_type == INSTANTANEOUS .or. accumulation_type_is_valid(accumulation_type)) then
         aspect%accumulation_type = accumulation_type
      end if

   end subroutine set_accumulation_type

   logical function matches(src, dst) result(does_match)
      class(FrequencyAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst
      type(ESMF_TimeInterval) :: this_timestep, other_timestep
      type(ESMF_TimeInterval), pointer :: zero

      does_match = .TRUE.
      zero => zero_time_interval()
      this_timestep = src%get_timestep()
      if(this_timestep == zero) return
      select type(dst)
      class is (FrequencyAspect)
         other_timestep = dst%get_timestep()
         if(other_timestep == zero) return
         if(.not. accumulation_type_is_valid(dst%get_accumulation_type())) return
         does_match = other_timestep == this_timestep
      end select

   end function matches

   function make_action(src, dst, other_aspects, rc) result(action)
      class(ExtensionAction), allocatable :: action
      class(FrequencyAspect), intent(in) :: src
      class(StateItemAspect), intent(in)  :: dst
      type(AspectMap), target, intent(in)  :: other_aspects
      integer, optional, intent(out) :: rc
      integer :: status
      character(len=:), allocatable :: accumulation_type

      select type(dst)
      class is (FrequencyAspect)
         accumulation_type = dst%get_accumulation_type()
         call get_accumulator_action(accumulation_type, ESMF_TYPEKIND_R4, action, _RC) 
         _ASSERT(allocated(action), 'Unable to allocate action')
      class default
         allocate(action,source=NullAction())
         _FAIL('FrequencyAspect cannot convert from other class.')
      end select

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(src)
   end function make_action

   subroutine connect_to_export(this, export, actual_pt, rc)
      class(FrequencyAspect), intent(inout) :: this
      class(StateItemAspect), intent(in) :: export
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(export)
      _UNUSED_DUMMY(actual_pt)
   end subroutine connect_to_export

  logical function supports_conversion_general(src) result(supports)
      class(FrequencyAspect), intent(in) :: src

      supports = .TRUE.
      _UNUSED_DUMMY(src)

   end function supports_conversion_general

   logical function supports_conversion_specific(src, dst) result(supports)
      class(FrequencyAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst
      integer :: status

      select type(dst)
      class is (FrequencyAspect)
         call intervals_and_offset_are_compatible(src%get_timestep(), &
            & dst%get_timestep(), src%get_reference_time_offset(), &
            & supports, rc=status)
         supports = supports .and. status == _SUCCESS
      end select

   end function supports_conversion_specific

   function get_aspect_id() result(aspect_id)
      type(AspectId) :: aspect_id
      aspect_id = FREQUENCY_ASPECT_ID
   end function get_aspect_id

end module mapl3g_FrequencyAspect
