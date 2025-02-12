#include "MAPL_Generic.h"
#include "unused_dummy.H"
module mapl3g_FrequencyAspect
   use mapl3g_ActualConnectionPt
   use mapl3g_AspectId
   use mapl3g_StateItemAspect
   use mapl3g_AccumulatorActionInterface
   use mapl3g_ESMF_Time_Utilities, only: times_and_intervals_are_compatible, zero_time_interval
   use esmf
   implicit none
   private

   public :: FrequencyAspect

   type, extends(StateItemAspect) :: FrequencyAspect
      private
      type(ESMF_TimeInterval) :: timestep_
      type(ESMF_Time) :: refTime_
      character(len=:), allocatable :: accumulation_type_
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
      procedure :: set_timestep
      procedure :: get_accumulation_type
      procedure :: set_accumulation_type
      procedure :: get_reference_time
      procedure :: set_reference_time
      procedure, private :: zero_timestep
   end type FrequencyAspect

   interface FrequencyAspect
      module procedure :: new_FrequencyAspect
   end interface FrequencyAspect

contains

   function new_FrequencyAspect(timeStep, refTime, accumulation_type) result(aspect)
      type(FrequencyAspect) :: aspect
      type(ESMF_TimeInterval), optional, intent(in) :: timeStep
      type(ESMF_Time), optional, intent(in) :: refTime
      character(len=*), optional, intent(in) :: accumulation_type

      call aspect%set_mirror(.FALSE.)
      call aspect%set_time_dependent(.FALSE.)
      call aspect%set_accumulation_type(INSTANTANEOUS)
      call aspect%zero_timestep()
      if(present(timeStep)) aspect%timestep_ = timeStep
      if(present(refTime)) aspect%refTime_ = refTime
      if(present(accumulation_type)) call aspect%set_accumulation_type(accumulation_type)
      
   end function new_FrequencyAspect

   function get_timestep(this) result(ts)
      type(ESMF_TimeInterval) :: ts
      class(FrequencyAspect), intent(in) :: this

      ts = this%timestep_

   end function get_timestep

   subroutine set_timestep(this, timeStep)
      class(FrequencyAspect), intent(inout) :: this
      type(ESMF_TimeInterval), intent(in) :: timeStep

      this%timestep_ = timeStep

   end subroutine set_timestep

   function get_reference_time(this) result(time)
      type(ESMF_Time) :: time
      class(FrequencyAspect), intent(in) :: this

      time = this%refTime_

   end function get_reference_time

   subroutine set_reference_time(this, time)
      class(FrequencyAspect), intent(inout) :: this
      type(ESMF_Time), intent(in) :: time

      this%refTime_ = time

   end subroutine set_reference_time

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
         if(.not. accumulation_type_is_valid(src%get_accumulation_type())) return
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
         call times_and_intervals_are_compatible(&
            & dst%get_timestep(), dst%get_reference_time(),&
            & src%get_timestep(), src%get_reference_time(),&
            & supports, rc=status)
         supports = supports .and. status == _SUCCESS
      end select

   end function supports_conversion_specific

   function get_aspect_id() result(aspect_id)
      type(AspectId) :: aspect_id
      aspect_id = FREQUENCY_ASPECT_ID
   end function get_aspect_id

end module mapl3g_FrequencyAspect
