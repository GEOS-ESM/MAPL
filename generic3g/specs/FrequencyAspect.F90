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
      type(ESMF_TimeInterval), allocatable :: timeStep
      type(ESMF_Time), allocatable :: runTime
      character(len=:), allocatable :: accumulation_type
   contains
      ! These are implementations of extended derived type.
      procedure :: matches
      procedure :: supports_conversion_general
      procedure :: supports_conversion_specific
      procedure :: make_action
      procedure :: connect_to_export
      procedure, nopass :: get_aspect_id
   end type FrequencyAspect

   interface FrequencyAspect
      module procedure :: new_FrequencyAspect
   end interface FrequencyAspect

contains

   function new_FrequencyAspect(timeStep, runTime, accumulation_type) result(aspect)
      type(FrequencyAspect) :: aspect
      type(ESMF_TimeInterval), optional, intent(in) :: timeStep
      type(ESMF_Time), optional, intent(in) :: runTime
      character(len=*), optional, intent(in) :: accumulation_type
      integer :: status

      call aspect%set_mirror(.FALSE.)
      call aspect%set_time_dependent(.FALSE.)
      call set_accumulation_type(aspect, INSTANTANEOUS)
      if(present(timeStep)) aspect%timeStep = timeStep
      if(present(runTime)) aspect%runTime = runTime
      if(present(accumulation_type)) call set_accumulation_type(aspect, accumulation_type)
      
   end function new_FrequencyAspect

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
      type(ESMF_TimeInterval) :: this_timeStep, other_timeStep
      type(ESMF_TimeInterval), pointer :: zero

      does_match = .TRUE.
      if(.not. allocated(src%timeStep)) return
      zero => zero_time_interval()
      this_timeStep = src%timeStep
      if(this_timeStep == zero) return
      select type(dst)
      class is (FrequencyAspect)
         if(.not. allocated(dst%timeStep)) return
         other_timeStep = dst%timeStep
         if(other_timeStep == zero) return
         if(.not. accumulation_type_is_valid(dst%accumulation_type)) return
         does_match = other_timeStep == this_timeStep
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
         accumulation_type = dst%accumulation_type
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

      supports = .FALSE.
      if(.not. (allocated(src%timeStep) .and. allocated(src%runTime))) return
      select type(dst)
      class is (FrequencyAspect)
         if(.not. (allocated(dst%timeStep) .and. allocated(dst%runTime))) return
         call intervals_and_offset_are_compatible(src%timeStep, dst%timeStep, &
            & src%runTime-dst%runTime, supports, rc=status)
         supports = supports .and. status == _SUCCESS
      end select

   end function supports_conversion_specific

   function get_aspect_id() result(aspect_id)
      type(AspectId) :: aspect_id
      aspect_id = FREQUENCY_ASPECT_ID
   end function get_aspect_id

end module mapl3g_FrequencyAspect
