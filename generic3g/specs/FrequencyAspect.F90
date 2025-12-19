#include "MAPL.h"
#include "unused_dummy.H"
module mapl3g_FrequencyAspect
   use mapl3g_ActualConnectionPt
   use mapl3g_AspectId
   use mapl3g_StateItemAspect
   use mapl3g_AccumulatorTransformInterface
   use mapl3g_ESMF_Time_Utilities, only: check_compatibility, interval_is_all_zero
   use esmf
   implicit none
   private

   public :: FrequencyAspect

   type, extends(StateItemAspect) :: FrequencyAspect
      private
      type(ESMF_TimeInterval), allocatable :: timeStep
      type(ESMF_TimeInterval) :: offset
      character(len=:), allocatable :: accumulation_type
   contains
      ! These are implementations of extended derived type.
      procedure :: matches
      procedure :: supports_conversion_general
      procedure :: supports_conversion_specific
      procedure :: make_transform
      procedure :: connect_to_export
      procedure, nopass :: get_aspect_id
      procedure :: update_from_payload
      procedure :: update_payload
      ! These are for testing
      procedure :: get_timeStep
      procedure :: timeStep_is_set
      procedure :: get_offset
      procedure :: offset_is_set
      procedure :: get_accumulation_type
      procedure :: accumulation_type_is_set
   end type FrequencyAspect

   interface FrequencyAspect
      module procedure :: new_FrequencyAspect
   end interface FrequencyAspect

contains

   function new_FrequencyAspect(timeStep, offset, accumulation_type) result(aspect)
      type(FrequencyAspect) :: aspect
      type(ESMF_TimeInterval), optional, intent(in) :: timeStep
      type(ESMF_TimeInterval), optional, intent(in) :: offset
      character(len=*), optional, intent(in) :: accumulation_type
      integer :: status

      call aspect%set_mirror(.FALSE.)
      call aspect%set_time_dependent(.FALSE.)
      call set_accumulation_type(aspect, INSTANTANEOUS)
      call ESMF_TimeIntervalSet(aspect%offset, s=0)
      if(present(timeStep)) aspect%timeStep = timeStep
      if(present(offset)) aspect%offset = offset
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
      logical :: all_zero

      does_match = .TRUE.
      if(.not. allocated(src%timeStep)) return
      this_timeStep = src%timeStep
      call interval_is_all_zero(this_timeStep, all_zero)
      if(all_zero) return
      select type(dst)
      class is (FrequencyAspect)
         if(.not. allocated(dst%timeStep)) return
         other_timeStep = dst%timeStep
         call interval_is_all_zero(other_timeStep, all_zero)
         if(all_zero) return
         if(.not. accumulation_type_is_valid(dst%accumulation_type)) return
         does_match = other_timeStep == this_timeStep
      end select

   end function matches

   function make_transform(src, dst, other_aspects, rc) result(transform)
      class(ExtensionTransform), allocatable :: transform
      class(FrequencyAspect), intent(in) :: src
      class(StateItemAspect), intent(in)  :: dst
      type(AspectMap), target, intent(in)  :: other_aspects
      integer, optional, intent(out) :: rc
      integer :: status
      character(len=:), allocatable :: accumulation_type

      select type(dst)
      class is (FrequencyAspect)
         accumulation_type = dst%accumulation_type
         call get_accumulator_transform(accumulation_type, ESMF_TYPEKIND_R4, transform) 
         _ASSERT(allocated(transform), 'Unable to allocate transform')
      class default
         allocate(transform,source=NullTransform())
         _FAIL('FrequencyAspect cannot convert from other class.')
      end select

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(src)
   end function make_transform

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
      if(.not. allocated(src%timeStep)) return
      select type(dst)
      class is (FrequencyAspect)
         if(.not. allocated(dst%timeStep)) return
         call check_compatibility(src%timeStep, dst%timeStep, &
            & supports, offset=src%offset-dst%offset, rc=status)
         supports = supports .and. status == _SUCCESS
      end select

   end function supports_conversion_specific

   function get_aspect_id() result(aspect_id)
      type(AspectId) :: aspect_id
      aspect_id = FREQUENCY_ASPECT_ID
   end function get_aspect_id

   ! Frequency aspect is going away (I think)
   subroutine update_from_payload(this, field, bundle, state, rc)
      class(FrequencyAspect), intent(inout) :: this
      type(esmf_Field), optional, intent(in) :: field
      type(esmf_FieldBundle), optional, intent(in) :: bundle
      type(esmf_State), optional, intent(in) :: state
      integer, optional, intent(out) :: rc

      integer :: status

      _RETURN(_SUCCESS)
   end subroutine update_from_payload

   subroutine update_payload(this, field, bundle, state, rc)
      class(FrequencyAspect), intent(in) :: this
      type(esmf_Field), optional, intent(inout) :: field
      type(esmf_FieldBundle), optional, intent(inout) :: bundle
      type(esmf_State), optional, intent(inout) :: state
      integer, optional, intent(out) :: rc

       integer :: status

      ! no-op; see above

      _RETURN(_SUCCESS)
   end subroutine update_payload

   logical function timeStep_is_set(this)
      class(FrequencyAspect), intent(in) :: this
      timeStep_is_set = allocated(this%timeStep)
   end function timeStep_is_set

   type(ESMF_TimeInterval) function get_timeStep(this, rc)
      class(FrequencyAspect), intent(in) :: this
      integer, optional, intent(out) :: rc
      integer :: status
      _ASSERT(this%timeStep_is_set, 'timeStep has not been set.')
      get_timeStep = this%timeStep
   end function get_timeStep

   logical function offset_is_set(this)
      class(FrequencyAspect), intent(in) :: this
      offset_is_set = allocated(this%offset)
   end function offset_is_set

   type(ESMF_TimeInterval) function get_offset(this, rc)
      class(FrequencyAspect), intent(in) :: this
      integer, optional, intent(out) :: rc
      integer :: status
      _ASSERT(this%offset_is_set, 'offset has not been set.')
      get_offset = this%offset
   end function get_timeStep

   logical function accumulation_type_is_set(this)
      class(FrequencyAspect), intent(in) :: this
      accumulation_type_is_set = allocated(this%accumulation_type)
   end function accumulation_type_is_set

   function get_accumulation_type(this)
      character(len=:), allocatable :: get_accumulation_type
      class(FrequencyAspect), intent(in) :: this
      if(this%accumulation_type_is_set) then
         get_accumulation_type = this%accumulation_type
         return
      end if
      get_accumulation_type = ''
   end function get_accumulation_type

end module mapl3g_FrequencyAspect
