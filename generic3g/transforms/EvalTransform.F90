#include "MAPL.h"

module mapl3g_EvalTransform
   use mapl3g_ExtensionTransform
   use mapl3g_TransformId
   use mapl3g_StateItem
   use mapl3g_ComponentDriver
   use mapl3g_ComponentDriverVector
   use mapl3g_CouplerPhases, only: GENERIC_COUPLER_UPDATE, GENERIC_COUPLER_INITIALIZE
   use mapl_ErrorHandling
   use MAPL_StateArithmeticParserMod
   use esmf

   implicit none(type,external)
   private

   public :: EvalTransform
   public :: EXPORT_FIELD_NAME

   type, extends(ExtensionTransform) :: EvalTransform
      private
      character(:), allocatable :: expression
      type(ComponentDriverVector) :: input_couplers
      type(ESMF_State) :: input_state
   contains
      procedure :: initialize
      procedure :: update
      procedure :: get_transformId
   end type EvalTransform

   interface EvalTransform
      procedure :: new_EvalTransform
   end interface EvalTransform

   character(len=*), parameter :: EXPORT_FIELD_NAME = 'export[1]'
contains

   function new_EvalTransform(expression, input_state, input_couplers) result(transform)
      type(EvalTransform) :: transform
      character(*), intent(in) :: expression
      type(ESMF_State), intent(in) :: input_state
      type(ComponentDriverVector), intent(in) :: input_couplers

      transform%expression = expression
      transform%input_state = input_state
      transform%input_couplers = input_couplers

   end function new_EvalTransform

   subroutine initialize(this, importState, exportState, clock, rc)
      class(EvalTransform), intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, optional, intent(out) :: rc

      integer :: status

      call initialize_with_target_attr(this, importState, exportState, clock, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)

   contains

      ! We need TARGET so that we can iterate over the internal map.  But the
      ! base class does not have TARGET attribute on "this".
      subroutine initialize_with_target_attr(this, importState, exportState, clock, rc)
         class(EvalTransform), target, intent(inout) :: this
         type(ESMF_State) :: importState
         type(ESMF_State) :: exportState
         type(ESMF_Clock) :: clock
         integer, optional, intent(out) :: rc
         
         integer :: status
         type(ComponentDriverVectorIterator) :: iter
         class(ComponentDriver), pointer :: coupler

         associate (e => this%input_couplers%ftn_end())
           iter = this%input_couplers%ftn_begin()
           do while (iter /= e)
              call iter%next()
              coupler => iter%of()
              call coupler%initialize(phase_idx=GENERIC_COUPLER_INITIALIZE, _RC)
           end do
         end associate
         _RETURN(_SUCCESS)
      end subroutine initialize_with_target_attr

   end subroutine initialize
   
   subroutine update(this, importState, exportState, clock, rc)
      class(EvalTransform), intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentDriverVectorIterator) :: iter
      type(ESMF_Field) :: f

      call update_with_target_attr(this, importState, exportState, clock, _RC)

      call ESMF_StateGet(exportState, itemName=EXPORT_FIELD_NAME, field=f, _RC)

      call MAPL_StateEval(this%input_state, this%expression, f, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(clock)

   contains

      ! We need TARGET so that we can iterate over the internal map.  But the
      ! base class does not have TARGET attribute on "this".
      subroutine update_with_target_attr(this, importState, exportState, clock, rc)
         class(EvalTransform), target, intent(inout) :: this
         type(ESMF_State) :: importState
         type(ESMF_State) :: exportState
         type(ESMF_Clock) :: clock
         integer, optional, intent(out) :: rc

         integer :: status
         type(ComponentDriverVectorIterator) :: iter
         class(ComponentDriver), pointer :: coupler
         
         associate (e => this%input_couplers%ftn_end())
           iter = this%input_couplers%ftn_begin()
           do while (iter /= e)
              call iter%next()
              coupler => iter%of()
              call coupler%run(phase_idx=GENERIC_COUPLER_UPDATE, _RC)
           end do
         end associate
         
         _RETURN(_SUCCESS)
      end subroutine update_with_target_attr

   end subroutine update

   function get_transformId(this) result(id)
      type(TransformId) :: id
      class(EvalTransform), intent(in) :: this

      id = EVAL_TRANSFORM_ID
   end function get_transformId

end module mapl3g_EvalTransform
