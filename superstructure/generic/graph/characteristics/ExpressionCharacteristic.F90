#include "MAPL.h"

!------------------------------------------------------------------------------
! ExpressionCharacteristic: the "expression" StateItemCharacteristic, a
! ValueCharacteristic holding the declared expression string of an EXPRESSION
! item. It is descriptive only and never compared: needs_extension_for is
! always false, so a connection never needs a step for it. Evaluation is
! built per consumer at connect time; this characteristic only carries the
! string.
!
! No Info key: write_info succeeds and writes nothing (design.md Expression
! row, §3.4).
!
! No Transform exists for this axis: build_transform reports unsupported.
!------------------------------------------------------------------------------
module mapl_ExpressionCharacteristic_mod
   use mapl_StateItemCharacteristic_mod, only: StateItemCharacteristic, &
        ValueCharacteristic, report_build_transform_unsupported
   use mapl_StateItemCharacteristicKind_mod, only: &
        StateItemCharacteristicKind, EXPRESSION_CHARACTERISTIC_KIND
   use mapl_CharacteristicStatus_mod, only: CHARACTERISTIC_STATUS_SPECIFIED
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_TransformBase_mod, only: TransformBase
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: ExpressionCharacteristic

   type, extends(ValueCharacteristic) :: ExpressionCharacteristic
      private
      character(:), allocatable :: expression
   contains
      procedure, nopass :: get_kind => expression_get_kind
      procedure :: needs_extension_for => expression_needs_extension_for
      procedure :: build_transform => expression_build_transform
      procedure :: get_signature => expression_get_signature
      procedure :: write_info => expression_write_info
      procedure :: get_expression
   end type ExpressionCharacteristic

   interface ExpressionCharacteristic
      module procedure new_ExpressionCharacteristic
   end interface ExpressionCharacteristic

contains

   function new_ExpressionCharacteristic(expression) result(characteristic)
      character(*), intent(in) :: expression
      type(ExpressionCharacteristic) :: characteristic

      characteristic%expression = expression
      call characteristic%set_status(CHARACTERISTIC_STATUS_SPECIFIED)
   end function new_ExpressionCharacteristic

   function expression_get_kind() result(kind)
      type(StateItemCharacteristicKind) :: kind

      kind = EXPRESSION_CHARACTERISTIC_KIND
   end function expression_get_kind

   ! Descriptive metadata: it is never compared between two items.
   logical function expression_needs_extension_for(this, goal) result(needs)
      class(ExpressionCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(goal)
      needs = .false.
   end function expression_needs_extension_for

   function get_expression(this) result(expression)
      class(ExpressionCharacteristic), intent(in) :: this
      character(:), allocatable :: expression

      expression = ''
      if (allocated(this%expression)) expression = this%expression
   end function get_expression

   subroutine expression_build_transform(this, goal, transformer, rc)
      class(ExpressionCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal
      class(TransformBase), allocatable, intent(out) :: transformer
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(goal)

      call report_build_transform_unsupported(rc)
   end subroutine expression_build_transform

   ! No Info key for the expression string: succeeds, writes nothing.
   subroutine expression_write_info(this, handle, phase, rc)
      class(ExpressionCharacteristic), intent(in) :: this
      type(GraphStateItem), intent(inout) :: handle
      integer, intent(in) :: phase
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(handle)
      _UNUSED_DUMMY(phase)

      _RETURN(_SUCCESS)
   end subroutine expression_write_info

   function expression_get_signature(this) result(signature)
      class(ExpressionCharacteristic), intent(in) :: this
      character(:), allocatable :: signature

      signature = 'expression=' // this%get_expression()
   end function expression_get_signature

end module mapl_ExpressionCharacteristic_mod
