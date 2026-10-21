#include "MAPL.h"

!------------------------------------------------------------------------------
! ExpressionTransform: the real Transform (mapl_Transform_mod) built by
! graphbuilder_run_expression_hook for an EXPRESSION-typed item's own N:1
! resolution (openspec/changes/phase-7b-expression-dispatch-wiring,
! design.md D1/D2/D4/D5). Deliberately independent of legacy's
! EvalTransform/ExtensionTransform interface (initialize/update(this,
! importState, exportState, clock, rc),
! superstructure/generic/transforms/EvalTransform.F90):
! TransformGraphNode%execute() calls Transform%compute(this, inputs,
! outputs, rc), passing this transform's own already-resolved, bound
! input/output GraphStateItem values (one per declared port name,
! openspec/changes/decouple-transform-from-graph) - no stored
! ComponentGraph/NodeId/StateRegistry of its own.
!
! Mirrors UnitsConverterTransform's/BracketTimeInterpolationTransform's
! own precedent exactly ("Deliberately independent of legacy's...
! interface... Reuses... read as a faithful behavioral reference...
! never called into"). Unlike those two (fixed 1:1 arity), this
! transform's own arity is determined per-instance from the expression
! string itself (design.md D1/D4): one named input port per referenced
! variable - the variable's own short name, not a positional convention
! (D4) - plus one 'destination_field' output port.
!
! compute() reuses StateArithmeticParser_mod::StateEval directly - the
! same byte-code expression engine legacy's own EvalTransform%update
! already calls (design.md D5) - by assembling its own bound named
! inputs into a fresh ESMF_State (one ESMF_StateAdd per referenced
! name, same name the expression string itself uses, so StateEval's own
! internal ESMF_StateGet lookup by name resolves correctly with no
! translation step).
!------------------------------------------------------------------------------
module mapl_ExpressionTransform_mod
   use mapl_Transform_mod, only: Transform
   use mapl_PortNameGraphStateItemMap_mod, only: PortNameGraphStateItemMap
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_StateArithmeticParser_mod, only: parser_variables_in_expression, StateEval
   use gFTL2_StringVector, only: StringVector, StringVectorIterator, operator(/=)
   use ESMF, only: ESMF_Field, ESMF_State, ESMF_StateCreate, ESMF_StateAdd, ESMF_StateDestroy
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: ExpressionTransform

   type, extends(Transform) :: ExpressionTransform
      private
      character(:), allocatable :: expression
      ! design.md D4: the referenced-variable names - also this
      ! transform's own declared input port names - kept so compute()
      ! can assemble the ESMF_State it hands to StateEval without
      ! re-parsing the expression on every call.
      type(StringVector) :: referenced_names
   contains
      procedure :: compute => expression_compute
   end type ExpressionTransform

   interface ExpressionTransform
      module procedure new_ExpressionTransform
   end interface ExpressionTransform

   character(*), parameter :: OUTPUT_PORT_NAME = 'destination_field'

contains

   ! design.md D2 step 4/D4: one declared input port per name
   ! parser_variables_in_expression finds in the expression string,
   ! named after the variable itself, plus one 'destination_field'
   ! output port.
   function new_ExpressionTransform(expression, rc) result(transform)
      character(*), intent(in) :: expression
      integer, optional, intent(out) :: rc
      type(ExpressionTransform) :: transform

      integer :: status
      type(StringVectorIterator) :: iter
      character(:), pointer :: name

      transform%expression = expression
      transform%referenced_names = parser_variables_in_expression(expression, _RC)

      associate (b => transform%referenced_names%begin(), e => transform%referenced_names%end())
         iter = b
         do while (iter /= e)
            name => iter%of()
            call transform%declare_input_port(name, rc=status)
            call iter%next()
         end do
      end associate
      call transform%declare_output_port(OUTPUT_PORT_NAME, rc=status)

      _RETURN(_SUCCESS)
   end function new_ExpressionTransform

   ! design.md D5: assembles this transform's own bound named inputs
   ! into a fresh ESMF_State (one ESMF_StateAdd per referenced name,
   ! under that same name) and calls StateEval directly - the exact
   ! same byte-code evaluator legacy's own EvalTransform%update already
   ! calls, just supplied a graph-native-assembled ESMF_State instead of
   ! legacy's MultiState%exportState.
   subroutine expression_compute(this, inputs, outputs, rc)
      class(ExpressionTransform), intent(inout) :: this
      type(PortNameGraphStateItemMap), intent(in) :: inputs
      type(PortNameGraphStateItemMap), intent(in) :: outputs
      integer, intent(out) :: rc

      integer :: status
      type(ESMF_State) :: state
      type(GraphStateItem), pointer :: input_item, output_item
      type(ESMF_Field) :: input_field, output_field
      type(StringVectorIterator) :: iter
      character(:), pointer :: name

      state = ESMF_StateCreate(_RC)

      associate (b => this%referenced_names%begin(), e => this%referenced_names%end())
         iter = b
         do while (iter /= e)
            name => iter%of()
            input_item => inputs%at(name)
            _ASSERT(associated(input_item), 'ExpressionTransform: input port not found in inputs')
            input_field = input_item%get_field(_RC)
            call ESMF_StateAdd(state, [input_field], _RC)
            call iter%next()
         end do
      end associate

      output_item => outputs%at(OUTPUT_PORT_NAME)
      _ASSERT(associated(output_item), 'ExpressionTransform: output port not found in outputs')
      output_field = output_item%get_field(_RC)

      call StateEval(state, this%expression, output_field, _RC)

      call ESMF_StateDestroy(state, _RC)

      _RETURN(_SUCCESS)
   end subroutine expression_compute

end module mapl_ExpressionTransform_mod
