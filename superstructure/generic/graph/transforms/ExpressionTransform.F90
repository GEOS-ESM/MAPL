#include "MAPL.h"

!------------------------------------------------------------------------------
! ExpressionTransform: the real Transform (mapl_Transform_mod) built by
! build_expression_evaluation, one per consumer of an EXPRESSION item, N:1
! resolution (design.md D1/D2/D4/D5). Deliberately independent of legacy's
! EvalTransform/ExtensionTransform interface (initialize/update(this,
! importState, exportState, clock, rc),
! superstructure/generic/transforms/EvalTransform.F90):
! TransformGraphNode%execute() calls Transform%compute(this, inputs,
! outputs, rc), passing this transform's own already-resolved, bound
! input/output GraphStateItem values (one per declared port name) - no
! stored ComponentGraph/NodeId/StateRegistry of its own.
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
   use mapl_PortSpec_mod, only: PortSpec
   use mapl_PortSpecMap_mod, only: PortSpecMap
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
      procedure :: is_input_port => expression_is_input_port
      procedure :: get_input_port => expression_get_input_port
      procedure :: get_input_ports => expression_get_input_ports
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

      transform%expression = expression
      transform%referenced_names = parser_variables_in_expression(expression, _RC)

      ! Input ports are answered from referenced_names, not declared in the
      ! base port map: gfortran's polymorphic copy of a TransformGraphNode
      ! leaves a multi-entry gFTL map with dangling internal pointers.
      call transform%declare_output_port(OUTPUT_PORT_NAME, rc=status)

      _RETURN(_SUCCESS)
   end function new_ExpressionTransform

   logical function expression_is_input_port(this, name) result(is_port)
      class(ExpressionTransform), intent(in) :: this
      character(*), intent(in) :: name

      integer :: i

      is_port = .false.
      do i = 1, this%referenced_names%size()
         if (this%referenced_names%of(i) == name) then
            is_port = .true.
            return
         end if
      end do
   end function expression_is_input_port

   function expression_get_input_port(this, name, rc) result(spec)
      class(ExpressionTransform), target, intent(in) :: this
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc
      type(PortSpec) :: spec

      spec = PortSpec(name)
      _ASSERT(this%is_input_port(name), 'ExpressionTransform: input port not declared')
      _RETURN(_SUCCESS)
   end function expression_get_input_port

   function expression_get_input_ports(this) result(ports)
      class(ExpressionTransform), intent(in) :: this
      type(PortSpecMap) :: ports

      integer :: i

      do i = 1, this%referenced_names%size()
         call ports%insert(this%referenced_names%of(i), &
              PortSpec(this%referenced_names%of(i)))
      end do
   end function expression_get_input_ports

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
