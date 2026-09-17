#include "MAPL.h"

!------------------------------------------------------------------------------
! UnitsConverterTransform: the one real Transform (mapl_Transform_mod)
! built by a Characteristic%build_transform in the extension-reuse
! change (UnitsCharacteristic.F90; design.md Decisions - "Extension-chain
! execution ships with one real characteristic provider"). Deliberately
! independent of legacy's ExtensionTransform interface (initialize/
! update(this, importState, exportState, clock, rc),
! superstructure/generic/transforms/ExtensionTransform.F90):
! TransformGraphNode%execute() calls Transform%compute(this, rc) with no
! other arguments (see mapl_ComponentGraph_DemandDrivenUpdate_smod), so
! this type holds the owning ComponentGraph plus the input/output NodeId
! itself, set at construction time by whoever builds the chain
! (UnitsCharacteristic%build_transform), and fetches both bound
! GraphStateItems' ESMF_Fields directly at compute() time.
!
! Named UnitsConverterTransform, not ConvertUnitsTransform, because
! `mapl_ConvertUnitsTransform_mod`/`ConvertUnitsTransform`
! (superstructure/generic/transforms/ConvertUnitsTransform.F90) is
! already taken by the legacy module this type is deliberately
! independent of - module names share one global namespace, so the two
! cannot coexist under the same name even though they live in different
! modules. Rename to ConvertUnitsTransform (matching RegridTransform's
! naming convention) once the legacy module is removed - tracked as a
! task in the 3c2 follow-up (docs/graph/spec/20-implementation-roadmap.md).
!
! Reuses two general-purpose infrastructure pieces, not legacy
! StateRegistry/ExtensionFamily machinery: mapl_FieldPointerUtilities_mod
! (assign_fptr - the same low-level ESMF field-pointer helper used
! throughout infrastructure/field) and udunits2f's Converter (the same
! real unit-conversion library legacy's own ConvertUnitsTransform uses -
! read as a faithful behavioral reference for the r4/r8 typekind
! dispatch, never called into).
!------------------------------------------------------------------------------
module mapl_UnitsConverterTransform_mod
   use mapl_Transform_mod, only: Transform
   use mapl_ComponentGraph_mod, only: ComponentGraph
   use mapl_NodeId_mod, only: NodeId
   use mapl_GraphNode_mod, only: GraphNode
   use mapl_StateItemNode_mod, only: StateItemNode
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_FieldPointerUtilities_mod, only: assign_fptr
   use udunits2f, only: UDUNITS_Converter => Converter
   use udunits2f, only: UDUNITS_GetConverter => get_converter
   use ESMF, only: ESMF_Field, ESMF_FieldGet, ESMF_TypeKind_Flag
   use ESMF, only: ESMF_TYPEKIND_R4, ESMF_TYPEKIND_R8
   use ESMF, only: ESMF_KIND_R4, ESMF_KIND_R8
   use ESMF, only: operator(==)
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: UnitsConverterTransform

   type, extends(Transform) :: UnitsConverterTransform
      private
      class(ComponentGraph), pointer :: graph => null()
      type(NodeId) :: input_node_id
      type(NodeId) :: output_node_id
      character(:), allocatable :: src_units
      character(:), allocatable :: dst_units
      type(UDUNITS_Converter) :: converter
      logical :: converter_ready = .false.
   contains
      procedure :: compute => units_compute
   end type UnitsConverterTransform

   interface UnitsConverterTransform
      module procedure new_UnitsConverterTransform
   end interface UnitsConverterTransform

   character(*), parameter :: INPUT_PORT_NAME = 'source_field'
   character(*), parameter :: OUTPUT_PORT_NAME = 'destination_field'

contains

   function new_UnitsConverterTransform(graph, input_node_id, output_node_id, &
        src_units, dst_units) result(transform)
      class(ComponentGraph), target, intent(in) :: graph
      type(NodeId), intent(in) :: input_node_id
      type(NodeId), intent(in) :: output_node_id
      character(*), intent(in) :: src_units
      character(*), intent(in) :: dst_units
      type(UnitsConverterTransform) :: transform

      integer :: status

      transform%graph => graph
      transform%input_node_id = input_node_id
      transform%output_node_id = output_node_id
      transform%src_units = src_units
      transform%dst_units = dst_units

      call transform%declare_input_port(INPUT_PORT_NAME, rc=status)
      call transform%declare_output_port(OUTPUT_PORT_NAME, rc=status)
   end function new_UnitsConverterTransform

   subroutine units_compute(this, rc)
      class(UnitsConverterTransform), intent(inout) :: this
      integer, intent(out) :: rc

      integer :: status
      class(GraphNode), pointer :: input_node, output_node
      type(GraphStateItem) :: input_item, output_item
      type(ESMF_Field) :: input_field, output_field
      type(ESMF_TypeKind_Flag) :: typekind

      if (.not. this%converter_ready) then
         call UDUNITS_GetConverter(this%converter, from=this%src_units, to=this%dst_units, _RC)
         this%converter_ready = .true.
      end if

      input_node => this%graph%get_node(this%input_node_id)
      _ASSERT(associated(input_node), 'UnitsConverterTransform: input NodeId not found in graph')
      output_node => this%graph%get_node(this%output_node_id)
      _ASSERT(associated(output_node), 'UnitsConverterTransform: output NodeId not found in graph')

      select type (input_node)
      class is (StateItemNode)
         input_item = input_node%get_payload()
      class default
         _FAIL('UnitsConverterTransform: input NodeId does not refer to a StateItemNode')
      end select

      select type (output_node)
      class is (StateItemNode)
         output_item = output_node%get_payload()
      class default
         _FAIL('UnitsConverterTransform: output NodeId does not refer to a StateItemNode')
      end select

      input_field = input_item%get_field(_RC)
      output_field = output_item%get_field(_RC)

      call ESMF_FieldGet(input_field, typekind=typekind, _RC)

      if (typekind == ESMF_TYPEKIND_R4) then
         call convert_r4(input_field, output_field, this%converter, _RC)
      else if (typekind == ESMF_TYPEKIND_R8) then
         call convert_r8(input_field, output_field, this%converter, _RC)
      else
         _FAIL('UnitsConverterTransform: unsupported typekind')
      end if

      _RETURN(_SUCCESS)
   end subroutine units_compute

   subroutine convert_r4(input_field, output_field, converter, rc)
      type(ESMF_Field), intent(inout) :: input_field, output_field
      type(UDUNITS_Converter), intent(in) :: converter
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R4), pointer :: x_in(:), x_out(:)

      call assign_fptr(input_field, x_in, _RC)
      call assign_fptr(output_field, x_out, _RC)
      x_out = converter%convert(x_in)

      _RETURN(_SUCCESS)
   end subroutine convert_r4

   subroutine convert_r8(input_field, output_field, converter, rc)
      type(ESMF_Field), intent(inout) :: input_field, output_field
      type(UDUNITS_Converter), intent(in) :: converter
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R8), pointer :: x_in(:), x_out(:)

      call assign_fptr(input_field, x_in, _RC)
      call assign_fptr(output_field, x_out, _RC)
      x_out = converter%convert(x_in)

      _RETURN(_SUCCESS)
   end subroutine convert_r8

end module mapl_UnitsConverterTransform_mod
