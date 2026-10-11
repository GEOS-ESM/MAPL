#include "MAPL.h"

!------------------------------------------------------------------------------
! TypeKindConverterTransform: the real Transform (mapl_Transform_mod) built
! by TypeKindCharacteristicAdapter%build_transform
! (TypeKindCharacteristicAdapter.F90; openspec/changes/
! phase-10c-typekind-characteristic design.md D2) - a pure numeric R4<->R8
! kind cast, structurally mirroring UnitsConverterTransform.F90 line for
! line (TransformGraphNode%execute() calls Transform%compute(this, rc)
! with no other arguments, so this type holds the owning ComponentGraph
! plus the input/output NodeId itself, set at construction time by
! whoever builds the chain).
!
! Named TypeKindConverterTransform, not CopyTransform: that name is
! already taken by the legacy, ExtensionTransform-based type
! (superstructure/generic/transforms/CopyTransform.F90), and was itself
! flagged as a poor placeholder in docs/graph/spec/
! 18-state-item-characteristics.md - no rename-to-dodge precedent to
! follow here (confirmed by direct grep at implementation time: neither
! `TypeKindConverterTransform` nor `mapl_TypeKindConverterTransform_mod`
! exists anywhere in superstructure/generic/transforms/ or
! superstructure/generic/graph/ before this file).
!
! Deliberately no udunits2 dependency - unlike UnitsConverterTransform,
! this is a pure numeric kind cast, not a unit conversion, so compute()
! only needs ESMF_FieldGet (to confirm the actual runtime typekinds)
! plus mapl_FieldPointerUtilities_mod::assign_fptr and an ordinary
! Fortran `real(..., kind=...)` cast.
!------------------------------------------------------------------------------
module mapl_TypeKindConverterTransform_mod
   use mapl_Transform_mod, only: Transform
   use mapl_ComponentGraph_mod, only: ComponentGraph
   use mapl_NodeId_mod, only: NodeId
   use mapl_GraphNode_mod, only: GraphNode
   use mapl_StateItemNode_mod, only: StateItemNode
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_FieldPointerUtilities_mod, only: assign_fptr
   use ESMF, only: ESMF_Field, ESMF_TypeKind_Flag
   use ESMF, only: ESMF_TYPEKIND_R4, ESMF_TYPEKIND_R8
   use ESMF, only: ESMF_KIND_R4, ESMF_KIND_R8
   use ESMF, only: operator(==)
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: TypeKindConverterTransform

   type, extends(Transform) :: TypeKindConverterTransform
      private
      class(ComponentGraph), pointer :: graph => null()
      type(NodeId) :: input_node_id
      type(NodeId) :: output_node_id
      type(ESMF_TypeKind_Flag) :: src_typekind
      type(ESMF_TypeKind_Flag) :: dst_typekind
   contains
      procedure :: compute => typekind_compute
   end type TypeKindConverterTransform

   interface TypeKindConverterTransform
      module procedure new_TypeKindConverterTransform
   end interface TypeKindConverterTransform

   character(*), parameter :: INPUT_PORT_NAME = 'source_field'
   character(*), parameter :: OUTPUT_PORT_NAME = 'destination_field'

contains

   function new_TypeKindConverterTransform(graph, input_node_id, output_node_id, &
        src_typekind, dst_typekind) result(transform)
      class(ComponentGraph), target, intent(in) :: graph
      type(NodeId), intent(in) :: input_node_id
      type(NodeId), intent(in) :: output_node_id
      type(ESMF_TypeKind_Flag), intent(in) :: src_typekind
      type(ESMF_TypeKind_Flag), intent(in) :: dst_typekind
      type(TypeKindConverterTransform) :: transform

      integer :: status

      transform%graph => graph
      transform%input_node_id = input_node_id
      transform%output_node_id = output_node_id
      transform%src_typekind = src_typekind
      transform%dst_typekind = dst_typekind

      call transform%declare_input_port(INPUT_PORT_NAME, rc=status)
      call transform%declare_output_port(OUTPUT_PORT_NAME, rc=status)
   end function new_TypeKindConverterTransform

   subroutine typekind_compute(this, rc)
      class(TypeKindConverterTransform), intent(inout) :: this
      integer, intent(out) :: rc

      integer :: status
      class(GraphNode), pointer :: input_node, output_node
      type(GraphStateItem) :: input_item, output_item
      type(ESMF_Field) :: input_field, output_field

      input_node => this%graph%get_node(this%input_node_id)
      _ASSERT(associated(input_node), 'TypeKindConverterTransform: input NodeId not found in graph')
      output_node => this%graph%get_node(this%output_node_id)
      _ASSERT(associated(output_node), 'TypeKindConverterTransform: output NodeId not found in graph')

      select type (input_node)
      class is (StateItemNode)
         input_item = input_node%get_payload()
      class default
         _FAIL('TypeKindConverterTransform: input NodeId does not refer to a StateItemNode')
      end select

      select type (output_node)
      class is (StateItemNode)
         output_item = output_node%get_payload()
      class default
         _FAIL('TypeKindConverterTransform: output NodeId does not refer to a StateItemNode')
      end select

      input_field = input_item%get_field(_RC)
      output_field = output_item%get_field(_RC)

      if (this%src_typekind == ESMF_TYPEKIND_R4 .and. this%dst_typekind == ESMF_TYPEKIND_R8) then
         call cast_r4_to_r8(input_field, output_field, _RC)
      else if (this%src_typekind == ESMF_TYPEKIND_R8 .and. this%dst_typekind == ESMF_TYPEKIND_R4) then
         call cast_r8_to_r4(input_field, output_field, _RC)
      else
         _FAIL('TypeKindConverterTransform: unsupported typekind pairing')
      end if

      _RETURN(_SUCCESS)
   end subroutine typekind_compute

   subroutine cast_r4_to_r8(input_field, output_field, rc)
      type(ESMF_Field), intent(inout) :: input_field, output_field
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R4), pointer :: x_in(:)
      real(kind=ESMF_KIND_R8), pointer :: x_out(:)

      call assign_fptr(input_field, x_in, _RC)
      call assign_fptr(output_field, x_out, _RC)
      x_out = real(x_in, kind=ESMF_KIND_R8)

      _RETURN(_SUCCESS)
   end subroutine cast_r4_to_r8

   subroutine cast_r8_to_r4(input_field, output_field, rc)
      type(ESMF_Field), intent(inout) :: input_field, output_field
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R8), pointer :: x_in(:)
      real(kind=ESMF_KIND_R4), pointer :: x_out(:)

      call assign_fptr(input_field, x_in, _RC)
      call assign_fptr(output_field, x_out, _RC)
      x_out = real(x_in, kind=ESMF_KIND_R4)

      _RETURN(_SUCCESS)
   end subroutine cast_r8_to_r4

end module mapl_TypeKindConverterTransform_mod
