#include "MAPL.h"

!------------------------------------------------------------------------------
! BracketTimeInterpolationTransform: the real Transform (mapl_Transform_mod)
! built by ItemShapeCharacteristic%build_transform for the BRACKET->FIELD
! pairing (ItemShapeCharacteristic.F90; openspec/changes/
! phase-7a-shape-dispatch-wiring design.md D3/D4). Deliberately independent
! of legacy's TimeInterpolateTransform/ExtensionTransform interface
! (initialize/update(this, importState, exportState, clock, rc),
! superstructure/generic/transforms/TimeInterpolateTransform.F90):
! TransformGraphNode%execute() calls Transform%compute(this, inputs,
! outputs, rc), passing this transform's own already-resolved, bound
! input/output GraphStateItem values (one per declared port name,
! openspec/changes/decouple-transform-from-graph) - no stored
! ComponentGraph/NodeId/ESMF_State of its own.
!
! Mirrors UnitsConverterTransform's own precedent exactly (that module's
! own header comment: "Deliberately independent of legacy's
! ExtensionTransform interface... Reuses... read as a faithful behavioral
! reference... never called into"). Re-implements
! TimeInterpolateTransform.F90's own run_r4/run_r8 weights-times-member
! arithmetic directly (read as the faithful behavioral reference, never
! called into), via the same mapl_field_bundle_api MAPL_FieldBundleGet
! (bundle, fieldList=..., interpolation_weights=...) call that module
! already uses - not legacy's StateRegistry/ExtensionFamily machinery.
!
! Declares its ports under the same hardcoded names every chain-built
! Transform must use (INPUT_PORT_NAME = 'source_field'/OUTPUT_PORT_NAME =
! 'destination_field', ExtensionResolution.F90's own bind_port calls in
! build_chain) even though its input port semantically holds a field
! *bundle* (the bracket) - reads it via
! inputs%at(INPUT_PORT_NAME)%get_field_bundle() instead of %get_field();
! the port name is a chain-wiring convention, not a type claim
! (design.md D4).
!------------------------------------------------------------------------------
module mapl_BracketTimeInterpolationTransform_mod
   use mapl_Transform_mod, only: Transform
   use mapl_PortNameGraphStateItemMap_mod, only: PortNameGraphStateItemMap
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_FieldPointerUtilities_mod, only: assign_fptr
   use mapl_field_bundle_api, only: MAPL_FieldBundleGet
   use MAPL_Constants, only: MAPL_UNDEFINED_REAL, MAPL_UNDEFINED_REAL64
   use ESMF, only: ESMF_Field, ESMF_FieldBundle, ESMF_FieldGet
   use ESMF, only: ESMF_TypeKind_Flag, ESMF_TYPEKIND_R4, ESMF_TYPEKIND_R8
   use ESMF, only: ESMF_KIND_R4, ESMF_KIND_R8
   use ESMF, only: operator(==)
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: BracketTimeInterpolationTransform

   type, extends(Transform) :: BracketTimeInterpolationTransform
   contains
      procedure :: compute => bracket_time_interpolation_compute
   end type BracketTimeInterpolationTransform

   interface BracketTimeInterpolationTransform
      module procedure new_BracketTimeInterpolationTransform
   end interface BracketTimeInterpolationTransform

   character(*), parameter :: INPUT_PORT_NAME = 'source_field'
   character(*), parameter :: OUTPUT_PORT_NAME = 'destination_field'

contains

   function new_BracketTimeInterpolationTransform() result(transform)
      type(BracketTimeInterpolationTransform) :: transform

      integer :: status

      call transform%declare_input_port(INPUT_PORT_NAME, rc=status)
      call transform%declare_output_port(OUTPUT_PORT_NAME, rc=status)
   end function new_BracketTimeInterpolationTransform

   subroutine bracket_time_interpolation_compute(this, inputs, outputs, rc)
      class(BracketTimeInterpolationTransform), intent(inout) :: this
      type(PortNameGraphStateItemMap), intent(in) :: inputs
      type(PortNameGraphStateItemMap), intent(in) :: outputs
      integer, intent(out) :: rc

      integer :: status
      type(GraphStateItem), pointer :: input_item, output_item
      type(ESMF_FieldBundle) :: input_bundle
      type(ESMF_Field) :: output_field
      type(ESMF_Field), allocatable :: field_list(:)
      type(ESMF_TypeKind_Flag) :: typekind

      input_item => inputs%at(INPUT_PORT_NAME)
      _ASSERT(associated(input_item), 'BracketTimeInterpolationTransform: input port not found in inputs')
      output_item => outputs%at(OUTPUT_PORT_NAME)
      _ASSERT(associated(output_item), 'BracketTimeInterpolationTransform: output port not found in outputs')

      ! design.md D4: the input port semantically holds a bundle (the
      ! bracket), not a field - get_field_bundle(), NOT get_field().
      input_bundle = input_item%get_field_bundle(_RC)
      output_field = output_item%get_field(_RC)

      call MAPL_FieldBundleGet(input_bundle, fieldList=field_list, _RC)
      call ESMF_FieldGet(field_list(1), typekind=typekind, _RC)

      if (typekind == ESMF_TYPEKIND_R4) then
         call interpolate_r4(input_bundle, output_field, _RC)
      else if (typekind == ESMF_TYPEKIND_R8) then
         call interpolate_r8(input_bundle, output_field, _RC)
      else
         _FAIL('BracketTimeInterpolationTransform: unsupported typekind')
      end if

      _RETURN(_SUCCESS)
   end subroutine bracket_time_interpolation_compute

   ! Verbatim re-implementation of TimeInterpolateTransform.F90's own
   ! run_r4 (read as the faithful behavioral reference, never called
   ! into - design.md D4), against the bundle/field directly, no
   ! ESMF_State arguments.
   subroutine interpolate_r4(bundle_in, field_out, rc)
      type(ESMF_FieldBundle), intent(in) :: bundle_in
      type(ESMF_Field), intent(inout) :: field_out
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R4), pointer :: y(:), xi(:)
      real(kind=ESMF_KIND_R4), allocatable :: weights(:)
      integer :: i
      type(ESMF_Field), allocatable :: fieldList(:)

      call MAPL_FieldBundleGet(bundle_in, fieldList=fieldList, interpolation_weights=weights, _RC)

      call assign_fptr(field_out, y, _RC)
      y = weights(1)
      do i = 1, size(fieldList)
         call assign_fptr(fieldList(i), xi, _RC)
         where (xi /= MAPL_UNDEFINED_REAL .and. y /= MAPL_UNDEFINED_REAL)
            y = y + weights(i+1) * xi
         elsewhere
            y = MAPL_UNDEFINED_REAL
         end where
      end do

      _RETURN(_SUCCESS)
   end subroutine interpolate_r4

   ! Verbatim re-implementation of TimeInterpolateTransform.F90's own
   ! run_r8 (same D4 rationale as interpolate_r4 above).
   subroutine interpolate_r8(bundle_in, field_out, rc)
      type(ESMF_FieldBundle), intent(in) :: bundle_in
      type(ESMF_Field), intent(inout) :: field_out
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R8), pointer :: y(:), xi(:)
      real(kind=ESMF_KIND_R4), allocatable :: weights(:)
      integer :: i
      type(ESMF_Field), allocatable :: fieldList(:)

      call MAPL_FieldBundleGet(bundle_in, fieldList=fieldList, interpolation_weights=weights, _RC)

      call assign_fptr(field_out, y, _RC)
      y = weights(1)
      do i = 1, size(fieldList)
         call assign_fptr(fieldList(i), xi, _RC)
         where (xi /= MAPL_UNDEFINED_REAL64 .and. y /= MAPL_UNDEFINED_REAL64)
            y = y + weights(i+1) * xi
         elsewhere
            y = MAPL_UNDEFINED_REAL64
         end where
      end do

      _RETURN(_SUCCESS)
   end subroutine interpolate_r8

end module mapl_BracketTimeInterpolationTransform_mod
