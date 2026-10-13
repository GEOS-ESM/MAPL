#include "MAPL.h"

!------------------------------------------------------------------------------
! TransformBase: abstract port-declaration descriptor - named, multi-
! input/multi-output port declarations (REQ-XFORM-002/003/004) only, with
! no compute()/PortNameGraphStateItemMap/GraphStateItem dependency of any
! kind.
!
! Split out of mapl_Transform_mod (openspec/changes/merge-
! characteristic-families, task: break a circular module dependency)
! specifically so mapl_StateItemCharacteristic_mod's build_transform
! deferred method could declare its own `transformer` dummy argument's
! type without pulling in mapl_PortNameGraphStateItemMap_mod ->
! mapl_GraphStateItem_mod -> mapl_StateItemCharacteristic_mod (a genuine
! cycle, not present before that change gave StateItemCharacteristic a
! Transform-returning build_transform): GraphStateItem is the one type
! StateItemCharacteristic.F90 is itself used by (its own characteristics
! map), so anything StateItemCharacteristic.F90 uses, directly or
! transitively, must not route back through GraphStateItem.
!
! mapl_Transform_mod (Transform.F90) extends this type, adding exactly
! one thing on top: the deferred compute(this, inputs, outputs, rc)
! binding, whose inputs/outputs ARE PortNameGraphStateItemMap values (the
! transform's own already-resolved, caller-supplied bound GraphStateItem
! values) - that one additional binding is the entire reason Transform
! needs the GraphStateItem-reaching dependency chain at all;
! TransformBase needs none of it, since it only declares port NAMES/
! shapes, never touches a bound value.
!
! Every real Transform subclass (ConvertUnitsTransform, etc.) still
! extends Transform directly, unaffected by this split - TransformBase
! is purely an extracted common ancestor, not a new subclassing seam of
! its own.
!------------------------------------------------------------------------------
module mapl_TransformBase_mod
   use mapl_PortSpec_mod, only: PortSpec
   use mapl_PortSpecMap_mod
   use mapl_StateItemFlag_mod, only: MAPL_StateItem_Flag
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: TransformBase
   ! operator(==)/operator(/=) re-export: PortSpecMapIterator's own
   ! comparison operators (gFTL map template, from the wildcard `use
   ! mapl_PortSpecMap_mod` above) - mapl_TransformGraphNode_mod and its
   ! own clients (e.g. mapl_ComponentGraph_DemandDrivenUpdate_smod)
   ! iterate a PortSpecMap reached transitively through this type's own
   ! get_input_ports()/get_output_ports() and need the merged generic to
   ! compare iterators, matching mapl_Transform_mod's own pre-split
   ! precedent for this exact re-export (that module's own header
   ! comment explains the one-legitimate-exception rationale).
   public :: operator(==)
   public :: operator(/=)

   type, abstract :: TransformBase
      private
      type(PortSpecMap) :: input_ports
      type(PortSpecMap) :: output_ports
   contains
      procedure :: declare_input_port => transform_declare_input_port
      procedure :: declare_output_port => transform_declare_output_port
      procedure :: is_input_port => transform_is_input_port
      procedure :: is_output_port => transform_is_output_port
      procedure :: get_input_port => transform_get_input_port
      procedure :: get_output_port => transform_get_output_port
      procedure :: get_input_ports => transform_get_input_ports
      procedure :: get_output_ports => transform_get_output_ports
   end type TransformBase

contains

   ! -- port declarations (REQ-XFORM-002/003/004) -------------------------

   subroutine transform_declare_input_port(this, name, rc, expected_kind)
      class(TransformBase), intent(inout) :: this
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc
      type(MAPL_StateItem_Flag), optional, intent(in) :: expected_kind

      type(PortSpec) :: spec

      _ASSERT(this%input_ports%count(name) == 0, 'TransformBase: input port name already declared')

      if (present(expected_kind)) then
         spec = PortSpec(name, expected_kind)
      else
         spec = PortSpec(name)
      end if
      call this%input_ports%insert(name, spec)

      _RETURN(_SUCCESS)
   end subroutine transform_declare_input_port

   subroutine transform_declare_output_port(this, name, rc, expected_kind)
      class(TransformBase), intent(inout) :: this
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc
      type(MAPL_StateItem_Flag), optional, intent(in) :: expected_kind

      type(PortSpec) :: spec

      _ASSERT(this%output_ports%count(name) == 0, 'TransformBase: output port name already declared')

      if (present(expected_kind)) then
         spec = PortSpec(name, expected_kind)
      else
         spec = PortSpec(name)
      end if
      call this%output_ports%insert(name, spec)

      _RETURN(_SUCCESS)
   end subroutine transform_declare_output_port

   logical function transform_is_input_port(this, name) result(is_port)
      class(TransformBase), intent(in) :: this
      character(*), intent(in) :: name

      is_port = this%input_ports%count(name) > 0
   end function transform_is_input_port

   logical function transform_is_output_port(this, name) result(is_port)
      class(TransformBase), intent(in) :: this
      character(*), intent(in) :: name

      is_port = this%output_ports%count(name) > 0
   end function transform_is_output_port

   function transform_get_input_port(this, name, rc) result(spec)
      class(TransformBase), target, intent(in) :: this
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc
      type(PortSpec) :: spec

      type(PortSpec), pointer :: found

      found => this%input_ports%at(name)
      _ASSERT(associated(found), 'TransformBase: input port not declared')
      spec = found

      _RETURN(_SUCCESS)
   end function transform_get_input_port

   function transform_get_output_port(this, name, rc) result(spec)
      class(TransformBase), target, intent(in) :: this
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc
      type(PortSpec) :: spec

      type(PortSpec), pointer :: found

      found => this%output_ports%at(name)
      _ASSERT(associated(found), 'TransformBase: output port not declared')
      spec = found

      _RETURN(_SUCCESS)
   end function transform_get_output_port

   function transform_get_input_ports(this) result(ports)
      class(TransformBase), intent(in) :: this
      type(PortSpecMap) :: ports

      ports = this%input_ports
   end function transform_get_input_ports

   function transform_get_output_ports(this) result(ports)
      class(TransformBase), intent(in) :: this
      type(PortSpecMap) :: ports

      ports = this%output_ports
   end function transform_get_output_ports

end module mapl_TransformBase_mod
