!------------------------------------------------------------------------------
! Test-only mock payload type standing in for a future polymorphic node
! payload (e.g. GraphNode, spec/03-graph-node-hierarchy.md), used only to
! demonstrate REQ-ID-006: NodeId is usable directly as a gFTL map key with
! a polymorphic mapped value, without a Box wrapper type. Not part of the
! identities capability's public API.
!------------------------------------------------------------------------------
module MockPayload_mod
   implicit none
   private

   public :: MockPayload

   type, abstract :: MockPayload
   end type MockPayload

end module MockPayload_mod
