!------------------------------------------------------------------------------
! VariableSpecTag: bare, empty abstract marker type. Exists solely so
! VariableSpecMemberMap (VariableSpecMemberMap.F90) can be templated
! polymorphically over it, which is what lets VariableSpec
! (VariableSpec.F90) contain a map of itself (its declared composite
! members) without a circular module dependency - see
! openspec/changes/composite-state-spec/design.md, Decisions. No
! components, no deferred procedures: this type carries no behavior of
! its own.
!------------------------------------------------------------------------------
module mapl_VariableSpecTag_mod
   implicit none(type, external)
   private

   public :: VariableSpecTag

   type, abstract :: VariableSpecTag
   end type VariableSpecTag

end module mapl_VariableSpecTag_mod
