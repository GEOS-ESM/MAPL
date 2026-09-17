!------------------------------------------------------------------------------
! Test-only helper for pFUnit suites exercising the identities capability
! (spec/05-identities.md). Seeds a generator produced by
! spec/templates/IdTemplate.inc near INT32 exhaustion so exhaustion-detection
! scenarios can run without ~2^31 calls to next().
!
! Uses transfer() to reinterpret the generator's physical representation
! rather than any named-component access: each <ID_NAME>Generator has
! exactly one integer(INT32) component (its private next_value counter),
! so this does not require, and does not depend on, any change to the
! encapsulation in the generated modules (REQ-ID-001). Confirmed empirically
! under nagfor; see design.md "Risks / Trade-offs".
!
! One concrete overload per sibling ID type (rather than an
! unlimited-polymorphic dummy) because Fortran forbids intrinsic
! assignment to a polymorphic left-hand side.
!------------------------------------------------------------------------------
module IdGeneratorTestSupport_mod
   use, intrinsic :: iso_fortran_env, only: INT32
   use mapl_NodeId_mod, only: NodeIdGenerator
   use mapl_DependencyNetworkId_mod, only: DependencyNetworkIdGenerator
   use mapl_PortId_mod, only: PortIdGenerator
   use mapl_CallbackInterfaceId_mod, only: CallbackInterfaceIdGenerator
   implicit none
   private

   public :: seed_generator_near_exhaustion

   interface seed_generator_near_exhaustion
      module procedure seed_NodeIdGenerator
      module procedure seed_DependencyNetworkIdGenerator
      module procedure seed_PortIdGenerator
      module procedure seed_CallbackInterfaceIdGenerator
   end interface seed_generator_near_exhaustion

contains

   subroutine seed_NodeIdGenerator(generator)
      type(NodeIdGenerator), intent(inout) :: generator
      generator = transfer(huge(1_INT32) - 1_INT32, generator)
   end subroutine seed_NodeIdGenerator

   subroutine seed_DependencyNetworkIdGenerator(generator)
      type(DependencyNetworkIdGenerator), intent(inout) :: generator
      generator = transfer(huge(1_INT32) - 1_INT32, generator)
   end subroutine seed_DependencyNetworkIdGenerator

   subroutine seed_PortIdGenerator(generator)
      type(PortIdGenerator), intent(inout) :: generator
      generator = transfer(huge(1_INT32) - 1_INT32, generator)
   end subroutine seed_PortIdGenerator

   subroutine seed_CallbackInterfaceIdGenerator(generator)
      type(CallbackInterfaceIdGenerator), intent(inout) :: generator
      generator = transfer(huge(1_INT32) - 1_INT32, generator)
   end subroutine seed_CallbackInterfaceIdGenerator

end module IdGeneratorTestSupport_mod
