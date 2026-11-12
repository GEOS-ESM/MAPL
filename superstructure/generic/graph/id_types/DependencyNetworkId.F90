!------------------------------------------------------------------------------
! DependencyNetworkId: encapsulated identity type for DependencyNetwork
! instances within a ComponentGraph.
!
! Generated from the authoritative template spec/templates/IdTemplate.inc
! per spec/05-identities.md. Defines DEFAULT_ID before
! inclusion so a distinguished default network id is available, per
! spec/07-component-graph.md.
!------------------------------------------------------------------------------
module mapl_DependencyNetworkId_mod

#define ID_NAME DependencyNetworkId
#define DEFAULT_ID DEFAULT_DependencyNetworkId
#include "IdTemplate.inc"

end module mapl_DependencyNetworkId_mod
