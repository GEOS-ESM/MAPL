! Public Export umbrella for the MAPL.enums layer.
! All enum entities carry the MAPL_ prefix, so Export = Internal.
! This is what mapl/MAPL.F90 imports from.
module mapl_Enums_export
   use mapl_Enums_internal
   implicit none(type, external)
   private

   ! ConservationType

   ! NormalizationType

   ! QuantityType

   ! VectorBasisKind

   ! ValidationMode

   ! VerificationStatus

   ! GenericPhases

   ! CouplerPhases

   ! StateItemAllocation
   public :: MAPL_StateItemAllocation
   public :: MAPL_STATEITEM_ALLOCATION_INVALID, MAPL_STATEITEM_ALLOCATION_CREATED
   public :: MAPL_STATEITEM_ALLOCATION_INACTIVE, MAPL_STATEITEM_ALLOCATION_ACTIVE
   public :: MAPL_STATEITEM_ALLOCATION_CONNECTED, MAPL_STATEITEM_ALLOCATION_ALLOCATED
   public :: operator(==), operator(/=), operator(<), operator(>=)

   ! FieldBundleType_Flag

   ! VerticalStaggerLoc

end module mapl_Enums_export
