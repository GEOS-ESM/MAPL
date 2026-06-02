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

   ! CouplerPhases
   public :: MAPL_GENERIC_COUPLER_INITIALIZE, MAPL_GENERIC_COUPLER_UPDATE
   public :: MAPL_GENERIC_COUPLER_INVALIDATE, MAPL_GENERIC_COUPLER_CLOCK_ADVANCE

   ! GenericPhases
   public :: MAPL_GENERIC_INIT_PHASE_SEQUENCE
   public :: MAPL_GENERIC_INIT_SET_CLOCK
   public :: MAPL_GENERIC_INIT_GEOM_A
   public :: MAPL_GENERIC_INIT_GEOM_B
   public :: MAPL_GENERIC_INIT_ADVERTISE
   public :: MAPL_GENERIC_INIT_MODIFY_ADVERTISED
   public :: MAPL_GENERIC_INIT_REALIZE
   public :: MAPL_GENERIC_INIT_READ_RESTART
   public :: MAPL_GENERIC_INIT_USER

   ! Run phases
   public :: MAPL_GENERIC_RUN_OFFSET
   public :: MAPL_GENERIC_RUN_CLOCK_ADVANCE
   public :: MAPL_GENERIC_RUN_USER

   ! Finalize phases
   public :: MAPL_GENERIC_FINALIZE_USER



   ! StateItemAllocation
   public :: MAPL_StateItemAllocation
   public :: MAPL_STATEITEM_ALLOCATION_INVALID, MAPL_STATEITEM_ALLOCATION_CREATED
   public :: MAPL_STATEITEM_ALLOCATION_INACTIVE, MAPL_STATEITEM_ALLOCATION_ACTIVE
   public :: MAPL_STATEITEM_ALLOCATION_CONNECTED, MAPL_STATEITEM_ALLOCATION_ALLOCATED
   public :: operator(==), operator(/=), operator(<), operator(>=)

   ! FieldBundleType_Flag
   public :: MAPL_FieldBundleType_Flag
   public :: MAPL_FIELDBUNDLETYPE_BASIC, MAPL_FIELDBUNDLETYPE_VECTOR
   public :: MAPL_FIELDBUNDLETYPE_BRACKET, MAPL_FIELDBUNDLETYPE_VECTORBRACKET
   public :: MAPL_FIELDBUNDLETYPE_SERVICE, MAPL_FIELDBUNDLETYPE_SERVICE_AGGREGATE
   public :: MAPL_FIELDBUNDLETYPE_SERVICE_SEPARATE, MAPL_FIELDBUNDLETYPE_INVALID

   ! VectorBasisKind
   public :: MAPL_VectorBasisKind
   public :: MAPL_VECTOR_BASIS_KIND_INVALID, MAPL_VECTOR_BASIS_KIND_GRID
   public :: MAPL_VECTOR_BASIS_KIND_NS

   ! VerticalStaggerLoc

end module mapl_Enums_export
