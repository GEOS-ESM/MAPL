! Public Export umbrella for the MAPL.enums layer.
! All enum entities carry the MAPL_ prefix, so Export = Internal.
! This is what mapl/MAPL.F90 imports from.
module mapl_enums_api
   use mapl_Enums_internal
   ! Vertical enums
   use mapl_VerticalAlignment_mod
   use mapl_VerticalStaggerLoc_mod
   use mapl_QuantityTypeMetadata_mod, mapl_QuantityTypeMetadata => QuantityTypeMetadata, &
        mapl_MakeQuantityTypeMetadata => make_QuantityTypeMetadata
   use mapl_NormalizationMetadata_mod, mapl_NormalizationMetadata => NormalizationMetadata, &
        mapl_MakeNormalizationMetadata => make_NormalizationMetadata
   use mapl_ConservationMetadata_mod, mapl_ConservationMetadata => ConservationMetadata, &
        mapl_MakeConservationMetadata => make_ConservationMetadata
   implicit none(type, external)
   private

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


   ! Vertical stagger locations
   public :: MAPL_VerticalStaggerLoc
   public :: MAPL_VERTICAL_STAGGER_NONE
   public :: MAPL_VERTICAL_STAGGER_EDGE
   public :: MAPL_VERTICAL_STAGGER_CENTER
   public :: MAPL_VERTICAL_STAGGER_MIRROR
   public :: MAPL_VERTICAL_STAGGER_INVALID

   ! Vertical alignment
   public :: VerticalAlignment
   public :: VALIGN_WITH_GRID
   public :: VALIGN_UP
   public :: VALIGN_DOWN
   public :: VALIGN_INVALID

   ! ConservationType
   public :: MAPL_ConservationType
   public :: MAPL_CONSERVE_NONE, MAPL_CONSERVE_MASS
   public :: MAPL_CONSERVE_ENERGY, MAPL_CONSERVE_MOMENTUM

   ! NormalizationType
   public :: MAPL_NormalizationType
   public :: MAPL_NORMALIZE_NONE, MAPL_NORMALIZE_DELP, MAPL_NORMALIZE_DZ

   ! QuantityType
   public :: MAPL_QuantityType, MAPL_MixingRatioBasis
   public :: MAPL_QUANTITY_UNKNOWN, MAPL_QUANTITY_MIXING_RATIO
   public :: MAPL_QUANTITY_CONCENTRATION, MAPL_QUANTITY_TEMPERATURE
   public :: MAPL_QUANTITY_PRESSURE, MAPL_QUANTITY_EXTENSIVE
   public :: MAPL_BASIS_NONE, MAPL_BASIS_WET_MASS, MAPL_BASIS_DRY_MASS, MAPL_BASIS_VOLUME

   public :: mapl_QuantityTypeMetadata
   public :: mapl_MakeQuantityTypeMetadata

   public :: mapl_NormalizationMetadata
   public :: mapl_MakeNormalizationMetadata
   public :: mapl_ConservationMetadata
   public :: mapl_MakeConservationMetadata
end module mapl_enums_api
