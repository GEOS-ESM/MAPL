! Public Export umbrella for the MAPL.enums layer.
! All enum entities carry the MAPL_ prefix.
! This is what mapl/MAPL.F90 imports from.
module mapl_enums_api
   ! ConservationType
   use mapl_ConservationType_mod, only: MAPL_CONSERVE_NONE     => CONSERVE_NONE
   use mapl_ConservationType_mod, only: MAPL_CONSERVE_MASS     => CONSERVE_MASS
   use mapl_ConservationType_mod, only: MAPL_CONSERVE_ENERGY   => CONSERVE_ENERGY
   use mapl_ConservationType_mod, only: MAPL_CONSERVE_MOMENTUM => CONSERVE_MOMENTUM
   use mapl_ConservationType_mod, only: MAPL_ConservationType   => ConservationType

   ! NormalizationType
   use mapl_NormalizationType_mod, only: MAPL_NORMALIZE_NONE => NORMALIZE_NONE
   use mapl_NormalizationType_mod, only: MAPL_NORMALIZE_DELP => NORMALIZE_DELP
   use mapl_NormalizationType_mod, only: MAPL_NORMALIZE_DZ   => NORMALIZE_DZ
   use mapl_NormalizationType_mod, only: MAPL_NormalizationType => NormalizationType
   use mapl_NormalizationType_mod, only: operator(==), operator(/=)

   ! QuantityType
   use mapl_QuantityType_mod, only: MAPL_QuantityType            => QuantityType
   use mapl_QuantityType_mod, only: MAPL_MixingRatioBasis        => MixingRatioBasis
   use mapl_QuantityType_mod, only: MAPL_QUANTITY_UNKNOWN        => QUANTITY_UNKNOWN
   use mapl_QuantityType_mod, only: MAPL_QUANTITY_MIXING_RATIO   => QUANTITY_MIXING_RATIO
   use mapl_QuantityType_mod, only: MAPL_QUANTITY_CONCENTRATION  => QUANTITY_CONCENTRATION
   use mapl_QuantityType_mod, only: MAPL_QUANTITY_TEMPERATURE    => QUANTITY_TEMPERATURE
   use mapl_QuantityType_mod, only: MAPL_QUANTITY_PRESSURE       => QUANTITY_PRESSURE
   use mapl_QuantityType_mod, only: MAPL_QUANTITY_EXTENSIVE      => QUANTITY_EXTENSIVE
   use mapl_QuantityType_mod, only: MAPL_BASIS_NONE              => BASIS_NONE
   use mapl_QuantityType_mod, only: MAPL_BASIS_WET_MASS          => BASIS_WET_MASS
   use mapl_QuantityType_mod, only: MAPL_BASIS_DRY_MASS          => BASIS_DRY_MASS
   use mapl_QuantityType_mod, only: MAPL_BASIS_VOLUME            => BASIS_VOLUME
   use mapl_QuantityType_mod, only: operator(==), operator(/=)

   ! VectorBasisKind
   use mapl_VectorBasisKind_mod, only: MAPL_VECTOR_BASIS_KIND_INVALID => VECTOR_BASIS_KIND_INVALID
   use mapl_VectorBasisKind_mod, only: MAPL_VECTOR_BASIS_KIND_GRID    => VECTOR_BASIS_KIND_GRID
   use mapl_VectorBasisKind_mod, only: MAPL_VECTOR_BASIS_KIND_NS      => VECTOR_BASIS_KIND_NS
   use mapl_VectorBasisKind_mod, only: MAPL_VectorBasisKind            => VectorBasisKind
   use mapl_VectorBasisKind_mod, only: operator(==), operator(/=)

   ! ValidationMode
   use mapl_ValidationMode_mod, only: MAPL_VALIDATION_MODE_PERMISSIVE => VALIDATION_MODE_PERMISSIVE
   use mapl_ValidationMode_mod, only: MAPL_VALIDATION_MODE_STRICT     => VALIDATION_MODE_STRICT
   use mapl_ValidationMode_mod, only: MAPL_ValidationMode              => ValidationMode

   ! VerificationStatus
   use mapl_VerificationStatus_mod, only: MAPL_VERIFICATION_STATUS_UNVERIFIED   => VERIFICATION_STATUS_UNVERIFIED
   use mapl_VerificationStatus_mod, only: MAPL_VERIFICATION_STATUS_VERIFIED     => VERIFICATION_STATUS_VERIFIED
   use mapl_VerificationStatus_mod, only: MAPL_VERIFICATION_STATUS_CF_COMPLIANT => VERIFICATION_STATUS_CF_COMPLIANT
   use mapl_VerificationStatus_mod, only: MAPL_VerificationStatus                => VerificationStatus

   ! GenericPhases
   use mapl_GenericPhases_mod, only: MAPL_GENERIC_INIT_PHASE_SEQUENCE  => GENERIC_INIT_PHASE_SEQUENCE
   use mapl_GenericPhases_mod, only: MAPL_GENERIC_INIT_SET_CLOCK       => GENERIC_INIT_SET_CLOCK
   use mapl_GenericPhases_mod, only: MAPL_GENERIC_INIT_GEOM_A          => GENERIC_INIT_GEOM_A
   use mapl_GenericPhases_mod, only: MAPL_GENERIC_INIT_GEOM_B          => GENERIC_INIT_GEOM_B
   use mapl_GenericPhases_mod, only: MAPL_GENERIC_INIT_ADVERTISE       => GENERIC_INIT_ADVERTISE
   use mapl_GenericPhases_mod, only: MAPL_GENERIC_INIT_MODIFY_ADVERTISED => GENERIC_INIT_MODIFY_ADVERTISED
   use mapl_GenericPhases_mod, only: MAPL_GENERIC_INIT_REALIZE         => GENERIC_INIT_REALIZE
   use mapl_GenericPhases_mod, only: MAPL_GENERIC_INIT_READ_RESTART    => GENERIC_INIT_READ_RESTART
   use mapl_GenericPhases_mod, only: MAPL_GENERIC_INIT_USER            => GENERIC_INIT_USER
   use mapl_GenericPhases_mod, only: MAPL_GENERIC_RUN_OFFSET           => GENERIC_RUN_OFFSET
   use mapl_GenericPhases_mod, only: MAPL_GENERIC_RUN_CLOCK_ADVANCE    => GENERIC_RUN_CLOCK_ADVANCE
   use mapl_GenericPhases_mod, only: MAPL_GENERIC_RUN_USER             => GENERIC_RUN_USER
   use mapl_GenericPhases_mod, only: MAPL_GENERIC_FINALIZE_USER        => GENERIC_FINALIZE_USER

   ! CouplerPhases
   use mapl_CouplerPhases_mod, only: MAPL_GENERIC_COUPLER_INITIALIZE    => GENERIC_COUPLER_INITIALIZE
   use mapl_CouplerPhases_mod, only: MAPL_GENERIC_COUPLER_UPDATE        => GENERIC_COUPLER_UPDATE
   use mapl_CouplerPhases_mod, only: MAPL_GENERIC_COUPLER_INVALIDATE    => GENERIC_COUPLER_INVALIDATE
   use mapl_CouplerPhases_mod, only: MAPL_GENERIC_COUPLER_CLOCK_ADVANCE => GENERIC_COUPLER_CLOCK_ADVANCE

   ! StateItemAllocation
   use mapl_StateItemAllocation_mod, only: MAPL_StateItemAllocation              => StateItemAllocation
   use mapl_StateItemAllocation_mod, only: MAPL_STATEITEM_ALLOCATION_INVALID   => STATEITEM_ALLOCATION_INVALID
   use mapl_StateItemAllocation_mod, only: MAPL_STATEITEM_ALLOCATION_CREATED   => STATEITEM_ALLOCATION_CREATED
   use mapl_StateItemAllocation_mod, only: MAPL_STATEITEM_ALLOCATION_INACTIVE  => STATEITEM_ALLOCATION_INACTIVE
   use mapl_StateItemAllocation_mod, only: MAPL_STATEITEM_ALLOCATION_ACTIVE    => STATEITEM_ALLOCATION_ACTIVE
   use mapl_StateItemAllocation_mod, only: MAPL_STATEITEM_ALLOCATION_CONNECTED => STATEITEM_ALLOCATION_CONNECTED
   use mapl_StateItemAllocation_mod, only: MAPL_STATEITEM_ALLOCATION_ALLOCATED => STATEITEM_ALLOCATION_ALLOCATED
   use mapl_StateItemAllocation_mod, only: operator(==), operator(/=), operator(<), operator(>=)

   ! FieldBundleType_Flag
   use mapl_FieldBundleType_Flag_mod, only: MAPL_FieldBundleType_Flag               => FieldBundleType_Flag
   use mapl_FieldBundleType_Flag_mod, only: MAPL_FIELDBUNDLETYPE_BASIC              => FIELDBUNDLETYPE_BASIC
   use mapl_FieldBundleType_Flag_mod, only: MAPL_FIELDBUNDLETYPE_VECTOR             => FIELDBUNDLETYPE_VECTOR
   use mapl_FieldBundleType_Flag_mod, only: MAPL_FIELDBUNDLETYPE_BRACKET            => FIELDBUNDLETYPE_BRACKET
   use mapl_FieldBundleType_Flag_mod, only: MAPL_FIELDBUNDLETYPE_VECTORBRACKET      => FIELDBUNDLETYPE_VECTORBRACKET
   use mapl_FieldBundleType_Flag_mod, only: MAPL_FIELDBUNDLETYPE_SERVICE            => FIELDBUNDLETYPE_SERVICE
   use mapl_FieldBundleType_Flag_mod, only: MAPL_FIELDBUNDLETYPE_SERVICE_AGGREGATE  => FIELDBUNDLETYPE_SERVICE_AGGREGATE
   use mapl_FieldBundleType_Flag_mod, only: MAPL_FIELDBUNDLETYPE_SERVICE_SEPARATE   => FIELDBUNDLETYPE_SERVICE_SEPARATE
   use mapl_FieldBundleType_Flag_mod, only: MAPL_FIELDBUNDLETYPE_INVALID            => FIELDBUNDLETYPE_INVALID
   use mapl_FieldBundleType_Flag_mod, only: operator(==), operator(/=)

   ! Vertical enums
   ! VerticalStaggerLoc
   ! TODO: pchakrab - remove aliases once GEOS is updated to use the prefixed enums
   use mapl_VerticalStaggerLoc_mod, only: MAPL_VerticalStaggerLoc       => VerticalStaggerLoc
   use mapl_VerticalStaggerLoc_mod, only: MAPL_VERTICAL_STAGGER_NONE    => VERTICAL_STAGGER_NONE
   use mapl_VerticalStaggerLoc_mod, only: MAPL_VERTICAL_STAGGER_EDGE    => VERTICAL_STAGGER_EDGE
   use mapl_VerticalStaggerLoc_mod, only: MAPL_VERTICAL_STAGGER_CENTER  => VERTICAL_STAGGER_CENTER
   use mapl_VerticalStaggerLoc_mod, only: MAPL_VERTICAL_STAGGER_MIRROR  => VERTICAL_STAGGER_MIRROR
   use mapl_VerticalStaggerLoc_mod, only: MAPL_VERTICAL_STAGGER_INVALID => VERTICAL_STAGGER_INVALID

   ! VerticalAlignment
   use mapl_VerticalAlignment_mod, only: MAPL_VerticalAlignment => VerticalAlignment
   use mapl_VerticalAlignment_mod, only: MAPL_VALIGN_WITH_GRID => VALIGN_WITH_GRID
   use mapl_VerticalAlignment_mod, only: MAPL_VALIGN_UP => VALIGN_UP
   use mapl_VerticalAlignment_mod, only: MAPL_VALIGN_DOWN => VALIGN_DOWN
   use mapl_VerticalAlignment_mod, only: MAPL_VALIGN_INVALID => VALIGN_INVALID

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
   ! TODO: pchakrab - remove aliases once GEOS is updated to use the prefixed enums
   public :: MAPL_VerticalStaggerLoc
   public :: MAPL_VERTICAL_STAGGER_NONE
   public :: MAPL_VERTICAL_STAGGER_EDGE
   public :: MAPL_VERTICAL_STAGGER_CENTER
   public :: MAPL_VERTICAL_STAGGER_MIRROR
   public :: MAPL_VERTICAL_STAGGER_INVALID

   ! Vertical alignment
   public :: MAPL_VerticalAlignment
   public :: MAPL_VALIGN_WITH_GRID
   public :: MAPL_VALIGN_UP
   public :: MAPL_VALIGN_DOWN
   public :: MAPL_VALIGN_INVALID

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
