esma_set_this (OVERRIDE MAPL.esmf)

# ESMF infrastructure layer (Tier 3 in MAPL v3 restructuring).
# Consolidates: esmf_utils/, vm/, alarm/, hconfig/, hconfig_utils/,
# and ESMF-related files from generic3g/.

set (srcs
    # From esmf_utils/
    ESMF_Interfaces.F90
    ESMF_Utilities.F90
    InfoUtilities.F90
    UngriddedDim.F90
    UngriddedDims.F90
    UngriddedDimVector.F90
    LU_Bound.F90
    ESMF_Time_Utilities.F90
    HorizontalDimsSpec.F90
    DistGridGet.F90
    FieldPointerUtilities.F90
    # From alarm/
    alarm/SimpleAlarm.F90
    # From hconfig/
    hconfig/HConfigAs.F90
    # From hconfig_utils/
    hconfig_utils/HConfig3G.F90
    hconfig_utils/hconfig_get.F90
    hconfig_utils/hconfig_params.F90
    hconfig_utils/hconfig_get_private.F90
    hconfig_utils/generalized_equality.F90
    hconfig_utils/get_hconfig.F90
    hconfig_utils/HConfigUtilities.F90
    # From generic3g/
    ESMF_HConfigUtilities.F90
    ESMF_HConfigUtilities/MAPL_HConfigMatch.F90
    ESMF_HConfigUtilities/write_hconfig.F90
    StateItem.F90
    # From esmf_utils/comms/
    comms/Comms.F90
    comms/ShmemComms.F90
    # Umbrella module
    API.F90
)

esma_add_library (${this}
    SRCS ${srcs}
    DEPENDENCIES MAPL.enums MAPL.utils MAPL.mp_utils ESMF::ESMF GFTL::gftl-v2 PFLOGGER::pflogger MPI::MPI_Fortran
    TYPE SHARED
)

target_include_directories (${this} PUBLIC
    $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/hconfig_utils>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/comms>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
)

set_target_properties (${this} PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})

if(ESMF_HCONFIGSET_HAS_INTENT_INOUT)
    target_compile_definitions (${this} PRIVATE ESMF_HCONFIGSET_HAS_INTENT_INOUT)
endif()

if (PFUNIT_FOUND)
    add_subdirectory (tests EXCLUDE_FROM_ALL)
    add_subdirectory (alarm/tests EXCLUDE_FROM_ALL)
    add_subdirectory (hconfig/tests EXCLUDE_FROM_ALL)
    add_subdirectory (hconfig_utils/tests EXCLUDE_FROM_ALL)
endif ()
