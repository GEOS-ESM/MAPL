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
    EsmfUtils_API.F90
    LU_Bound.F90
    ESMF_Time_Utilities.F90
    HorizontalDimsSpec.F90
    DistGridGet.F90
    FieldPointerUtilities.F90
    # From vm/
    API.F90
    vm.F90
    # From alarm/
    alarm/SimpleAlarm.F90
    # From hconfig/
    hconfig/API.F90
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
    ESMF_Subset.F90
    ESMF_HConfigUtilities.F90
    ESMF_HConfigUtilities/MAPL_HConfigMatch.F90
    ESMF_HConfigUtilities/write_hconfig.F90
    StateItem.F90
    # From esmf_utils/comms/
    comms/MAPL_Comms.F90
    comms/API.F90
)

esma_add_library (${this}
    SRCS ${srcs}
    DEPENDENCIES MAPL.enums MAPL.shared ESMF::ESMF GFTL::gftl-v2 PFLOGGER::pflogger
    TYPE SHARED
)

target_include_directories (${this} PUBLIC
    $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/hconfig_utils>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/comms>
)

set_target_properties (${this} PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})

if(ESMF_HCONFIGSET_HAS_INTENT_INOUT)
    target_compile_definitions (${this} PRIVATE ESMF_HCONFIGSET_HAS_INTENT_INOUT)
endif()

# Backward-compatibility aliases
foreach (alias MAPL.esmf_utils MAPL.vm MAPL.alarm MAPL.hconfig MAPL.hconfig_utils)
    add_library (${alias} INTERFACE)
    target_link_libraries (${alias} INTERFACE MAPL.esmf)
    install (TARGETS ${alias} EXPORT MAPL-targets)
endforeach ()

if (PFUNIT_FOUND)
    add_subdirectory (tests)
    add_subdirectory (alarm/tests)
    add_subdirectory (hconfig/tests)
    add_subdirectory (hconfig_utils/tests)
endif ()
