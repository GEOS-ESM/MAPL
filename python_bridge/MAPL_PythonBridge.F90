#include "MAPL_Generic.h"

module MAPL_PythonBridge
    ! -----------------------------
    ! Generic bridge mechanism to pull MAPL down to the python level
    !
    ! WARNING: All functional code should be #ifdef with PYTHONBRIDGE_INTEGRATION
    !          in order to keep the functionality as a deliberate opt-in
    !
    ! -----------------------------
    
    use ESMF
    use MAPL_BaseMod
    use MAPL_GenericMod
    use MaplShared
#ifdef PYTHONBRIDGE_INTEGRATION
    use GEOS_PythonBridge_PyInterfaceMod
    use ieee_exceptions, only: ieee_get_halting_mode, ieee_set_halting_mode, ieee_overflow, ieee_support_halting
    use iso_c_binding, only: c_loc, C_NULL_CHAR
#endif
    implicit none
    private

    public initialize_python_bridge
    public MAPL_pybridge_gcrun
    public MAPL_pybridge_gcrun_with_internal
    public MAPL_pybridge_gcinit
    public MAPL_pybridge_gcfinalize

contains

    subroutine initialize_python_bridge ( IM, JM, LM )
        ! -----------------------------
        ! Initialize the underlying python tools with a grid size.
        ! It will spin a central CFFI-handled python interpreter
        ! It also gives us a hook to do pre-load of common packages (numpy, tf, etc.)
        !
        ! TODO | Dev Note : this works for a single GRID. A much powerful system
        !                   would not get the dimensions upfront but have them part
        !                   of the backward fetch from python to MAPL
        ! -----------------------------

        integer, intent(in), value :: IM, JM, LM
        character(len=ESMF_MAXSTR) :: IAm
        integer                    :: STATUS
        integer                    :: RC

#ifdef PYTHONBRIDGE_INTEGRATION
        logical                    :: halting_mode(5)
        logical                    :: set_halting_allowed

        ! Spin the interface - we have to deactivate the ieee error
        ! to be able to load numpy, scipy and other numpy packages
        ! that generate an overflow during init

        set_halting_allowed = ieee_support_halting(ieee_overflow)

        if (set_halting_allowed) then
            call ieee_get_halting_mode(ieee_overflow, halting_mode)
            call ieee_set_halting_mode(ieee_overflow, .false.)
        endif
        call MAPL_PythonBridge_C_global_initialize(IM, JM, LM)
        if (set_halting_allowed) then
            call ieee_set_halting_mode(ieee_overflow, halting_mode)
        endif
#endif

    end subroutine initialize_python_bridge

    subroutine MAPL_pybridge_gcinit(PYPKG_NAME, MAPL, IMPORT, EXPORT)
        ! -----------------------------
        ! Call the python integration by marhshalling Fortran object/memory
        ! into C compatible memory
        ! Will trigger the `GEOSInterfaceCode.init` python function on the user code
        ! -----------------------------
        type(MAPL_MetaComp), intent(inout), TARGET :: MAPL   ! MAPL state
        type(ESMF_State),    intent(inout), TARGET :: IMPORT ! Import state
        type(ESMF_State),    intent(inout), TARGET :: EXPORT ! Export state

        character(len=*),    intent(in)            :: PYPKG_NAME
    
#ifdef PYTHONBRIDGE_INTEGRATION
        PYGEOSBRIDGE_NAME_BUFFER = PYPKG_NAME // C_NULL_CHAR
        call pyGEOSBridge_C_init( c_loc(PYGEOSBRIDGE_NAME_BUFFER), c_loc(MAPL), c_loc(IMPORT), c_loc(EXPORT) )
#endif
    end subroutine

    subroutine MAPL_pybridge_gcrun(PYPKG_NAME, MAPL, IMPORT, EXPORT)
        ! -----------------------------
        ! Call the python integration by marhshalling Fortran object/memory
        ! into C compatible memory
        ! Will trigger the `GEOSInterfaceCode.run` python function on the user code
        ! -----------------------------
        type(MAPL_MetaComp), intent(inout), TARGET  :: MAPL   ! MAPL state
        type(ESMF_State),    intent(inout), TARGET :: IMPORT ! Import state
        type(ESMF_State),    intent(inout), TARGET :: EXPORT ! Export state

        character(len=*),    intent(in)            :: PYPKG_NAME
    
#ifdef PYTHONBRIDGE_INTEGRATION
        PYGEOSBRIDGE_NAME_BUFFER = PYPKG_NAME // C_NULL_CHAR
        call pyGEOSBridge_C_run( c_loc(PYGEOSBRIDGE_NAME_BUFFER), c_loc(MAPL), c_loc(IMPORT), c_loc(EXPORT) )
#endif
    end subroutine

    subroutine MAPL_pybridge_gcrun_with_internal(PYPKG_NAME, MAPL, IMPORT, EXPORT, INTERNAL)
        ! -----------------------------
        ! Call the python integration by marhshalling Fortran object/memory
        ! into C compatible memory. Variation of `gcrun` with an INTERNAL state
        ! Will trigger the `GEOSInterfaceCode.run_with_internal` python function on the user code
        ! -----------------------------
        type(MAPL_MetaComp), intent(inout), TARGET  :: MAPL   ! MAPL state
        type(ESMF_State),    intent(inout), TARGET :: IMPORT ! Import state
        type(ESMF_State),    intent(inout), TARGET :: EXPORT ! Export state
        type(ESMF_State),    intent(inout), TARGET :: INTERNAL ! Internal state

        character(len=*),    intent(in)            :: PYPKG_NAME
    
#ifdef PYTHONBRIDGE_INTEGRATION
        PYGEOSBRIDGE_NAME_BUFFER = PYPKG_NAME // C_NULL_CHAR
        call pyGEOSBridge_C_run_with_internal( c_loc(PYGEOSBRIDGE_NAME_BUFFER), c_loc(MAPL), c_loc(IMPORT), c_loc(EXPORT), c_loc(INTERNAL) )
#endif
    end subroutine

    subroutine MAPL_pybridge_gcfinalize(PYPKG_NAME, MAPL, IMPORT, EXPORT)
        ! -----------------------------
        ! Call the python integration by marhshalling Fortran object/memory
        ! into C compatible memory.
        ! Will trigger the `GEOSInterfaceCode.finalize` python function on the user code
        ! -----------------------------
        type(MAPL_MetaComp), intent(inout), TARGET  :: MAPL   ! MAPL state
        type(ESMF_State),    intent(inout), TARGET :: IMPORT ! Import state
        type(ESMF_State),    intent(inout), TARGET :: EXPORT ! Export state

        character(len=*),    intent(in)            :: PYPKG_NAME
    
#ifdef PYTHONBRIDGE_INTEGRATION
        PYGEOSBRIDGE_NAME_BUFFER = PYPKG_NAME // C_NULL_CHAR
        call pyGEOSBridge_C_finalize( c_loc(PYGEOSBRIDGE_NAME_BUFFER), c_loc(MAPL), c_loc(IMPORT), c_loc(EXPORT) )
#endif
    end subroutine
end MODULE MAPL_PythonBridge
