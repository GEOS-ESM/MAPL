module GEOS_PythonBridge_PyInterfaceMod
    use ESMF
    use iso_c_binding, only: c_int, c_ptr

    implicit none

    character(len=ESMF_MAXSTR), TARGET :: PYGEOSBRIDGE_NAME_BUFFER

    private
    public :: MAPL_PythonBridge_C_global_initialize
    
    public :: pyGEOSBridge_C_init
    public :: pyGEOSBridge_C_run
    public :: pyGEOSBridge_C_finalize

    public :: PYGEOSBRIDGE_NAME_BUFFER

    interface

        ! - - - Global subroutines - setup/teardown python - - - !

        subroutine MAPL_PythonBridge_C_global_initialize( IM, JM, LM) bind(c, name='MAPL_PythonBridge_C_global_initialize')
            import c_int
            implicit none
            integer(kind=c_int), intent(in), value :: IM, JM, LM
        end subroutine MAPL_PythonBridge_C_global_initialize

        ! - - - Generic Interfacing keyed by name - - - !

        subroutine pyGEOSBridge_C_init(NAME, MAPL_STATE, IMPORT, EXPORT) bind(c, name='pyGEOSBridge_C_init')
            import c_ptr
            implicit none
            type(c_ptr), intent(in), value :: NAME, MAPL_STATE, IMPORT, EXPORT
        end subroutine pyGEOSBridge_C_init

        subroutine pyGEOSBridge_C_run(NAME, MAPL_STATE, IMPORT, EXPORT) bind(c, name='pyGEOSBridge_C_run')
            import c_ptr
            implicit none
            type(c_ptr), intent(in), value :: NAME, MAPL_STATE, IMPORT, EXPORT
        end subroutine pyGEOSBridge_C_run

        subroutine pyGEOSBridge_C_finalize(NAME, MAPL_STATE, IMPORT, EXPORT) bind(c, name='pyGEOSBridge_C_finalize')
            import c_ptr
            implicit none
            type(c_ptr), intent(in), value :: NAME, MAPL_STATE, IMPORT, EXPORT
        end subroutine pyGEOSBridge_C_finalize

    end interface

end module GEOS_PythonBridge_PyInterfaceMod
