module  mapl_fortran_python_bridge
    
    ! - - - - - - - - - - - - - - -!
    !
    ! Fortan interface forwarding into an iso_c_binding worth C code defined in bridge.C
    !
    ! - - - - - - - - - - - - - - -!
    
    use ESMF
    use iso_c_binding, only: c_int, c_ptr

    implicit none

    character(len=ESMF_MAXSTR), TARGET :: PYGEOSBRIDGE_name_BUFFER

    private
    public :: mapl_fortran_python_bridge_global_initialize
    
    public :: mapl_fortran_python_bridge_user_init
    public :: mapl_fortran_python_bridge_user_run
    public :: mapl_fortran_python_bridge_user_run_with_internal
    public :: mapl_fortran_python_bridge_user_finalize

    public :: PYGEOSBRIDGE_name_BUFFER

    interface

        ! - - - Global subroutines - setup/teardown python - - - !

        subroutine mapl_fortran_python_bridge_global_initialize( im, jm, lm) bind(c, name='mapl_fortran_python_bridge_global_initialize')
            import c_int
            implicit none
            integer(kind=c_int), intent(in), value :: im, jm, lm
        end subroutine mapl_fortran_python_bridge_global_initialize

        ! - - - User interface to reach user code keyed by name - - - !

        subroutine mapl_fortran_python_bridge_user_init(name, mapl_state, import, export) bind(c, name='mapl_fortran_python_bridge_user_init')
            import c_ptr
            implicit none
            type(c_ptr), intent(in), value :: name, mapl_state, import, export
        end subroutine mapl_fortran_python_bridge_user_init

        subroutine mapl_fortran_python_bridge_user_run(name, mapl_state, import, export) bind(c, name='mapl_fortran_python_bridge_user_run')
            import c_ptr
            implicit none
            type(c_ptr), intent(in), value :: name, mapl_state, import, export
        end subroutine mapl_fortran_python_bridge_user_run

        subroutine mapl_fortran_python_bridge_user_run_with_internal(name, mapl_state, import, export, INTERNAL) bind(c, name='mapl_fortran_python_bridge_user_run_with_internal')
            import c_ptr
            implicit none
            type(c_ptr), intent(in), value :: name, mapl_state, import, export, INTERNAL
        end subroutine mapl_fortran_python_bridge_user_run_with_internal

        subroutine mapl_fortran_python_bridge_user_finalize(name, mapl_state, import, export) bind(c, name='mapl_fortran_python_bridge_user_finalize')
            import c_ptr
            implicit none
            type(c_ptr), intent(in), value :: name, mapl_state, import, export
        end subroutine mapl_fortran_python_bridge_user_finalize

    end interface

end module mapl_fortran_python_bridge
