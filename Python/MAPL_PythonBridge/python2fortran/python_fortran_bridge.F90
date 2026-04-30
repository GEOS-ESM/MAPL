#include "MAPL.h"

module mapl_python_fortran_bridge

    use ESMF
    use MAPL
    use iso_c_binding

    implicit NONE

    private

    character(len=ESMF_MAXSTR) :: IAm
    integer :: STATUS
    integer :: RC  ! return code
    character(len=ESMF_MAXSTR), TARGET :: F90_STRING_BUFFER

    CONTAINS

    function MAPLPy_ESMF_AttributeGet_1D_int(state_c_ptr, name_c_ptr, name_len) result(return_value) bind(c, name="MAPLPy_ESMF_AttributeGet_1D_int")
        ! Read in STATE
        type(c_ptr), intent(in), value :: state_c_ptr
        type(ESMF_State), pointer :: state

        ! Read in name
        type(c_ptr), intent(in), value :: name_c_ptr
        integer(c_int), intent(in), value :: name_len
        character(len=name_len,kind=c_char), pointer :: varname

        ! Return value
        integer :: return_value
        type(ESMF_Info) :: info

        ! Turn the C string into a Fortran string
        call c_f_pointer(name_c_ptr, varname)

        ! Turn the ESMF State C pointer to a Fortran pointer
        call c_f_pointer(state_c_ptr, state)

        ! Call function
        call ESMF_InfoGetFromHost(state, info, _RC)
        call ESMF_InfoGet(info, key=varname, value=return_value, _RC)

    end function MAPLPy_ESMF_AttributeGet_1D_int

    subroutine MAPLPy_ESMF_MethodExecute(state_c_ptr, label_c_ptr, label_len) bind(c, name="MAPLPy_ESMF_MethodExecute")
        ! Read in STATE
        type(c_ptr), intent(in), value :: state_c_ptr
        type(ESMF_State), pointer :: state

        ! Read in name
        type(c_ptr), intent(in), value :: label_c_ptr
        integer(c_int), intent(in), value :: label_len
        character(len=label_len,kind=c_char), pointer :: label

        ! Turn the C string into a Fortran string
        call c_f_pointer(label_c_ptr, label)

        ! Turn the ESMF State C pointer to a Fortran pointer
        call c_f_pointer(state_c_ptr, state)

        call ESMF_MethodExecute(state, label=label, _RC)

    end subroutine MAPLPy_ESMF_MethodExecute

    function MAPLpy_GetPointer_via_ESMFAttr(state_c_ptr, name_c_ptr, name_len) result(c_data_ptr) bind(c, name="MAPLpy_GetPointer_via_ESMFAttr")
        ! Read in STATE
        type(c_ptr), intent(in), value :: state_c_ptr
        type(ESMF_State), pointer :: state

        ! Read in name
        type(c_ptr), intent(in), value :: name_c_ptr
        integer(c_int), intent(in), value :: name_len
        character(len=name_len,kind=c_char), pointer :: name

        ! Results
        character(len=ESMF_MAXSTR) :: field_name_from_esmf
        real, pointer, dimension(:,:,:) :: f_ptr
        type(c_ptr) :: c_data_ptr
        type(ESMF_Info) :: info

        ! Turn the C string into a Fortran string
        call c_f_pointer(name_c_ptr, name)

        ! Turn the ESMF State C pointer to a Fortran pointer
        call c_f_pointer(state_c_ptr, state)

        call ESMF_InfoGetFromHost(state, info, _RC)
        call ESMF_InfoGet(info, key=name, value=field_name_from_esmf, _RC)
        call MAPL_StateGetPointer(state, f_ptr, trim(field_name_from_esmf), _RC)
        c_data_ptr=c_loc(f_ptr)

    end function

    function MAPLpy_GetPointer_2D(state_c_ptr, name_c_ptr, name_len, alloc) result(c_data_ptr) bind(c, name="MAPLpy_GetPointer_2D")
        ! Read in STATE
        type(c_ptr), intent(in), value :: state_c_ptr
        type(ESMF_State), pointer :: state

        ! Read in name
        type(c_ptr), intent(in), value :: name_c_ptr
        integer(c_int), intent(in), value :: name_len
        character(len=name_len,kind=c_char), pointer :: name
        logical(c_bool), intent(in), value :: alloc

        ! Results
        real, pointer, dimension(:,:) :: f_ptr
        type(c_ptr) :: c_data_ptr

        ! Turn the C string into a Fortran string
        call c_f_pointer(name_c_ptr, name)

        ! Turn the ESMF State C pointer to a Fortran pointer
        call c_f_pointer(state_c_ptr, state)

        call MAPL_StateGetPointer(state, f_ptr, trim(name), alloc=logical(alloc), _RC)
        c_data_ptr=c_loc(f_ptr)

    end function

    function MAPLpy_GetPointer_2D_associated(state_c_ptr, name_c_ptr, name_len, alloc) result(is_associated_as_integer) bind(c, name="MAPLpy_GetPointer_2D_associated")
        ! We retrieve the associated state via an Int to circumvent disagreement in ICX about
        ! the definition fo a Fortran/C boolean. Integer remains the most portable solution.

        ! Read in STATE
        type(c_ptr), intent(in), value :: state_c_ptr
        type(ESMF_State), pointer :: state

        ! Read in name
        type(c_ptr), intent(in), value :: name_c_ptr
        integer(c_int), intent(in), value :: name_len
        character(len=name_len,kind=c_char), pointer :: name
        logical(c_bool), intent(in), value :: alloc

        ! Results
        real, pointer, dimension(:,:) :: f_ptr
        integer(kind=c_int):: is_associated_as_integer

        ! Turn the C string into a Fortran string
        call c_f_pointer(name_c_ptr, name)

        ! Turn the ESMF State C pointer to a Fortran pointer
        call c_f_pointer(state_c_ptr, state)

        call MAPL_StateGetPointer(state, f_ptr, trim(name), alloc=logical(alloc), _RC)
        is_associated_as_integer = merge(1, 0, associated(f_ptr))

    end function

    function MAPLpy_GetPointer_3D(state_c_ptr, name_c_ptr, name_len, alloc) result(c_data_ptr) bind(c, name="MAPLpy_GetPointer_3D")
        ! Read in STATE
        type(c_ptr), intent(in), value :: state_c_ptr
        type(ESMF_State), pointer :: state

        ! Read in name
        type(c_ptr), intent(in), value :: name_c_ptr
        integer(c_int), intent(in), value :: name_len
        character(len=name_len,kind=c_char), pointer :: name
        logical(c_bool), intent(in), value :: alloc

        ! Results
        real, pointer, dimension(:,:,:) :: f_ptr
        type(c_ptr) :: c_data_ptr

        ! Turn the C string into a Fortran string
        call c_f_pointer(name_c_ptr, name)

        ! Turn the ESMF State C pointer to a Fortran pointer
        call c_f_pointer(state_c_ptr, state)

        call MAPL_StateGetPointer(state, f_ptr, trim(name), alloc=logical(alloc), _RC)

        c_data_ptr=c_loc(f_ptr)

    end function

    function MAPLpy_GetPointer_associated(state_c_ptr, name_c_ptr, name_len, alloc) result(is_associated_as_integer) bind(c, name="MAPLpy_GetPointer_3D_associated")
        ! We retrieve the associated state via an Int to circumvent disagreement in ICX about
        ! the definition fo a Fortran/C boolean. Integer remains the most portable solution.
        ! Read in STATE
        type(c_ptr), intent(in), value :: state_c_ptr
        type(ESMF_State), pointer :: state

        ! Read in name
        type(c_ptr), intent(in), value :: name_c_ptr
        integer(c_int), intent(in), value :: name_len
        character(len=name_len,kind=c_char), pointer :: name
        logical, intent(in), value :: alloc

        ! Results
        real, pointer, dimension(:,:,:) :: f_ptr
        integer(kind=c_int):: is_associated_as_integer

        ! Turn the C string into a Fortran string
        call c_f_pointer(name_c_ptr, name)

        ! Turn the ESMF State C pointer to a Fortran pointer
        call c_f_pointer(state_c_ptr, state)

        call MAPL_StateGetPointer(state, f_ptr, trim(name), alloc=logical(alloc), _RC)
        is_associated_as_integer = merge(1, 0, associated(f_ptr))

    end function

    function MAPLpy_GetResource_Float(gridcomp_c_ptr, name_c_ptr, name_len, default) result(result) bind(c, name="MAPLpy_GetResource_Float")
        ! Read in GridComp
        type(c_ptr), intent(in), value :: gridcomp_c_ptr
        type(ESMF_GridComp), pointer :: gridcomp

        ! Read in name
        type(c_ptr), intent(in), value :: name_c_ptr
        integer(c_int), intent(in), value :: name_len
        character(len=name_len,kind=c_char), pointer :: name
        real(c_float), intent(in), value :: default

        ! Results & local
        real(c_float) :: result
        real(kind=4) :: local_r, local_d

        ! Make pointer & string fortran from C
        call c_f_pointer(name_c_ptr, name)
        call c_f_pointer(gridcomp_c_ptr, gridcomp)

        ! Use fortran type & cast back to C types
        local_d = default
        call MAPL_GridCompGetResource(gridcomp, trim(name), local_r, default=local_d, _RC)
        result = local_r
    end function

    function MAPLpy_GetResource_Bool(gridcomp_c_ptr, name_c_ptr, name_len, default) result(result) bind(c, name="MAPLpy_GetResource_Bool")
        ! Read in GridComp
        type(c_ptr), intent(in), value :: gridcomp_c_ptr
        type(ESMF_GridComp), pointer :: gridcomp

        ! Read in name
        type(c_ptr), intent(in), value :: name_c_ptr
        integer(c_int), intent(in), value :: name_len
        character(len=name_len,kind=c_char), pointer :: name
        logical(c_bool), intent(in), value :: default

        ! Results
        logical(c_bool) :: result
        logical :: local_r, local_d

        ! Turn the C string into a Fortran string
        call c_f_pointer(name_c_ptr, name)

        ! Turn the GridComp C pointer to a Fortran pointer
        call c_f_pointer(gridcomp_c_ptr, gridcomp)

        local_d = default
        call MAPL_GridCompGetResource(gridcomp, trim(name), local_r, default=local_d, _RC)
        result = local_r

    end function

    function MAPLpy_GetResource_Int32(gridcomp_c_ptr, name_c_ptr, name_len, default) result(result) bind(c, name="MAPLpy_GetResource_Int32")
        ! Read in GridComp
        type(c_ptr), intent(in), value :: gridcomp_c_ptr
        type(ESMF_GridComp), pointer :: gridcomp

        ! Read in name
        type(c_ptr), intent(in), value :: name_c_ptr
        integer(c_int), intent(in), value :: name_len
        character(len=name_len,kind=c_char), pointer :: name
        integer(C_INT32_T), intent(in), value :: default

        ! Results
        integer(C_INT32_T) :: result
        integer(kind=4) :: local_r, local_d

        ! Make pointer & string fortran from C
        call c_f_pointer(name_c_ptr, name)
        call c_f_pointer(gridcomp_c_ptr, gridcomp)

        local_d = default
        call MAPL_GridCompGetResource(gridcomp, trim(name), local_r, default=local_d, _RC)
        result = local_r

    end function

    ! function MAPLpy_GetResource_Int64(state_c_ptr, name_c_ptr, name_len, default) result(result) bind(c, name="MAPLpy_GetResource_Int64")
    !     ! Read in STATE
    !     type(c_ptr), intent(in), value :: state_c_ptr
    !     type(ESMF_State), pointer :: state

    !     ! Read in name
    !     type(c_ptr), intent(in), value :: name_c_ptr
    !     integer(c_int), intent(in), value :: name_len
    !     character(len=name_len,kind=c_char), pointer :: name
    !     logical(c_bool), intent(in), value :: default

    !     ! Results
    !     integer(C_INT64_T) :: result

    !     ! Turn the C string into a Fortran string
    !     call c_f_pointer(name_c_ptr, name)

    !     ! Turn the ESMF State C pointer to a Fortran pointer
    !     call c_f_pointer(state_c_ptr, state)

    !     call MAPL_GetResource(state, result, label=trim(name), default=logical(default))

    ! end function

    ! function MAPLpy_GetResource_Double(state_c_ptr, name_c_ptr, name_len, default) result(result) bind(c, name="MAPLpy_GetResource_Double")
    !     ! Read in STATE
    !     type(c_ptr), intent(in), value :: state_c_ptr
    !     type(ESMF_State), pointer :: state

    !     ! Read in name
    !     type(c_ptr), intent(in), value :: name_c_ptr
    !     integer(c_int), intent(in), value :: name_len
    !     character(len=name_len,kind=c_char), pointer :: name
    !     logical(c_bool), intent(in), value :: default

    !     ! Results
    !     real(c_double) :: result

    !     ! Turn the C string into a Fortran string
    !     call c_f_pointer(name_c_ptr, name)

    !     ! Turn the ESMF State C pointer to a Fortran pointer
    !     call c_f_pointer(state_c_ptr, state)

    !     call MAPL_GetResource(state, result, label=trim(name), default=logical(default))

    ! end function

    function MAPLpy_ESMF_TimeIntervalGet(time_interval_c_ptr) result(result) bind(c, name="MAPLpy_ESMF_TimeIntervalGet")
        ! Read in TimeInterval
        type(c_ptr), intent(in), value :: time_interval_c_ptr
        type(ESMF_TimeInterval), pointer :: time_interval

        ! Results
        real(c_double) :: result

        ! Turn the C pointer to a Fortran pointer
        call c_f_pointer(time_interval_c_ptr, time_interval)

        call ESMF_TimeIntervalGet(time_interval, S_R8=result)

    end function MAPLpy_ESMF_TimeIntervalGet

    function MAPLpy_ESMF_GridCompGetName(c_grid_comp) result(c_string) bind(c, name="MAPLpy_ESMF_GridCompGetName")

        type(c_ptr), intent(in), value     :: c_grid_comp

        type(ESMF_GridComp), pointer       :: gridcomp
        type(c_ptr)                        :: c_string

        call c_f_pointer(c_grid_comp, gridcomp)

        call ESMF_GridCompGet(gridcomp, name=F90_STRING_BUFFER, _RC)
        c_string = c_loc(F90_STRING_BUFFER)

    end function MAPLpy_ESMF_GridCompGetName

    function MAPLPy_MAPL_GetIM(c_mapl_state) result(result) bind(c, name="MAPLPy_MAPL_GetIM")
        ! Read in GridComp
        type(c_ptr), intent(in), value :: c_gridcomp
        type(ESMF_GridComp), pointer :: gridcomp

        ! Results
        integer(C_INT32_T) :: result

        ! Make pointer from C
        call c_f_pointer(c_gridcomp, gridcomp)

        _FAIL("MAPL_Get IM not available in MAPL3 - port python bridge to use geom API")

    end function

    function MAPLPy_MAPL_GetJM(c_mapl_state) result(result) bind(c, name="MAPLPy_MAPL_GetJM")
        ! Read in GridComp
        type(c_ptr), intent(in), value :: c_mapl_state
        type(ESMF_GridComp), pointer :: gridcomp

        ! Results
        integer(C_INT32_T) :: result

        ! Make pointer from C
        call c_f_pointer(c_mapl_state, gridcomp)

        _FAIL("MAPL_Get JM not available in MAPL3 - port python bridge to use geom API")

    end function

    function MAPLPy_MAPL_GetLM(c_mapl_state) result(result) bind(c, name="MAPLPy_MAPL_GetLM")
        ! Read in GridComp
        type(c_ptr), intent(in), value :: c_mapl_state
        type(ESMF_GridComp), pointer :: gridcomp

        ! Results
        integer(C_INT32_T) :: result
        integer(kind=4) :: local_r

        ! Make pointer from C
        call c_f_pointer(c_mapl_state, gridcomp)

        call MAPL_GridCompGet(gridcomp, num_levels=local_r, _RC)
        result = local_r

    end function

    function MAPLPy_MAPL_GetNX(c_mapl_state) result(result) bind(c, name="MAPLPy_MAPL_GetNX")
        ! Read in GridComp
        type(c_ptr), intent(in), value :: c_mapl_state
        type(ESMF_GridComp), pointer :: gridcomp

        ! Results
        integer(C_INT32_T) :: result

        ! Make pointer from C
        call c_f_pointer(c_mapl_state, gridcomp)

        _FAIL("MAPL_Get NX not available in MAPL3 - port python bridge to use geom API")

    end function

    function MAPLPy_MAPL_GetNY(c_mapl_state) result(result) bind(c, name="MAPLPy_MAPL_GetNY")
        ! Read in GridComp
        type(c_ptr), intent(in), value :: c_mapl_state
        type(ESMF_GridComp), pointer :: gridcomp

        ! Results
        integer(C_INT32_T) :: result

        ! Make pointer from C
        call c_f_pointer(c_mapl_state, gridcomp)

        _FAIL("MAPL_Get NY not available in MAPL3 - port python bridge to use geom API")

    end function

end module mapl_python_fortran_bridge
