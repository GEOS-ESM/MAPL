#include "MAPL_Generic.h"
#include "MAPL_Exceptions.h"
#include "MAPL_ErrLogMain.h"

module C_BRIDGE_TO_MAPL
    
    use ESMFL_Mod
    use ESMF
    use MAPL_BaseMod
    use MAPL_GenericMod
    use MaplShared
    use iso_c_binding

    implicit NONE
    
    private
    public :: MAPLPy_ESMF_AttributeGet_1D_int
    public :: MAPLPy_ESMF_MethodExecute
    public :: MAPLpy_GetPointer_via_ESMFAttr
    public :: MAPLpy_GetPointer_2D
    public :: MAPLpy_GetPointer_3D
    public :: MAPLpy_GetResource_Bool
    public :: MAPLpy_GetResource_Float
    ! public :: MAPLpy_GetResource_Double ! No MAPL_GetResource for int 64?
    public :: MAPLpy_GetResource_Int32
    ! public :: MAPLpy_GetResource_Int64 ! No MAPL_GetResource for int 64?
    public :: MAPLpy_ESMF_TimeIntervalGet
    public :: MAPLpy_Associated

    character(len=ESMF_MAXSTR) :: IAm
    integer :: STATUS
    integer :: RC  ! return code
    character(len=ESMF_MAXSTR), TARGET :: F90_STRING_BUFFER

    CONTAINS

    function MAPLPy_ESMF_AttributeGet_1D_int(esmf_state_c_ptr, name_c_ptr, name_len) result(return_value) bind(c, name="MAPLPy_ESMF_AttributeGet_1D_int")
        ! Read in STATE
        type(c_ptr), intent(in), value :: esmf_state_c_ptr
        type(ESMF_State), pointer :: state

        ! Read in name
        type(c_ptr), intent(in), value :: name_c_ptr
        integer(c_int), intent(in), value :: name_len
        character(len=name_len,kind=c_char), pointer :: varname

        ! Return value
        integer :: return_value

        ! Turn the C string into a Fortran string
        call c_f_pointer(name_c_ptr, varname)

        ! Turn the ESMF State C pointer to a Fortran pointer
        call c_f_pointer(esmf_state_c_ptr, state)

        ! Call function
        call ESMF_AttributeGet(state, name=varname, value=return_value, RC=STATUS)
        VERIFY_(STATUS)
    
    end function MAPLPy_ESMF_AttributeGet_1D_int

    subroutine MAPLPy_ESMF_MethodExecute(esmf_state_c_ptr, label_c_ptr, label_len) bind(c, name="MAPLPy_ESMF_MethodExecute")
        ! Read in STATE
        type(c_ptr), intent(in), value :: esmf_state_c_ptr
        type(ESMF_State), pointer :: state

        ! Read in name
        type(c_ptr), intent(in), value :: label_c_ptr
        integer(c_int), intent(in), value :: label_len
        character(len=label_len,kind=c_char), pointer :: label

        ! Turn the C string into a Fortran string
        call c_f_pointer(label_c_ptr, label)

        ! Turn the ESMF State C pointer to a Fortran pointer
        call c_f_pointer(esmf_state_c_ptr, state)

        call ESMF_MethodExecute(state, label=label, RC=STATUS)
        VERIFY_(STATUS)

    end subroutine MAPLPy_ESMF_MethodExecute

    function MAPLpy_GetPointer_via_ESMFAttr(esmf_state_c_ptr, name_c_ptr, name_len) result(c_data_ptr) bind(c, name="MAPLpy_GetPointer_via_ESMFAttr")
        ! Read in STATE
        type(c_ptr), intent(in), value :: esmf_state_c_ptr
        type(ESMF_State), pointer :: state

        ! Read in name
        type(c_ptr), intent(in), value :: name_c_ptr
        integer(c_int), intent(in), value :: name_len
        character(len=name_len,kind=c_char), pointer :: name

        ! Results
        character(len=ESMF_MAXSTR) :: field_name_from_esmf
        real, pointer, dimension(:,:,:) :: f_ptr
        type(c_ptr) :: c_data_ptr

        ! Turn the C string into a Fortran string
        call c_f_pointer(name_c_ptr, name)

        ! Turn the ESMF State C pointer to a Fortran pointer
        call c_f_pointer(esmf_state_c_ptr, state)        

        call ESMF_AttributeGet(state, name=name, value=field_name_from_esmf, RC=STATUS)
        VERIFY_(STATUS)
        call MAPL_GetPointer(state, f_ptr, trim(field_name_from_esmf), RC=STATUS)
        VERIFY_(STATUS)
        c_data_ptr=c_loc(f_ptr)
    
    end function    

    function MAPLpy_GetPointer_2D(esmf_state_c_ptr, name_c_ptr, name_len, alloc) result(c_data_ptr) bind(c, name="MAPLpy_GetPointer_2D")
        ! Read in STATE
        type(c_ptr), intent(in), value :: esmf_state_c_ptr
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
        call c_f_pointer(esmf_state_c_ptr, state)        
        
        call MAPL_GetPointer(state, f_ptr, trim(name), alloc=logical(alloc), RC=STATUS)
        VERIFY_(STATUS)
        ! if (associated(f_ptr)) then
        c_data_ptr=c_loc(f_ptr)
        ! endif

    end function

    function MAPLpy_GetPointer_2D_associated(esmf_state_c_ptr, name_c_ptr, name_len, alloc) result(is_associated) bind(c, name="MAPLpy_GetPointer_2D_associated")
        ! Read in STATE
        type(c_ptr), intent(in), value :: esmf_state_c_ptr
        type(ESMF_State), pointer :: state

        ! Read in name
        type(c_ptr), intent(in), value :: name_c_ptr
        integer(c_int), intent(in), value :: name_len
        character(len=name_len,kind=c_char), pointer :: name
        logical(c_bool), intent(in), value :: alloc

        ! Results
        real, pointer, dimension(:,:) :: f_ptr
        logical(kind=c_bool):: is_associated

        ! Turn the C string into a Fortran string
        call c_f_pointer(name_c_ptr, name)

        ! Turn the ESMF State C pointer to a Fortran pointer
        call c_f_pointer(esmf_state_c_ptr, state)        
        
        call MAPL_GetPointer(state, f_ptr, trim(name), alloc=logical(alloc), RC=STATUS)
        VERIFY_(STATUS)
        is_associated = associated(f_ptr)
    
    end function

    function MAPLpy_GetPointer_3D(esmf_state_c_ptr, name_c_ptr, name_len, alloc) result(c_data_ptr) bind(c, name="MAPLpy_GetPointer_3D")
        ! Read in STATE
        type(c_ptr), intent(in), value :: esmf_state_c_ptr
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
        call c_f_pointer(esmf_state_c_ptr, state)    

        call MAPL_GetPointer(state, f_ptr, trim(name), alloc=logical(alloc), RC=STATUS)
        VERIFY_(STATUS)

        c_data_ptr=c_loc(f_ptr)
    
    end function

    function MAPLpy_GetPointer_3D_associated(esmf_state_c_ptr, name_c_ptr, name_len, alloc) result(is_associated) bind(c, name="MAPLpy_GetPointer_3D_associated")
        ! Read in STATE
        type(c_ptr), intent(in), value :: esmf_state_c_ptr
        type(ESMF_State), pointer :: state

        ! Read in name
        type(c_ptr), intent(in), value :: name_c_ptr
        integer(c_int), intent(in), value :: name_len
        character(len=name_len,kind=c_char), pointer :: name
        logical(c_bool), intent(in), value :: alloc

        ! Results
        real, pointer, dimension(:,:,:) :: f_ptr
        logical(kind=c_bool):: is_associated

        ! Turn the C string into a Fortran string
        call c_f_pointer(name_c_ptr, name)

        ! Turn the ESMF State C pointer to a Fortran pointer
        call c_f_pointer(esmf_state_c_ptr, state)        
        
        call MAPL_GetPointer(state, f_ptr, trim(name), alloc=logical(alloc), RC=STATUS)
        VERIFY_(STATUS)
        is_associated = associated(f_ptr)
    
    end function

    function MAPLpy_Associated(pointer_to_test) result(result) bind(c, name="MAPLpy_Associated")

        type(c_ptr), intent(in), value :: pointer_to_test
        type(ESMF_TimeInterval), pointer :: state
        logical(c_bool) :: result

        real, pointer :: f_ptr

        call c_f_pointer(pointer_to_test, f_ptr)
        result = associated(f_ptr)

    end function

    function MAPLpy_GetResource_Float(state_c_ptr, name_c_ptr, name_len, default) result(result) bind(c, name="MAPLpy_GetResource_Float")
        ! Read in STATE
        type(c_ptr), intent(in), value :: state_c_ptr
        type(MAPL_MetaComp), pointer :: state

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
        call c_f_pointer(state_c_ptr, state)        

        ! Use fortran type & cast back to C types
        local_d = default
        call MAPL_GetResource(state, local_r, label=trim(name), default=local_d, RC=STATUS)
        VERIFY_(STATUS)
        result = local_r
    end function

    function MAPLpy_GetResource_Bool(mapl_metacomp_c_ptr, name_c_ptr, name_len, default) result(result) bind(c, name="MAPLpy_GetResource_Bool")
        ! Read in STATE
        type(c_ptr), intent(in), value :: mapl_metacomp_c_ptr
        type(MAPL_MetaComp), pointer :: state

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

        ! Turn the ESMF State C pointer to a Fortran pointer
        call c_f_pointer(mapl_metacomp_c_ptr, state)        

        local_d = default
        call MAPL_GetResource(state, local_r, label=trim(name), default=local_d, RC=STATUS)
        VERIFY_(STATUS)
        result = local_r
    
    end function

    function MAPLpy_GetResource_Int32(state_c_ptr, name_c_ptr, name_len, default) result(result) bind(c, name="MAPLpy_GetResource_Int32")
        ! Read in STATE
        type(c_ptr), intent(in), value :: state_c_ptr
        type(MAPL_MetaComp), pointer :: state

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
        call c_f_pointer(state_c_ptr, state)              

        local_d = default
        call MAPL_GetResource(state, local_r, label=trim(name), default=local_d, RC=STATUS)
        VERIFY_(STATUS)
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

    function MAPLpy_ESMF_TimeIntervalGet(time_state_c_ptr) result(result) bind(c, name="MAPLpy_ESMF_TimeIntervalGet")
        ! Read in STATE
        type(c_ptr), intent(in), value :: time_state_c_ptr
        type(ESMF_TimeInterval), pointer :: state

        ! Results
        real(c_double) :: result

        ! Turn the ESMF State C pointer to a Fortran pointer
        call c_f_pointer(time_state_c_ptr, state)        

        call ESMF_TimeIntervalGet(state, S_R8=result)
    
    end function MAPLpy_ESMF_TimeIntervalGet

    function MAPLpy_ESMF_GridCompGetName(c_grid_comp) result(c_string) bind(c, name="MAPLpy_ESMF_GridCompGetName")
        
        type(c_ptr), intent(in), value     :: c_grid_comp

        type(ESMF_GridComp), pointer       :: f90_state
        type(c_ptr)                        :: c_string

        call c_f_pointer(c_grid_comp, f90_state)

        call ESMF_GridCompGet(f90_state, name=F90_STRING_BUFFER, RC=STATUS)
        c_string = c_loc(F90_STRING_BUFFER)

    end function MAPLpy_ESMF_GridCompGetName

    function MAPLPy_MAPL_GetIM(c_mapl_state) result(result) bind(c, name="MAPLPy_MAPL_GetIM")
        ! Read in STATE
        type(c_ptr), intent(in), value :: c_mapl_state
        type(MAPL_MetaComp), pointer :: state

        ! Results
        integer(C_INT32_T) :: result
        integer(kind=4) :: local_r

        ! Make pointer & string fortran from C
        call c_f_pointer(c_mapl_state, state)              

        call MAPL_Get(state, IM=local_r, RC=STATUS)
        print*, "MAPL_GetIM", result

        VERIFY_(STATUS)
        result = local_r
    
    end function

    function MAPLPy_MAPL_GetJM(c_mapl_state) result(result) bind(c, name="MAPLPy_MAPL_GetJM")
        ! Read in STATE
        type(c_ptr), intent(in), value :: c_mapl_state
        type(MAPL_MetaComp), pointer :: state

        ! Results
        integer(C_INT32_T) :: result
        integer(kind=4) :: local_r

        ! Make pointer & string fortran from C
        call c_f_pointer(c_mapl_state, state)              

        call MAPL_Get(state, JM=local_r, RC=STATUS)

        VERIFY_(STATUS)
        result = local_r
    
    end function

    function MAPLPy_MAPL_GetLM(c_mapl_state) result(result) bind(c, name="MAPLPy_MAPL_GetLM")
        ! Read in STATE
        type(c_ptr), intent(in), value :: c_mapl_state
        type(MAPL_MetaComp), pointer :: state

        ! Results
        integer(C_INT32_T) :: result
        integer(kind=4) :: local_r

        ! Make pointer & string fortran from C
        call c_f_pointer(c_mapl_state, state)              

        call MAPL_Get(state, LM=local_r, RC=STATUS)

        VERIFY_(STATUS)
        result = local_r
    
    end function

    function MAPLPy_MAPL_GetNX(c_mapl_state) result(result) bind(c, name="MAPLPy_MAPL_GetNX")
        ! Read in STATE
        type(c_ptr), intent(in), value :: c_mapl_state
        type(MAPL_MetaComp), pointer :: state

        ! Results
        integer(C_INT32_T) :: result
        integer(kind=4) :: local_r

        ! Make pointer & string fortran from C
        call c_f_pointer(c_mapl_state, state)              

        call MAPL_Get(state, NX=local_r, RC=STATUS)

        VERIFY_(STATUS)
        result = local_r
    
    end function

    function MAPLPy_MAPL_GetNY(c_mapl_state) result(result) bind(c, name="MAPLPy_MAPL_GetNY")
        ! Read in STATE
        type(c_ptr), intent(in), value :: c_mapl_state
        type(MAPL_MetaComp), pointer :: state

        ! Results
        integer(C_INT32_T) :: result
        integer(kind=4) :: local_r

        ! Make pointer & string fortran from C
        call c_f_pointer(c_mapl_state, state)              

        call MAPL_Get(state, NY=local_r, RC=STATUS)

        VERIFY_(STATUS)
        result = local_r
    
    end function

end module C_BRIDGE_TO_MAPL