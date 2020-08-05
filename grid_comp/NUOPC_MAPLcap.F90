#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"

module NUOPC_MAPLcapMod
    use ESMF
    use NUOPC
    use NUOPC_Model, &
            model_SetServices    => SetServices, &
            model_Advance        => label_advance, &
            model_CheckImport    => label_CheckImport, &
            model_DataInitialize => label_DataInitialize, &
            model_SetClock       => label_SetClock, &
            model_Finalize       => label_finalize
    use NUOPC_MAPLcapClass
    use MAPL_Mod

    implicit none
    private

    public SetServices
    public init_internal_wrapper

    character(*), parameter :: internal_name = "NUOPC_MAPLcap"

    type NUOPC_MAPLcap_wrapper
        type(NUOPC_MAPLcap), pointer :: ptr
    end type NUOPC_MAPLcap_wrapper

#include "mpif.h"

contains
    subroutine SetServices(model, rc)
        type(ESMF_GridComp)  :: model
        integer, intent(out) :: rc

        rc = ESMF_SUCCESS

        ! the NUOPC model component will register the generic methods
        call NUOPC_CompDerive(model, model_SetServices, rc=rc)
        VERIFY_NUOPC_(rc)

        call ESMF_GridCompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
                userRoutine=initialize_p0, phase=0, rc=rc)
        VERIFY_NUOPC_(rc)

        ! set entry point for methods that require specific implementation
        call NUOPC_CompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
                phaseLabelList=["IPDv05p1"], userRoutine=initialize_p1, rc=rc)
        VERIFY_NUOPC_(rc)
        call NUOPC_CompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
                phaseLabelList=["IPDv05p2"], userRoutine=initialize_p2, rc=rc)
        VERIFY_NUOPC_(rc)
        call NUOPC_CompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
                phaseLabelList=["IPDv05p3"], userRoutine=initialize_p3, rc=rc)
        VERIFY_NUOPC_(rc)
        call NUOPC_CompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
                phaseLabelList=["IPDv05p4"], userRoutine=initialize_p4, rc=rc)
        VERIFY_NUOPC_(rc)
        call NUOPC_CompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
                phaseLabelList=["IPDv05p5"], userRoutine=initialize_p5, rc=rc)
        VERIFY_NUOPC_(rc)
        call NUOPC_CompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
                phaseLabelList=["IPDv05p6"], userRoutine=initialize_p6, rc=rc)
        VERIFY_NUOPC_(rc)
        call NUOPC_CompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
                phaseLabelList=["IPDv05p7"], userRoutine=initialize_p7, rc=rc)
        VERIFY_NUOPC_(rc)

        ! attach specializing method(s)
        call NUOPC_CompSpecialize(model, specLabel=model_DataInitialize, &
                specRoutine=initialize_data, rc=rc)
        VERIFY_NUOPC_(rc)
        call NUOPC_CompSpecialize(model, specLabel=model_Advance, &
                specRoutine=advance, rc=rc)
        VERIFY_NUOPC_(rc)
        call ESMF_MethodRemove(model, label=model_CheckImport, rc=rc)
        VERIFY_NUOPC_(rc)
        call NUOPC_CompSpecialize(model, specLabel=model_CheckImport, &
                specRoutine=check_import, rc=rc)
        VERIFY_NUOPC_(rc)
        call NUOPC_CompSpecialize(model, specLabel=model_SetClock, &
                specRoutine=set_clock, rc=rc)
        VERIFY_NUOPC_(rc)
        call NUOPC_CompSpecialize(model, specLabel=model_Finalize, &
                specRoutine=finalize, rc=rc)
        VERIFY_NUOPC_(rc)
    end subroutine SetServices

    subroutine initialize_p0(model, import_state, export_state, clock, rc)
        type(ESMF_GridComp)  :: model
        type(ESMF_State)     :: import_state
        type(ESMF_State)     :: export_state
        type(ESMF_Clock)     :: clock
        integer, intent(out) :: rc

        type(NUOPC_MAPLcap) :: cap

        rc = ESMF_SUCCESS

        call NUOPC_CompFilterPhaseMap(model, ESMF_METHOD_INITIALIZE, &
                acceptStringList=["IPDv05p"], rc=rc)
        VERIFY_NUOPC_(rc)

        cap = get_NUOPC_MAPLcap(model, rc)
        VERIFY_NUOPC_(rc)

        call cap%init_p0(model, import_state, export_state, clock, rc)
        VERIFY_NUOPC_(rc)
    end subroutine initialize_p0

    subroutine initialize_p1(model, import_state, export_state, clock, rc)
        type(ESMF_GridComp)  :: model
        type(ESMF_State)     :: import_state
        type(ESMF_State)     :: export_state
        type(ESMF_Clock)     :: clock
        integer, intent(out) :: rc

        type(NUOPC_MAPLcap) :: cap

        rc = ESMF_SUCCESS

        cap = get_NUOPC_MAPLcap(model, rc)
        VERIFY_NUOPC_(rc)

        call cap%init_p1(import_state, export_state, clock, rc)
        VERIFY_NUOPC_(rc)
    end subroutine initialize_p1

    subroutine initialize_p2(model, import_state, export_state, clock, rc)
        type(ESMF_GridComp)  :: model
        type(ESMF_State)     :: import_state
        type(ESMF_State)     :: export_state
        type(ESMF_Clock)     :: clock
        integer, intent(out) :: rc

        type(NUOPC_MAPLcap) :: cap

        rc = ESMF_SUCCESS

        cap = get_NUOPC_MAPLcap(model, rc)
        VERIFY_NUOPC_(rc)

        call cap%init_p2(import_state, export_state, clock, rc)
        VERIFY_NUOPC_(rc)
    end subroutine initialize_p2

    subroutine initialize_p3(model, import_state, export_state, clock, rc)
        type(ESMF_GridComp)  :: model
        type(ESMF_State)     :: import_state
        type(ESMF_State)     :: export_state
        type(ESMF_Clock)     :: clock
        integer, intent(out) :: rc

        type(NUOPC_MAPLcap) :: cap

        rc = ESMF_SUCCESS

        cap = get_NUOPC_MAPLcap(model, rc)
        VERIFY_NUOPC_(rc)

        call cap%init_p3(import_state, export_state, clock, rc)
        VERIFY_NUOPC_(rc)
    end subroutine initialize_p3

    subroutine initialize_p4(model, import_state, export_state, clock, rc)
        type(ESMF_GridComp)  :: model
        type(ESMF_State)     :: import_state
        type(ESMF_State)     :: export_state
        type(ESMF_Clock)     :: clock
        integer, intent(out) :: rc

        type(NUOPC_MAPLcap) :: cap

        rc = ESMF_SUCCESS

        cap = get_NUOPC_MAPLcap(model, rc)
        VERIFY_NUOPC_(rc)

        call cap%init_p4(import_state, export_state, clock, rc)
        VERIFY_NUOPC_(rc)
    end subroutine initialize_p4

    subroutine initialize_p5(model, import_state, export_state, clock, rc)
        type(ESMF_GridComp)  :: model
        type(ESMF_State)     :: import_state
        type(ESMF_State)     :: export_state
        type(ESMF_Clock)     :: clock
        integer, intent(out) :: rc

        type(NUOPC_MAPLcap) :: cap

        rc = ESMF_SUCCESS

        cap = get_NUOPC_MAPLcap(model, rc)
        VERIFY_NUOPC_(rc)

        call cap%init_p5(import_state, export_state, clock, rc)
        VERIFY_NUOPC_(rc)
    end subroutine initialize_p5

    subroutine initialize_p6(model, import_state, export_state, clock, rc)
        type(ESMF_GridComp)  :: model
        type(ESMF_State)     :: import_state
        type(ESMF_State)     :: export_state
        type(ESMF_Clock)     :: clock
        integer, intent(out) :: rc

        type(NUOPC_MAPLcap) :: cap

        rc = ESMF_SUCCESS

        cap = get_NUOPC_MAPLcap(model, rc)
        VERIFY_NUOPC_(rc)

        call cap%init_p6(import_state, export_state, clock, rc)
        VERIFY_NUOPC_(rc)
    end subroutine initialize_p6

    subroutine initialize_p7(model, import_state, export_state, clock, rc)
        type(ESMF_GridComp)  :: model
        type(ESMF_State)     :: import_state
        type(ESMF_State)     :: export_state
        type(ESMF_Clock)     :: clock
        integer, intent(out) :: rc

        type(NUOPC_MAPLcap) :: cap

        rc = ESMF_SUCCESS

        cap = get_NUOPC_MAPLcap(model, rc)
        VERIFY_NUOPC_(rc)

        call cap%init_p7(import_state, export_state, clock, rc)
        VERIFY_NUOPC_(rc)
    end subroutine initialize_p7

    subroutine initialize_data(model, rc)
        type(ESMF_GridComp)  :: model
        integer, intent(out) :: rc

        type(NUOPC_MAPLcap) :: cap

        rc = ESMF_SUCCESS

        cap = get_NUOPC_MAPLcap(model, rc)
        VERIFY_NUOPC_(rc)

        call cap%data_init(model, rc)
        VERIFY_NUOPC_(rc)
    end subroutine initialize_data

    subroutine advance(model, rc)
        type(ESMF_GridComp)  :: model
        integer, intent(out) :: rc

        type(NUOPC_MAPLcap) :: cap

        rc = ESMF_SUCCESS

        cap = get_NUOPC_MAPLcap(model, rc)
        VERIFY_NUOPC_(rc)

        call cap%advance(rc)
        VERIFY_NUOPC_(rc)
    end subroutine advance

    subroutine check_import(model, rc)
        type(ESMF_GridComp)  :: model
        integer, intent(out) :: rc

        type(NUOPC_MAPLcap) :: cap

        rc = ESMF_SUCCESS

        cap = get_NUOPC_MAPLcap(model, rc)
        VERIFY_NUOPC_(rc)

        call cap%check_import(rc)
        VERIFY_NUOPC_(rc)
    end subroutine check_import

    subroutine set_clock(model, rc)
        type(ESMF_GridComp)  :: model
        integer, intent(out) :: rc

        type(NUOPC_MAPLcap) :: cap

        rc = ESMF_SUCCESS

        cap = get_NUOPC_MAPLcap(model, rc)
        VERIFY_NUOPC_(rc)

        call cap%set_clock(model, rc)
        VERIFY_NUOPC_(rc)
    end subroutine set_clock

    subroutine finalize(model, rc)
        type(ESMF_GridComp)  :: model
        integer, intent(out) :: rc

        type(NUOPC_MAPLcap) :: cap

        rc = ESMF_SUCCESS

        cap = get_NUOPC_MAPLcap(model, rc)
        VERIFY_NUOPC_(rc)

        call cap%finalize(rc)
        VERIFY_NUOPC_(rc)
    end subroutine finalize

    subroutine init_internal_wrapper(gc, name, rc_file, root_set_services, rc)
        type(ESMF_GridComp), target, intent(inout) :: gc
        character(*),                intent(in   ) :: name
        character(*),                intent(in   ) :: rc_file
        procedure(abs_set_services)                :: root_set_services
        integer,                     intent(  out) :: rc

        type(NUOPC_MAPLcap_wrapper)  :: wrapper

        rc = ESMF_SUCCESS

        allocate(wrapper%ptr)
        wrapper%ptr = NUOPC_MAPLcap(name, rc_file, root_set_services)

        call ESMF_UserCompSetInternalState(gc, internal_name, wrapper, rc)
        VERIFY_NUOPC_(rc)
    end subroutine init_internal_wrapper

    function get_NUOPC_MAPLcap(gc, rc) result(cap)
        type(ESMF_GridComp), intent(inout) :: gc
        integer,             intent(  out) :: rc
        type(NUOPC_MAPLcap)                :: cap

        type(NUOPC_MAPLcap_wrapper) :: wrapper

        rc = ESMF_SUCCESS

        call ESMF_UserCompGetInternalState(gc, internal_name, wrapper, rc)
        VERIFY_NUOPC_(rc)

        cap = wrapper%ptr
    end function get_NUOPC_MAPLcap
end module NUOPC_MAPLcapMod
