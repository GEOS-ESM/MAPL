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
    use NUOPCmapMod
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

        integer :: i

        rc = ESMF_SUCCESS

        print*, "NUOPC_MAPLcap start SetServices"
        call ESMF_LogWrite("NUOPC_MAPLcap start SetServices", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)

        ! the NUOPC model component will register the generic methods
        call NUOPC_CompDerive(model, model_SetServices, rc=rc)
        VERIFY_NUOPC_(rc)

        call ESMF_GridCompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
                userRoutine=initialize_p0, phase=0, rc=rc)
        VERIFY_NUOPC_(rc)

        ! set entry point for methods that require specific implementation
        do i=1, num_phases
            call NUOPC_CompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
                    phaseLabelList=[phase_label_list(i)], userRoutine=generic_initialize, rc=rc)
            VERIFY_NUOPC_(rc)
        end do

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

        print*, "NUOPC_MAPLcap finish SetServices"
        call ESMF_LogWrite("NUOPC_MAPLcap finish SetServices", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
    end subroutine SetServices

    subroutine initialize_p0(model, import_state, export_state, clock, rc)
        type(ESMF_GridComp)  :: model
        type(ESMF_State)     :: import_state
        type(ESMF_State)     :: export_state
        type(ESMF_Clock)     :: clock
        integer, intent(out) :: rc

        type(NUOPC_MAPLcap), pointer :: cap

        rc = ESMF_SUCCESS

        print*, "NUOPC_MAPLcap start initialize_p0"
        call ESMF_LogWrite("NUOPC_MAPLcap start initialize_p0", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)

        call NUOPC_CompFilterPhaseMap(model, ESMF_METHOD_INITIALIZE, &
                acceptStringList=["IPDv05p"], rc=rc)
        VERIFY_NUOPC_(rc)

        cap => get_NUOPC_MAPLcap(model, rc)
        VERIFY_NUOPC_(rc)

        call cap%init_p0(model, import_state, export_state, clock, rc)
        VERIFY_NUOPC_(rc)

        print*, "NUOPC_MAPLcap finish initialize_p0"
        call ESMF_LogWrite("NUOPC_MAPLcap finish initialize_p0", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
    end subroutine initialize_p0

    subroutine generic_initialize(model, import_state, export_state, clock, rc)
        type(ESMF_GridComp)  :: model
        type(ESMF_State)     :: import_state
        type(ESMF_State)     :: export_state
        type(ESMF_Clock)     :: clock
        integer, intent(out) :: rc

        type(NUOPC_MAPLcap), pointer :: cap

        rc = ESMF_SUCCESS

        print*, "NUOPC_MAPLcap start generic_initialize"
        call ESMF_LogWrite("NUOPC_MAPLcap start generic_initialize", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)

        cap => get_NUOPC_MAPLcap(model, rc)
        VERIFY_NUOPC_(rc)

        call cap%generic_init(model, import_state, export_state, clock, rc)
        VERIFY_NUOPC_(rc)

        print*, "NUOPC_MAPLcap finish generic_initialize"
        call ESMF_LogWrite("NUOPC_MAPLcap finish generic_initialize", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
    end subroutine generic_initialize

    subroutine initialize_data(model, rc)
        type(ESMF_GridComp)  :: model
        integer, intent(out) :: rc

        type(NUOPC_MAPLcap), pointer :: cap

        rc = ESMF_SUCCESS

        print*, "NUOPC_MAPLcap start initialize_data"
        call ESMF_LogWrite("NUOPC_MAPLcap start initialize_data", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)

        cap => get_NUOPC_MAPLcap(model, rc)
        VERIFY_NUOPC_(rc)

        call cap%data_init(model, rc)
        VERIFY_NUOPC_(rc)

        print*, "NUOPC_MAPLcap finish initialize_data"
        call ESMF_LogWrite("NUOPC_MAPLcap finish initialize_data", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
    end subroutine initialize_data

    subroutine advance(model, rc)
        type(ESMF_GridComp)  :: model
        integer, intent(out) :: rc

        type(NUOPC_MAPLcap), pointer :: cap

        rc = ESMF_SUCCESS

        print*, "NUOPC_MAPLcap start advance"
        call ESMF_LogWrite("NUOPC_MAPLcap start advance", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)

        cap => get_NUOPC_MAPLcap(model, rc)
        VERIFY_NUOPC_(rc)

        call cap%advance(rc)
        VERIFY_NUOPC_(rc)

        print*, "NUOPC_MAPLcap finish advance"
        call ESMF_LogWrite("NUOPC_MAPLcap finish advance", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
    end subroutine advance

    subroutine check_import(model, rc)
        type(ESMF_GridComp)  :: model
        integer, intent(out) :: rc

        type(NUOPC_MAPLcap), pointer :: cap

        rc = ESMF_SUCCESS

        print*, "NUOPC_MAPLcap start check_import"
        call ESMF_LogWrite("NUOPC_MAPLcap start check_import", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)

        cap => get_NUOPC_MAPLcap(model, rc)
        VERIFY_NUOPC_(rc)

        call cap%check_import(rc)
        VERIFY_NUOPC_(rc)

        print*, "NUOPC_MAPLcap finish check_import"
        call ESMF_LogWrite("NUOPC_MAPLcap finish check_import", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
    end subroutine check_import

    subroutine set_clock(model, rc)
        type(ESMF_GridComp)  :: model
        integer, intent(out) :: rc

        type(NUOPC_MAPLcap), pointer :: cap

        rc = ESMF_SUCCESS

        print*, "NUOPC_MAPLcap start set_clock"
        call ESMF_LogWrite("NUOPC_MAPLcap start set_clock", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)

        cap => get_NUOPC_MAPLcap(model, rc)
        VERIFY_NUOPC_(rc)

        call cap%set_clock(model, rc)
        VERIFY_NUOPC_(rc)

        print*, "NUOPC_MAPLcap finish set_clock"
        call ESMF_LogWrite("NUOPC_MAPLcap finish set_clock", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
    end subroutine set_clock

    subroutine finalize(model, rc)
        type(ESMF_GridComp)  :: model
        integer, intent(out) :: rc

        type(NUOPC_MAPLcap), pointer :: cap

        rc = ESMF_SUCCESS

        print*, "NUOPC_MAPLcap start finialize"
        call ESMF_LogWrite("NUOPC_MAPLcap start finalize", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)

        cap => get_NUOPC_MAPLcap(model, rc)
        VERIFY_NUOPC_(rc)

        call cap%finalize(rc)
        VERIFY_NUOPC_(rc)

        print*, "NUOPC_MAPLcap finish finialize"
        call ESMF_LogWrite("NUOPC_MAPLcap finish finalize", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
    end subroutine finalize

    subroutine init_internal_wrapper(gc, name, rc_file, root_set_services, rc)
        type(ESMF_GridComp), target, intent(inout) :: gc
        character(*),                intent(in   ) :: name
        character(*),                intent(in   ) :: rc_file
        procedure(abs_set_services)                :: root_set_services
        integer,                     intent(  out) :: rc

        type(MAPL_Cap), pointer      :: cap
        type(NUOPC_MAPLcap_wrapper)  :: wrapper

        rc = ESMF_SUCCESS

        print*, "NUOPC_MAPLcap start init_internal_wrapper"
        call ESMF_LogWrite("NUOPC_MAPLcap start init_internal_wrapper", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)

        allocate(wrapper%ptr)
        wrapper%ptr = NUOPC_MAPLcap(name, rc_file, root_set_services)

        call ESMF_UserCompSetInternalState(gc, internal_name, wrapper, rc)
        VERIFY_NUOPC_(rc)

        print*, "NUOPC_MAPLcap finish init_internal_wrapper"
        call ESMF_LogWrite("NUOPC_MAPLcap finish init_internal_wrapper", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
    end subroutine init_internal_wrapper

    function get_NUOPC_MAPLcap(gc, rc) result(cap)
        type(ESMF_GridComp), intent(inout) :: gc
        integer,             intent(  out) :: rc
        type(NUOPC_MAPLcap), pointer       :: cap

        type(NUOPC_MAPLcap_wrapper) :: wrapper

        rc = ESMF_SUCCESS

        print*, "NUOPC_MAPLcap start get_NUOPC_MAPLcap"
        call ESMF_LogWrite("NUOPC_MAPLcap start get_NUOPC_MAPLcap", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)

        call ESMF_UserCompGetInternalState(gc, internal_name, wrapper, rc)
        VERIFY_NUOPC_(rc)

        cap => wrapper%ptr

        print*, "NUOPC_MAPLcap finish get_NUOPC_MAPLcap"
        call ESMF_LogWrite("NUOPC_MAPLcap finish get_NUOPC_MAPLcap", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
    end function get_NUOPC_MAPLcap
end module NUOPC_MAPLcapMod
