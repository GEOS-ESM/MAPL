#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"

module MAPL_NUOPCWrapperMod
    !-----------------------------------------------------------------------------
    ! ATM Component.
    !-----------------------------------------------------------------------------
    use ESMF
    use NUOPC
    use NUOPC_Model, &
        model_routine_SS    => SetServices, &
        model_label_Advance => label_Advance, &
        model_label_CheckImport => label_CheckImport, &
        model_label_DataInitialize => label_DataInitialize, &
        model_label_SetClock => label_SetClock, &
        model_label_SetRunClock => label_SetRunClock
    use MAPL_Mod
    use MAPL_CapMod
    use pFIO
    use MAPL_Profiler, only: BaseProfiler, get_global_time_profiler
    use pflogger, only: pfl_initialize => initialize

    implicit none
    private

    public SetServices
    public cap_parameters
    public get_cap_parameters_from_gc
    public init_wrapper

    character(*), parameter :: internal_parameters_name = "cap_parameters"

    type :: Field_Attributes
        type(ESMF_Field)           :: field
        character(len=ESMF_MAXSTR) :: short_name, long_name, units
    end type Field_Attributes

    type Cap_Wrapper
        type(MAPL_Cap), pointer :: ptr
    end type Cap_Wrapper

    abstract interface
        subroutine set_services_interface(gc, rc)
            import ESMF_GridComp
            type(ESMF_GridComp), intent(inout) :: gc
            integer,             intent(  out) :: rc
        end subroutine set_services_interface
    end interface

    ! Values needed to create CapGridComp.
    type :: cap_parameters
        character(len=:), allocatable                      :: name, cap_rc_file
        procedure(set_services_interface), nopass, pointer :: set_services
    end type cap_parameters

    type :: cap_parameters_wrapper
        type(cap_parameters), pointer :: ptr
    end type cap_parameters_wrapper

#include "mpif.h"

contains
    subroutine SetServices(model, rc)
        type(ESMF_GridComp)  :: model
        integer, intent(out) :: rc

        ! the NUOPC model component will register the generic methods
        call NUOPC_CompDerive(model, model_routine_SS, rc=rc)
        VERIFY_NUOPC_(rc)

        call ESMF_GridCompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
                userRoutine=initialize_p0, phase=0, rc=rc)
        VERIFY_NUOPC_(rc)

        ! set entry point for methods that require specific implementation
        call NUOPC_CompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
                phaseLabelList=["IPDv05p1"], userRoutine=advertise_fields, rc=rc)
        VERIFY_NUOPC_(rc)

        call NUOPC_CompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
                phaseLabelList=["IPDv05p4"], userRoutine=realize_fields, rc=rc)
        VERIFY_NUOPC_(rc)

        ! attach specializing method(s)
        call NUOPC_CompSpecialize(model, specLabel=model_label_DataInitialize, &
                specRoutine=initialize_data, rc=rc)
        VERIFY_NUOPC_(rc)

        call NUOPC_CompSpecialize(model, specLabel=model_label_Advance, &
                specRoutine=model_advance, rc=rc)
        VERIFY_NUOPC_(rc)

        call ESMF_MethodRemove(model, label=model_label_CheckImport, rc=rc)
        VERIFY_NUOPC_(rc)

        call NUOPC_CompSpecialize(model, specLabel=model_label_CheckImport, &
                specRoutine=CheckImport, rc=rc)
        VERIFY_NUOPC_(rc)

        call NUOPC_CompSpecialize(model, specLabel=model_label_SetClock, &
                specRoutine=set_clock, rc=rc)
        VERIFY_NUOPC_(rc)

        ! call NUOPC_CompSpecialize(model, specLabel=model_label_CheckImport, &
        !      specRoutine=CheckImport, rc=rc)
        ! if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        !      line=__LINE__, &
        !      file=__FILE__)) &
        !      return  ! bail out

        call NUOPC_CompSpecialize(model, specLabel=label_Finalize, &
                specRoutine=model_finalize, rc=rc)
        VERIFY_NUOPC_(rc)

        print*,"Wrapper finish SetServices"

        _RETURN(_SUCCESS)
    end subroutine SetServices

    subroutine initialize_p0(model, import_state, export_state, clock, rc)
        type(ESMF_GridComp)  :: model
        type(ESMF_State)     :: import_state, export_state
        type(ESMF_Clock)     :: clock
        integer, intent(out) :: rc

        type(MAPL_Cap), pointer :: cap
        type(Cap_Wrapper)       :: wrapped_cap
        type(cap_parameters)    :: cap_params

        type(ESMF_VM) :: vm
        integer       :: my_rank, npes, mpi_comm, dup_comm, status, subcommunicator

        type(MAPL_CapOptions) :: cap_options
        logical, save         :: first = .true.

        _UNUSED_DUMMY(import_state)
        _UNUSED_DUMMY(export_state)
        _UNUSED_DUMMY(clock)

        call NUOPC_CompFilterPhaseMap(model, ESMF_METHOD_INITIALIZE, &
                acceptStringList=(/"IPDv05p"/), rc=rc)
        VERIFY_NUOPC_(rc)

        call ESMF_GridCompGet(model, vm=vm, rc=status)
        _VERIFY(status)
        call ESMF_VMGet(vm, localPet=my_rank, petCount=npes, &
                mpiCommunicator=mpi_comm, rc=status)
        _VERIFY(status)

        call MPI_Comm_dup(mpi_comm, dup_comm, status)
        _VERIFY(status)

        cap_params = get_cap_parameters_from_gc(model, status)
        _VERIFY(status)

        cap_options = MAPL_CapOptions(cap_rc_file=cap_params%cap_rc_file, rc=status)
        _VERIFY(status)
        cap_options%use_comm_world = .false.
        cap_options%comm = dup_comm
        ! cap_options%logging_config = "logging.yaml"
        cap_options%logging_config = ''
        call MPI_Comm_size(dup_comm, cap_options%npes_model, status)
        _VERIFY(status)

        allocate(cap)
        cap = MAPL_Cap(cap_params%name, cap_params%set_services,  &
                cap_options=cap_options, rc=status)
        _VERIFY(status)
        wrapped_cap%ptr => cap

        call ESMF_UserCompSetInternalState(model, "MAPL_Cap", &
                wrapped_cap, status)
        _VERIFY(status)

        call cap%initialize_mpi(rc = status)
        _VERIFY(status)

        subcommunicator = cap%create_member_subcommunicator(cap%get_comm_world(), rc=status)
        _VERIFY(status)

        if (first) then
            call cap%initialize_io_clients_servers(subcommunicator, rc=status)
            _VERIFY(status)
            first = .false.
        end if

        call cap%nuopc_fill_mapl_comm(rc=status)
        _VERIFY(status)
        call cap%initialize_cap_gc(cap%get_mapl_comm())

        call cap%cap_gc%set_services(rc=status)
        _VERIFY(status)
        call cap%cap_gc%initialize(rc=status)
        _VERIFY(status)

        _RETURN(_SUCCESS)
    end subroutine initialize_p0

    subroutine set_clock(model, rc)
        type(ESMF_GridComp)  :: model
        integer, intent(out) :: rc

        type(ESMF_Clock)        :: model_clock
        type(MAPL_Cap), pointer :: cap
        type(ESMF_TimeInterval) :: time_step
        integer                 :: heartbeat_dt

        cap => get_cap_from_gc(model, rc)
        VERIFY_NUOPC_(rc)
        heartbeat_dt = cap%cap_gc%get_heartbeat_dt()

        call NUOPC_ModelGet(model, modelClock=model_clock, rc=rc)
        VERIFY_NUOPC_(rc)

        call ESMF_TimeIntervalSet(time_step, s=heartbeat_dt, rc=rc)
        VERIFY_NUOPC_(rc)

        call ESMF_ClockSet(model_clock, timeStep=time_step)
        VERIFY_NUOPC_(rc)

        _RETURN(_SUCCESS)
    end subroutine set_clock

    subroutine advertise_fields(model, import_state, export_state, clock, rc)
        type(ESMF_GridComp)  :: model
        type(ESMF_State)     :: import_state, export_state
        type(ESMF_Clock)     :: clock
        integer, intent(out) :: rc

        type(MAPL_Cap), pointer             :: cap
        type(Field_Attributes), allocatable :: export_attributes(:), import_attributes(:)

        _UNUSED_DUMMY(clock)

        cap => get_cap_from_gc(model, rc)
        VERIFY_NUOPC_(rc)

        export_attributes = get_field_attributes_from_state(cap%cap_gc%export_state)
        import_attributes = get_field_attributes_from_state(cap%cap_gc%import_state)

        call advertise_to_state(import_state, import_attributes)
        call advertise_to_state(export_state, export_attributes)

        _RETURN(_SUCCESS)
    contains
        subroutine advertise_to_state(state, fields)
            type(ESMF_State),       intent(inout) :: state
            type(field_attributes), intent(in   ) :: fields(:)
            integer :: i, status

            do i = 1, size(fields)
                associate(short_name => fields(i)%short_name, units => fields(i)%units)
                    if (.not. NUOPC_FieldDictionaryHasEntry(short_name)) then
                        call NUOPC_FieldDictionaryAddEntry(standardName=trim(short_name), &
                                canonicalUnits=trim(units), rc=status)
                        VERIFY_NUOPC_(status)
                    end if

                    call NUOPC_Advertise(state, StandardName=trim(short_name), &
                            TransferOfferGeomObject="will provide", rc=status)
                    VERIFY_NUOPC_(status)
                end associate
            end do
        end subroutine advertise_to_state
    end subroutine advertise_fields

    subroutine realize_fields(model, import_state, export_state, clock, rc)
        type(ESMF_GridComp)  :: model
        type(ESMF_State)     :: import_state, export_state
        type(ESMF_Clock)     :: clock
        integer, intent(out) :: rc

        type(MAPL_Cap), pointer             :: cap
        type(Field_Attributes), allocatable :: export_attributes(:), import_attributes(:)
        integer                             :: i, status

        _UNUSED_DUMMY(clock)

        cap => get_cap_from_gc(model, rc)
        VERIFY_NUOPC_(rc)
        export_attributes = get_field_attributes_from_state(cap%cap_gc%export_state)
        import_attributes = get_field_attributes_from_state(cap%cap_gc%import_state)

        do i = 1, size(export_attributes)
            associate(export => export_attributes(i))
                call MAPL_AllocateCoupling(export%field, status)
                _VERIFY(status)
                call NUOPC_Realize(export_state, field=export%field, rc=status)
                VERIFY_NUOPC_(status)
            end associate
        end do

        do i = 1, size(import_attributes)
            associate(import => import_attributes(i))
                call ESMF_FieldValidate(import%field, rc=status)
                VERIFY_NUOPC_(status)
                call NUOPC_Realize(import_state, field=import%field, rc=status)
                VERIFY_NUOPC_(status)
            end associate
        end do

        _RETURN(_SUCCESS)
    end subroutine realize_fields

    subroutine CheckImport(model, rc)
        type(ESMF_GridComp)  :: model
        integer, intent(out) :: rc

        ! This is the routine that enforces the implicit time dependence on the
        ! import fields. This simply means that the timestamps on the Fields in the
        ! importState are checked against the stopTime on the Component's
        ! internalClock. Consequenty, this model starts out with forcing fields
        ! at the future stopTime, as it does its forward stepping from currentTime
        ! to stopTime.

        _UNUSED_DUMMY(model)

        _RETURN(_SUCCESS)
    end subroutine CheckImport

    subroutine initialize_data(model, rc)
        type(ESMF_GridComp)  :: model
        integer, intent(out) :: rc

        type(ESMF_State) :: import_state, export_state
        type(ESMF_Clock) :: clock
        type(ESMF_Field) :: field

        integer                                 :: num_items
        character(len=ESMF_MAXSTR), allocatable :: item_names(:)

        call ESMF_GridCompGet(model, clock=clock, importState=import_state, &
                exportState=export_state, rc=rc)
        VERIFY_NUOPC_(rc)

        call ESMF_StateGet(export_state, itemcount=num_items, rc=rc)
        VERIFY_NUOPC_(rc)

        ! if (num_items /= 0) then
        !    allocate(item_names(num_items))

        !    call ESMF_StateGet(export_state, itemnamelist = item_names, rc = rc)
        !    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        !         line=__LINE__, file=__FILE__)) return

        !    do i = 1, num_items
        !       call ESMF_StateGet(export_state, item_names(i), field, rc = rc)
        !       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        !            line=__LINE__, file=__FILE__)) return

        !       call NUOPC_SetAttribute(field, name="Updated", value="true", rc=rc)
        !       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        !            line=__LINE__, &
        !            file=__FILE__)) &
        !            return  ! bail out
        !    end do
        ! end if

        call NUOPC_CompAttributeSet(model, &
                name="InitializeDataComplete", value="true", rc=rc)
        VERIFY_NUOPC_(rc)

        _RETURN(_SUCCESS)
    end subroutine initialize_data

    subroutine model_advance(model, rc)
        type(ESMF_GridComp)  :: model
        integer, intent(out) :: rc

        type(MAPL_Cap), pointer :: cap

        cap => get_cap_from_gc(model, rc)
        VERIFY_NUOPC_(rc)

        call cap%step_model(rc=rc)
        _VERIFY(rc)

        _RETURN(_SUCCESS)
    end subroutine model_advance

    subroutine model_finalize(model, rc)
        type(ESMF_GridComp)  :: model
        integer, intent(out) :: rc

        type(MAPL_Cap),     pointer :: cap
        class(BaseProfiler), pointer :: t_p

        cap => get_cap_from_gc(model, rc)
        _VERIFY(rc)
        call cap%cap_gc%finalize(rc=rc)
        _VERIFY(rc)

        call i_Clients%terminate()
        call o_Clients%terminate()

        call cap%finalize_io_clients_servers(rc=rc)
        _VERIFY(rc)

        t_p => get_global_time_profiler()
        call t_p%stop()

        _RETURN(_SUCCESS)
    end subroutine model_finalize

    function get_cap_from_gc(gc, rc) result(cap)
        type(ESMF_GridComp), intent(inout) :: gc
        integer,             intent(  out) :: rc

        type(MAPL_Cap), pointer :: cap
        type(Cap_Wrapper)       :: wrapped_cap

        call ESMF_UserCompGetInternalState(gc, "MAPL_Cap", wrapped_cap, rc)
        VERIFY_NUOPC_(rc)

        cap => wrapped_cap%ptr

        _RETURN(_SUCCESS)
    end function get_cap_from_gc

    function get_field_attributes_from_state(state) result(attributes)
        type(Field_Attributes), allocatable :: attributes(:)
        type(ESMF_State), intent(in)        :: state

        integer                                 :: num_items, rc, i
        type(ESMF_Field)                        :: field
        character(len=ESMF_MAXSTR), allocatable :: item_names(:)
        character(len=ESMF_MAXSTR)              :: str

        call ESMF_StateGet(state, itemcount = num_items, rc = rc)
        VERIFY_NUOPC_(rc)

        allocate(item_names(num_items))
        allocate(attributes(num_items))

        call ESMF_StateGet(state, itemnamelist = item_names, rc = rc)
        VERIFY_NUOPC_(rc)

        do i = 1, num_items
            call ESMF_StateGet(state, item_names(i), field, rc = rc)
            VERIFY_NUOPC_(rc)

            call ESMF_FieldValidate(field, rc = rc)
            VERIFY_NUOPC_(rc)
            attributes(i)%field = field

            call ESMF_AttributeGet(field, name = "LONG_NAME", value = str, rc = rc)
            VERIFY_NUOPC_(rc)
            attributes(i)%long_name = trim(str)

            call ESMF_FieldGet(field, name = str, rc = rc)
            VERIFY_NUOPC_(rc)
            attributes(i)%short_name = trim(str)

            call ESMF_AttributeGet(field, name = "UNITS", value = str, rc = rc)
            VERIFY_NUOPC_(rc)
            if (str == "" .or. str == " ") str = "1" ! NUOPC doesn't like blank units
            attributes(i)%units = trim(str)
        end do
    end function get_field_attributes_from_state

    function get_cap_parameters_from_gc(gc, rc) result(cap_params)
        type(cap_parameters)               :: cap_params
        type(ESMF_GridComp), intent(inout) :: gc
        integer,             intent(  out) :: rc

        type(cap_parameters_wrapper) :: parameters_wrapper

        call ESMF_UserCompGetInternalState(gc, internal_parameters_name, parameters_wrapper, rc)
        VERIFY_NUOPC_(rc)

        cap_params = parameters_wrapper%ptr

        _RETURN(_SUCCESS)
    end function get_cap_parameters_from_gc

    subroutine add_wrapper_comp(driver, name, cap_rc_file, root_set_services, pet_list, wrapper_gc, rc)
        use NUOPC_Driver

        type(ESMF_GridComp), intent(inout) :: driver
        character(len=*),    intent(in   ) :: name, cap_rc_file
        procedure(set_services_interface)  :: root_set_services
        integer, intent(in), optional      :: pet_list(:)
        type(ESMF_GridComp), intent(out)   :: wrapper_gc
        integer,             intent(out)   :: rc

        type(cap_parameters_wrapper) :: wrapper

        call NUOPC_DriverAddComp(driver, name, SetServices, comp = wrapper_gc, &
                petlist = pet_list, rc = rc)
        VERIFY_NUOPC_(rc)

        allocate(wrapper%ptr)
        wrapper%ptr = cap_parameters(name, cap_rc_file, root_set_services)

        call ESMF_UserCompSetInternalState(wrapper_gc, internal_parameters_name, wrapper, rc)
        VERIFY_NUOPC_(rc)

        _RETURN(_SUCCESS)
    end subroutine add_wrapper_comp

    subroutine init_wrapper(wrapper_gc, name, cap_rc_file, root_set_services, rc)
        type(ESMF_GridComp), intent(inout) :: wrapper_gc
        character(*),        intent(in   ) :: name, cap_rc_file
        procedure(set_services_interface)  :: root_set_services
        integer, intent(out)               :: rc

        type(cap_parameters_wrapper) :: wrapper

        allocate(wrapper%ptr)
        wrapper%ptr = cap_parameters(name, cap_rc_file, root_set_services)

        call ESMF_UserCompSetInternalState(wrapper_gc, internal_parameters_name, wrapper, rc)
        VERIFY_NUOPC_(rc)

        _RETURN(_SUCCESS)
    end subroutine init_wrapper
end module MAPL_NUOPCWrapperMod
