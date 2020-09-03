#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"

module NUOPC_MAPLcapClass
    use ESMF
    use NUOPC
    use NUOPC_Model
    use MAPL_Mod
    use MAPL_CapGridCompMod
    use MAPL_Profiler, only: BaseProfiler, get_global_time_profiler

    use NUOPCmapMod

    implicit none
    private

    public NUOPC_MAPLcap
    public abs_set_services

    type :: NUOPC_MAPLcap
        character(len=:), allocatable                :: name
        character(len=:), allocatable                :: rc_file
        procedure(abs_set_services), nopass, pointer :: set_services
        type(MAPL_Cap), pointer                      :: cap => null()
        type(NUOPCmap), pointer                      :: phase_map => null()
    contains
        procedure :: init_MAPL_cap
        procedure :: create_phase_map
        procedure :: init_MAPL_comm
        procedure :: init_MAPL
        procedure :: generic_init
        procedure :: init_p0
        procedure :: init_p1
        procedure :: init_p2
        procedure :: init_p3
        procedure :: init_p4
        procedure :: init_p5
        procedure :: init_p6
        procedure :: init_p7
        procedure :: advertise_fields
        procedure :: realize_fields
        procedure :: data_init
        procedure :: advance
        procedure :: check_import
        procedure :: set_clock
        procedure :: finalize
    end type NUOPC_MAPLcap

    type :: FieldAttributes
        type(ESMF_Field)              :: field
        character(len=:), allocatable :: name
        character(len=:), allocatable :: long_name
        character(len=:), allocatable :: units
    contains
        procedure :: advertise_to_state
        procedure :: realize_to_import_state
        procedure :: realize_to_export_state
    end type FieldAttributes

    abstract interface
        subroutine abs_set_services(gc, rc)
            import ESMF_GridComp
            type(ESMF_GridComp), intent(inout) :: gc
            integer,             intent(  out) :: rc
        end subroutine abs_set_services
    end interface

#include "mpif.h"

contains
    subroutine init_MAPL_cap(this, model, rc)
        class(NUOPC_MAPLcap), intent(inout) :: this
        type(ESMF_GridComp)                 :: model
        integer, optional,    intent(  out) :: rc

        type(ESMF_VM)           :: vm
        type(MAPL_CapOptions)   :: cap_options
        type(MAPL_Cap), pointer :: cap
        logical, save           :: first = .true.
        integer                 :: status, mpi_comm, dup_comm, sub_comm

        print*, "NUOPC_MAPLcapClass start init_MAPL_cap"
        call ESMF_LogWrite("NUOPC_MAPLcapClass start init_MAPL_cap", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)

        ! Read ESMF VM information
        print*, "NUOPC_MAPLcapClass get VM/comm info"
        call ESMF_LogWrite("NUOPC_MAPLcapClass get VM/comm info", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
        call ESMF_GridCompGet(model, vm=vm, __RC__)
        call ESMF_VMGet(vm, mpiCommunicator=mpi_comm, __RC__)
        call MPI_Comm_dup(mpi_comm, dup_comm, status)
        _VERIFY(status)

        ! Setup MAPL_CapOptions
        print*, "NUOPC_MAPLcapClass set cap_options"
        call ESMF_LogWrite("NUOPC_MAPLcapClass set cap_options", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
        cap_options                = MAPL_CapOptions(cap_rc_file=this%rc_file, __RC__)
        cap_options%use_comm_world = .false.
        cap_options%comm           = dup_comm
        cap_options%logging_config = ''
        call MPI_Comm_size(dup_comm, cap_options%npes_model, status)
        _VERIFY(status)

        ! Create MAPL Cap
        print*, "NUOPC_MAPLcapClass create cap"
        call ESMF_LogWrite("NUOPC_MAPLcapClass create cap", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
        allocate(cap)
        cap = MAPL_Cap(this%name, this%set_services, cap_options=cap_options, __RC__)
        print*, "NUOPC_MAPLcapClass assign cap"
        call ESMF_LogWrite("NUOPC_MAPLcapClass assign cap", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
        this%cap => cap

        print*, "NUOPC_MAPLcapClass finish init_MAPL_cap"
        call ESMF_LogWrite("NUOPC_MAPLcapClass finish init_MAPL_cap", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)

        _RETURN(_SUCCESS)
    end subroutine init_MAPL_cap

    subroutine create_phase_map(this, model, rc)
        class(NUOPC_MAPLcap), intent(inout) :: this
        type(ESMF_GridComp)                 :: model
        integer,              intent(  out) :: rc

        type(NUOPCmap), pointer :: phase_map

        rc = ESMF_SUCCESS

        print*, "NUOPC_MAPLcapClass start create_phase_map"
        call ESMF_LogWrite("NUOPC_MAPLcapClass start create_phase_map", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)

        allocate(phase_map)
        call phase_map%create_phase_map(model, rc)
        VERIFY_NUOPC_(rc)

        print*, "NUOPC_MAPLcapClass assign phase_map"
        call ESMF_LogWrite("NUOPC_MAPLcapClass assign phase_map", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
        this%phase_map => phase_map

        print*, "NUOPC_MAPLcapClass finish create_phase_map"
        call ESMF_LogWrite("NUOPC_MAPLcapClass finish create_phase_map", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
    end subroutine create_phase_map

    subroutine init_MAPL_comm(this, rc)
        class(NUOPC_MAPLcap), intent(inout) :: this
        integer, optional,    intent(  out) :: rc

        logical, save :: first = .true.
        integer       :: status, sub_comm

        print*, "NUOPC_MAPLcapClass start init_MAPL_comm"
        call ESMF_LogWrite("NUOPC_MAPLcapClass start init_MAPL_comm", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)

        print*, "NUOPC_MAPLcapClass cap initialize MPI"
        call ESMF_LogWrite("NUOPC_MAPLcapClass cap initialize MPI", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
        call this%cap%initialize_mpi(__RC__)
        if (first) then
            print*, "NUOPC_MAPLcapClass create sub_comm"
            call ESMF_LogWrite("NUOPC_MAPLcapClass create sub_comm", ESMF_LOGMSG_INFO, rc=rc)
            VERIFY_ESMF_(rc)
            sub_comm = this%cap%create_member_subcommunicator(this%cap%get_comm_world(),__RC__)
            print*, "NUOPC_MAPLcapClass initialize io_clients_servers"
            call ESMF_LogWrite("NUOPC_MAPLcapClass initialize io_clients_servers", ESMF_LOGMSG_INFO, rc=rc)
            VERIFY_ESMF_(rc)
            call this%cap%initialize_io_clients_servers(sub_comm, __RC__)
            first = .false.
        end if
        print*, "NUOPC_MAPLcapClass nuopc_fill_mapl_comm"
        call ESMF_LogWrite("NUOPC_MAPLcapClass nuopc_fill_mapl_comm", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
        call this%cap%nuopc_fill_mapl_comm(__RC__)

        print*, "NUOPC_MAPLcapClass finish init_MAPL_comm"
        call ESMF_LogWrite("NUOPC_MAPLcapClass finish init_MAPL_comm", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)

        _RETURN(_SUCCESS)
    end subroutine init_MAPL_comm

    subroutine init_MAPL(this, rc)
        class(NUOPC_MAPLcap), intent(inout) :: this
        integer, optional,    intent(  out) :: rc

        integer :: status

        print*, "NUOPC_MAPLcapClass start init_MAPL"
        call ESMF_LogWrite("NUOPC_MAPLcapClass start init_MAPL", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)

        ! Initialize MAPL
        call this%cap%initialize_cap_gc(this%cap%get_mapl_comm())
        call this%cap%cap_gc%set_services(__RC__)
        call this%cap%cap_gc%initialize(__RC__)

        print*, "NUOPC_MAPLcapClass finish init_MAPL"
        call ESMF_LogWrite("NUOPC_MAPLcapClass finish init_MAPL", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)

        _RETURN(_SUCCESS)
    end subroutine init_MAPL

    subroutine init_p0(this, model, import_state, export_state, clock, rc)
        class(NUOPC_MAPLcap), intent(inout) :: this
        type(ESMF_GridComp)                 :: model
        type(ESMF_State)                    :: import_state
        type(ESMF_State)                    :: export_state
        type(ESMF_Clock)                    :: clock
        integer,              intent(  out) :: rc

        integer :: status

        rc = ESMF_SUCCESS

        print*, "NUOPC_MAPLcapClass start init_p0"
        call ESMF_LogWrite("NUOPC_MAPLcapClass start init_p0", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)

        call NUOPC_CompFilterPhaseMap(model, ESMF_METHOD_INITIALIZE, &
                acceptStringList=["IPDv05p"], rc=rc)
        VERIFY_NUOPC_(rc)

        call this%init_MAPL_cap(model, __RC__)
        call this%init_MAPL_comm(__RC__)
        call this%init_MAPL(__RC__)

        call this%create_phase_map(model, rc)
        VERIFY_NUOPC_(rc)

        print*, "NUOPC_MAPLcapClass finish init_p0"
        call ESMF_LogWrite("NUOPC_MAPLcapClass finish init_p0", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
    end subroutine init_p0

    subroutine generic_init(this, model, import_state, export_state, clock, rc)
        class(NUOPC_MAPLcap), intent(inout) :: this
        type(ESMF_GridComp)                 :: model
        type(ESMF_State)                    :: import_state
        type(ESMF_State)                    :: export_state
        type(ESMF_Clock)                    :: clock
        integer,              intent(  out) :: rc

        character(len=:), pointer :: phase_label
        integer                   :: current_phase_index

        rc = ESMF_SUCCESS

        print*, "NUOPC_MAPLcapClass start generic_init"
        call ESMF_LogWrite("NUOPC_MAPLcapClass start generic_init", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)

        call ESMF_GridCompGet(model, currentPhase=current_phase_index, rc=rc)
        VERIFY_NUOPC_(rc)

        call this%phase_map%get_phase(current_phase_index, phase_label, rc)
        VERIFY_NUOPC_(rc)

        select case(phase_label)
        case (phase_label_list(1))
            call this%init_p1(import_state, export_state, clock, rc)
            VERIFY_NUOPC_(rc)

        case (phase_label_list(2))
            call this%init_p2(import_state, export_state, clock, rc)
            VERIFY_NUOPC_(rc)

        case (phase_label_list(3))
            call this%init_p3(import_state, export_state, clock, rc)
            VERIFY_NUOPC_(rc)

        case (phase_label_list(4))
            call this%init_p4(import_state, export_state, clock, rc)
            VERIFY_NUOPC_(rc)

        case (phase_label_list(5))
            call this%init_p5(import_state, export_state, clock, rc)
            VERIFY_NUOPC_(rc)

        case (phase_label_list(6))
            call this%init_p6(import_state, export_state, clock, rc)
            VERIFY_NUOPC_(rc)
        end select

        print*, "NUOPC_MAPLcapClass finish generic_init"
        call ESMF_LogWrite("NUOPC_MAPLcapClass finish generic_init", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
    end subroutine generic_init

    subroutine init_p1(this, import_state, export_state, clock, rc)
        class(NUOPC_MAPLcap), intent(inout) :: this
        type(ESMF_State)                    :: import_state
        type(ESMF_State)                    :: export_state
        type(ESMF_Clock)                    :: clock
        integer,              intent(  out) :: rc

        rc = ESMF_SUCCESS

        print*, "NUOPC_MAPLcapClass start init_p1"
        call ESMF_LogWrite("NUOPC_MAPLcapClass start init_p1", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)

        call this%advertise_fields(import_state, export_state, rc=rc)
        VERIFY_NUOPC_(rc)

        print*, "NUOPC_MAPLcapClass finish init_p1"
        call ESMF_LogWrite("NUOPC_MAPLcapClass finish init_p1", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
    end subroutine init_p1

    subroutine init_p2(this, import_state, export_state, clock, rc)
        class(NUOPC_MAPLcap), intent(inout) :: this
        type(ESMF_State)                    :: import_state
        type(ESMF_State)                    :: export_state
        type(ESMF_Clock)                    :: clock
        integer,              intent(  out) :: rc

        rc = ESMF_SUCCESS

        print*, "NUOPC_MAPLcapClass init_p2"
        call ESMF_LogWrite("NUOPC_MAPLcapClass init_p2", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
    end subroutine init_p2

    subroutine init_p3(this, import_state, export_state, clock, rc)
        class(NUOPC_MAPLcap), intent(inout) :: this
        type(ESMF_State)                    :: import_state
        type(ESMF_State)                    :: export_state
        type(ESMF_Clock)                    :: clock
        integer,              intent(  out) :: rc

        rc = ESMF_SUCCESS

        print*, "NUOPC_MAPLcapClass init_p3"
        call ESMF_LogWrite("NUOPC_MAPLcapClass init_p3", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
    end subroutine init_p3

    subroutine init_p4(this, import_state, export_state, clock, rc)
        class(NUOPC_MAPLcap), intent(inout) :: this
        type(ESMF_State)                    :: import_state
        type(ESMF_State)                    :: export_state
        type(ESMF_Clock)                    :: clock
        integer,              intent(  out) :: rc

        rc = ESMF_SUCCESS

        print*, "NUOPC_MAPLcapClass start init_p4"
        call ESMF_LogWrite("NUOPC_MAPLcapClass start init_p4", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)

        call this%realize_fields(import_state, export_state, rc=rc)
        VERIFY_NUOPC_(rc)

        print*, "NUOPC_MAPLcapClass finish init_p4"
        call ESMF_LogWrite("NUOPC_MAPLcapClass finish init_p4", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
    end subroutine init_p4

    subroutine init_p5(this, import_state, export_state, clock, rc)
        class(NUOPC_MAPLcap), intent(inout) :: this
        type(ESMF_State)                    :: import_state
        type(ESMF_State)                    :: export_state
        type(ESMF_Clock)                    :: clock
        integer,              intent(  out) :: rc

        rc = ESMF_SUCCESS

        print*, "NUOPC_MAPLcapClass init_p5"
        call ESMF_LogWrite("NUOPC_MAPLcapClass init_p5", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
    end subroutine init_p5

    subroutine init_p6(this, import_state, export_state, clock, rc)
        class(NUOPC_MAPLcap), intent(inout) :: this
        type(ESMF_State)                    :: import_state
        type(ESMF_State)                    :: export_state
        type(ESMF_Clock)                    :: clock
        integer,              intent(  out) :: rc

        rc = ESMF_SUCCESS

        print*, "NUOPC_MAPLcapClass init_p6"
        call ESMF_LogWrite("NUOPC_MAPLcapClass init_p6", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
    end subroutine init_p6

    subroutine init_p7(this, import_state, export_state, clock, rc)
        class(NUOPC_MAPLcap), intent(inout) :: this
        type(ESMF_State)                    :: import_state
        type(ESMF_State)                    :: export_state
        type(ESMF_Clock)                    :: clock
        integer,              intent(  out) :: rc

        rc = ESMF_SUCCESS

        print*, "NUOPC_MAPLcapClass init_p7"
        call ESMF_LogWrite("NUOPC_MAPLcapClass init_p7", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
    end subroutine init_p7

    subroutine advertise_fields(this, import_state, export_state, rc)
        class(NUOPC_MAPLcap), intent(inout) :: this
        type(ESMF_State)                    :: import_state
        type(ESMF_State)                    :: export_state
        integer,              intent(  out) :: rc

        type(MAPL_Cap)   :: cap
        type(MAPL_CapGridComp) :: cap_gc
        type(ESMF_State) :: mapl_import_state
        type(FieldAttributes), allocatable :: import_attributes(:)
        type(FieldAttributes), allocatable :: export_attributes(:)

        rc = ESMF_SUCCESS

        print*, "NUOPC_MAPLcapClass start advertise_fields"
        call ESMF_LogWrite("NUOPC_MAPLcapClass start advertise_fields", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)

        print*, "NUOPC_MAPLcapClass get the mapl_cap"
        cap = this%cap

        print*, "NUOPC_MAPLcapClass get the mapl_cap_gc"
        cap_gc = this%cap%cap_gc

        print*, "NUOPC_MAPLcapClass get import state"
        mapl_import_state = cap_gc%import_state

        print*, "NUOPC_MAPLcapClass get import attributes"
        import_attributes = field_attributes_from_state(mapl_import_state, rc)
        VERIFY_NUOPC_(rc)
        export_attributes = field_attributes_from_state(this%cap%cap_gc%export_state, rc)
        VERIFY_NUOPC_(rc)

        call advertise(import_state, import_attributes, rc)
        VERIFY_NUOPC_(rc)
        call advertise(export_state, export_attributes, rc)
        VERIFY_NUOPC_(rc)

        print*, "NUOPC_MAPLcapClass finish advertise_fields"
        call ESMF_LogWrite("NUOPC_MAPLcapClass finish advertise_fields", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
    contains
        subroutine advertise(state, fields, rc)
            type(ESMF_State),      intent(inout) :: state
            type(FieldAttributes), intent(inout) :: fields(:)
            integer,               intent(  out) :: rc

            integer :: i

            rc = ESMF_SUCCESS

            do i=1, size(fields)
                call fields(i)%advertise_to_state(state, rc)
                VERIFY_NUOPC_(rc)
            end do
        end subroutine advertise
    end subroutine advertise_fields

    subroutine advertise_to_state(this, state, rc)
        class(FieldAttributes), intent(inout) :: this
        type(ESMF_State),       intent(inout) :: state
        integer,                intent(  out) :: rc

        rc = ESMF_SUCCESS

        print*, "NUOPC_MAPLcapClass start advertise_to_state"
        call ESMF_LogWrite("NUOPC_MAPLcapClass start advertise_to_state", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)

        if (.not. NUOPC_FieldDictionaryHasEntry(this%name)) then
            call NUOPC_FieldDictionaryAddEntry(standardName=trim(this%name), &
                    canonicalUnits=trim(this%units), rc=rc)
            VERIFY_NUOPC_(rc)
        end if

        call NUOPC_Advertise(state, standardName=trim(this%name), &
                TransferOfferGeomObject="will provide", rc=rc)
        VERIFY_NUOPC_(rc)

        print*, "NUOPC_MAPLcapClass finish advertise_to_state"
        call ESMF_LogWrite("NUOPC_MAPLcapClass finish advertise_to_state", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
    end subroutine advertise_to_state

    subroutine realize_fields(this, import_state, export_state, rc)
        class(NUOPC_MAPLcap), intent(inout) :: this
        type(ESMF_State)                    :: import_state
        type(ESMF_State)                    :: export_state
        integer,              intent(  out) :: rc

        type(FieldAttributes), allocatable :: import_attributes(:)
        type(FieldAttributes), allocatable :: export_attributes(:)
        integer                            :: i

        rc = ESMF_SUCCESS

        print*, "NUOPC_MAPLcapClass start realize_fields"
        call ESMF_LogWrite("NUOPC_MAPLcapClass start realize_fields", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)

        import_attributes = field_attributes_from_state(this%cap%cap_gc%import_state, rc)
        VERIFY_NUOPC_(rc)
        export_attributes = field_attributes_from_state(this%cap%cap_gc%export_state, rc)
        VERIFY_NUOPC_(rc)

        do i=1, size(import_attributes)
            call import_attributes(i)%realize_to_import_state(import_state, rc)
            VERIFY_NUOPC_(rc)
        end do

        do i=1, size(export_attributes)
            call export_attributes(i)%realize_to_export_state(export_state, rc)
            VERIFY_NUOPC_(rc)
        end do

        print*, "NUOPC_MAPLcapClass finish realize_fields"
        call ESMF_LogWrite("NUOPC_MAPLcapClass finish realize_fields", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
    end subroutine realize_fields

    subroutine realize_to_import_state(this, state, rc)
        class(FieldAttributes), intent(inout) :: this
        type(ESMF_State),       intent(inout) :: state
        integer,                intent(  out) :: rc

        rc = ESMF_SUCCESS

        print*, "NUOPC_MAPLcapClass start realize_to_import_state"
        call ESMF_LogWrite("NUOPC_MAPLcapClass start realize_to_import_state", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)

        call ESMF_FieldValidate(this%field, rc=rc)
        VERIFY_NUOPC_(rc)
        call NUOPC_Realize(state, field=this%field, rc=rc)
        VERIFY_NUOPC_(rc)

        print*, "NUOPC_MAPLcapClass finish realize_to_import_state"
        call ESMF_LogWrite("NUOPC_MAPLcapClass finish realize_to_import_state", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
    end subroutine realize_to_import_state

    subroutine realize_to_export_state(this, state, rc)
        class(FieldAttributes), intent(inout) :: this
        type(ESMF_State),       intent(inout) :: state
        integer,                intent(  out) :: rc

        rc = ESMF_SUCCESS

        print*, "NUOPC_MAPLcapClass start realize_to_export_state"
        call ESMF_LogWrite("NUOPC_MAPLcapClass start realize_to_export_state", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)

        call MAPL_AllocateCoupling(this%field, rc=rc)
        VERIFY_NUOPC_(rc)
        call NUOPC_Realize(state, field=this%field, rc=rc)
        VERIFY_NUOPC_(rc)

        print*, "NUOPC_MAPLcapClass finish realize_to_export_state"
        call ESMF_LogWrite("NUOPC_MAPLcapClass finish realize_to_export_state", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
    end subroutine realize_to_export_state

    subroutine data_init(this, model, rc)
        class(NUOPC_MAPLcap), intent(inout) :: this
        type(ESMF_GridComp)                 :: model
        integer,              intent(  out) :: rc

        rc = ESMF_SUCCESS

        print*, "NUOPC_MAPLcapClass start data_init"
        call ESMF_LogWrite("NUOPC_MAPLcapClass start data_init", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)

        call NUOPC_CompAttributeSet(model, &
                name="InitializeDataComplete", value="true", rc=rc)
        VERIFY_NUOPC_(rc)

        print*, "NUOPC_MAPLcapClass finish data_init"
        call ESMF_LogWrite("NUOPC_MAPLcapClass finish data_init", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
    end subroutine data_init

    subroutine advance(this, rc)
        class(NUOPC_MAPLcap), intent(inout) :: this
        integer,              intent(  out) :: rc

        rc = ESMF_SUCCESS

        print*, "NUOPC_MAPLcapClass start advance"
        call ESMF_LogWrite("NUOPC_MAPLcapClass start advance", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)

        call this%cap%step_model(rc=rc)
        VERIFY_NUOPC_(rc)

        print*, "NUOPC_MAPLcapClass finish advance"
        call ESMF_LogWrite("NUOPC_MAPLcapClass finish advance", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
    end subroutine advance

    subroutine check_import(this, rc)
        class(NUOPC_MAPLcap), intent(inout) :: this
        integer,              intent(  out) :: rc

        rc = ESMF_SUCCESS

        print*, "NUOPC_MAPLcapClass check_import"
        call ESMF_LogWrite("NUOPC_MAPLcapClass check_import", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
    end subroutine check_import

    subroutine set_clock(this, model, rc)
        class(NUOPC_MAPLcap), intent(inout) :: this
        type(ESMF_GridComp)                 :: model
        integer,              intent(  out) :: rc

        type(ESMF_TimeInterval) :: time_step
        type(ESMF_Clock)        :: model_clock
        integer                 :: heartbeat_dt

        rc = ESMF_SUCCESS

        print*, "NUOPC_MAPLcapClass start set_clock"
        call ESMF_LogWrite("NUOPC_MAPLcapClass start set_clock", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)

        ! set time interval
        heartbeat_dt = this%cap%cap_gc%get_heartbeat_dt()
        call ESMF_TimeIntervalSet(time_step, s=heartbeat_dt, rc=rc)
        VERIFY_NUOPC_(rc)

        ! set clock with time interval
        call NUOPC_ModelGet(model, modelClock=model_clock, rc=rc)
        VERIFY_NUOPC_(rc)
        call ESMF_ClockSet(model_clock, timeStep=time_step, rc=rc)
        VERIFY_NUOPC_(rc)

        print*, "NUOPC_MAPLcapClass finish set_clock"
        call ESMF_LogWrite("NUOPC_MAPLcapClass finish set_clock", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
    end subroutine set_clock

    subroutine finalize(this, rc)
        class(NUOPC_MAPLcap), intent(inout) :: this
        integer,              intent(  out) :: rc

        class(BaseProfiler), pointer :: t_p

        rc = ESMF_SUCCESS

        print*, "NUOPC_MAPLcapClass start finialize"
        call ESMF_LogWrite("NUOPC_MAPLcapClass start finialize", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)

        call this%cap%cap_gc%finalize(rc=rc)
        VERIFY_NUOPC_(rc)

        call i_Clients%terminate()
        call o_Clients%terminate()

        call  this%cap%finalize_io_clients_servers(rc=rc)
        VERIFY_NUOPC_(rc)

        t_p => get_global_time_profiler()
        call t_p%stop()

        print*, "NUOPC_MAPLcapClass finish finialize"
        call ESMF_LogWrite("NUOPC_MAPLcapClass finish finialize", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
    end subroutine finalize

    function field_attributes_from_state(state, rc) result(field_attributes)
        type(ESMF_State),    intent(inout) :: state
        integer,             intent(  out) :: rc
        type(FieldAttributes), allocatable :: field_attributes(:)

        type(ESMF_Field)                        :: field
        character(len=ESMF_MAXSTR), allocatable :: item_names(:)
        character(len=ESMF_MAXSTR)              :: name, long_name, units
        integer                                 :: i, item_count

        rc = ESMF_SUCCESS

        print*, "NUOPC_MAPLcapClass start field_attributes_from_state"
        call ESMF_LogWrite("NUOPC_MAPLcapClass start field_attributes_from_state", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)

        ! Allocate lists and get item_names
        print*, "NUOPC_MAPLcapClass read item_count from state"
        call ESMF_LogWrite("NUOPC_MAPLcapClass read item_count from state", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
        call ESMF_StateGet(state, itemCount=item_count, rc=rc)
        VERIFY_NUOPC_(rc)
        allocate(item_names(item_count))
        allocate(field_attributes(item_count))
        print*, "NUOPC_MAPLcapClass read item_names from state"
        call ESMF_LogWrite("NUOPC_MAPLcapClass read item_names from state", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
        call ESMF_StateGet(state, itemNameList=item_names, rc=rc)
        VERIFY_NUOPC_(rc)

        print*, "NUOPC_MAPLcapClass start loop over item_names"
        call ESMF_LogWrite("NUOPC_MAPLcapClass start loop over item_names", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
        do i=1, item_count
            ! Get a field from the state
            print*, "NUOPC_MAPLcapClass get field for item_name:", i, item_names(i)
            call ESMF_LogWrite("NUOPC_MAPLcapClass get field for item_name: "//item_names(i), ESMF_LOGMSG_INFO, rc=rc)
            VERIFY_ESMF_(rc)
            call ESMF_StateGet(state, item_names(i), field, rc=rc)
            VERIFY_NUOPC_(rc)
            print*, "NUOPC_MAPLcapClass validate get field"
            call ESMF_LogWrite("NUOPC_MAPLcapClass validate the field", ESMF_LOGMSG_INFO, rc=rc)
            VERIFY_ESMF_(rc)
            call ESMF_FieldValidate(field, rc=rc)
            VERIFY_NUOPC_(rc)

            ! Get the name of the field
            print*, "NUOPC_MAPLcapClass read name from field"
            call ESMF_LogWrite("NUOPC_MAPLcapClass read name from field", ESMF_LOGMSG_INFO, rc=rc)
            VERIFY_ESMF_(rc)
            call ESMF_FieldGet(field, name=name, rc=rc)
            VERIFY_NUOPC_(rc)

            ! Get the LONG_NAME of the field
            print*, "NUOPC_MAPLcapClass read LONG_NAME from field"
            call ESMF_LogWrite("NUOPC_MAPLcapClass read LONG_NAME from field", ESMF_LOGMSG_INFO, rc=rc)
            VERIFY_ESMF_(rc)
            call ESMF_AttributeGet(field, name="LONG_NAME", value=long_name, rc=rc)
            VERIFY_NUOPC_(rc)

            ! Get the UNITS of the field
            print*, "NUOPC_MAPLcapClass read UNITS from field"
            call ESMF_LogWrite("NUOPC_MAPLcapClass read UNITS from field", ESMF_LOGMSG_INFO, rc=rc)
            VERIFY_ESMF_(rc)
            call ESMF_AttributeGet(field, name="UNITS", value=units, rc=rc)
            VERIFY_NUOPC_(rc)
            if (units == "" .or. units == " ") units = "1"

            ! Create Field Attributes
            print*, "NUOPC_MAPLcapClass create FieldAttributes for field"
            call ESMF_LogWrite("NUOPC_MAPLcapClass create FieldAttributes for field", ESMF_LOGMSG_INFO, rc=rc)
            VERIFY_ESMF_(rc)
            field_attributes(i) = FieldAttributes(field, name, long_name, units)
        end do

        print*, "NUOPC_MAPLcapClass finish field_attributes_from_state"
        call ESMF_LogWrite("NUOPC_MAPLcapClass finish field_attributes_from_state", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
    end function field_attributes_from_state
end module NUOPC_MAPLcapClass
