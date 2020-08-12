#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"

module NUOPCmapMod
    use ESMF
    use NUOPC
    use MAPL_Mod
    use gFTL_IntegerStringMap

    implicit none
    private

    public num_phases
    public phase_label_list

    public NUOPCmap

    integer, parameter :: num_phases = 7
    character(len=*), dimension(num_phases), parameter :: &
            phase_label_list = [&
                "IPDv05p1", &
                "IPDv05p2", &
                "IPDv05p3", &
                "IPDv05p4", &
                "IPDv05p5", &
                "IPDv05p6", &
                "IPDv05p7"  &
            ]

    type, extends(IntegerStringMap) :: NUOPCmap
    contains
        procedure :: get_phase
        procedure :: add_phase
        procedure :: create_phase_map
    end type NUOPCmap

contains
    subroutine get_phase(this, index, phase_label, rc)
        class(NUOPCmap),           intent(inout) :: this
        integer,                   intent(in   ) :: index
        character(len=:), pointer, intent(  out) :: phase_label
        integer,                   intent(  out) :: rc

        rc = ESMF_SUCCESS

        print*, "NUOPCmap start get_phase"

        print*, "NUOPCmap getting phase for index: ", index
        phase_label => this%at(index)
        if (.not. associated(phase_label)) then
            rc = ESMF_RC_OBJ_BAD
            return
        end if

        print*, "NUOPCmap index: ", index, " found phase: ", phase_label

        print*, "NUOPCmap finish get_phase"
    end subroutine get_phase

    subroutine add_phase(this, index, phase_label, rc)
        class(NUOPCmap),  intent(inout) :: this
        integer,          intent(in   ) :: index
        character(len=*), intent(in   ) :: phase_label
        integer,          intent(  out) :: rc

        rc = ESMF_SUCCESS

        print*, "NUOPCmap start add_phase"

        if (this%count(index) > 0) then
            rc = ESMF_RC_OBJ_BAD
            return
        else
            print*, "NUOPCmap inserting phase: ", phase_label, " under index: ", index
            call this%insert(index, phase_label)
        end if

        print*, "NUOPCmap finish add_phase"
    end subroutine add_phase

    subroutine create_phase_map(this, gc, rc)
        class(NUOPCmap),     intent(inout) :: this
        type(ESMF_GridComp), intent(inout) :: gc
        integer,             intent(  out) :: rc

        integer :: i, phase_index

        rc = ESMF_SUCCESS

        print*, "NUOPCmap start create_phase_map"

        do i=1, num_phases
            print*, "NUOPCmap searching for phase index"
            call NUOPC_CompSearchPhaseMap(gc, ESMF_METHOD_INITIALIZE, &
                    phaseLabel=phase_label_list(i), phaseIndex=phase_index, rc=rc)
            VERIFY_NUOPC_(rc)

            print*, "NUOPCmap found phase: ", phase_label_list(i), " has index: ", phase_index

            if (phase_index >= 0) then
                call this%add_phase(phase_index, phase_label_list(i), rc=rc)
                VERIFY_NUOPC_(rc)
            else
                rc = ESMF_RC_OBJ_BAD
                return
            end if
        end do

        print*, "NUOPCmap finish create_phase_map"
    end subroutine create_phase_map
end module NUOPCmapMod
