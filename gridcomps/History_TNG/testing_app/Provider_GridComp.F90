#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"

module Provider_GridCompMod
    use ESMF
    use MAPL

    implicit none
    private

    public SetServices

    include "mpif.h"
contains
    subroutine SetServices(gc,rc)
        type(ESMF_GridComp), intent(INOUT) :: gc
        integer,             intent(  OUT) :: rc

        character(len=ESMF_MAXSTR) :: comp_name

        __Iam__('SetServices')
        call ESMF_GridCompGet(gc, name=comp_name, __RC__)
        Iam = trim(comp_name) //'::'// Iam

        print*, "Provider start SetServices"
        call ESMF_LogWrite("Provider start SetServices", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)

        print*, "Provider SetEntryPoints"
        call ESMF_LogWrite("Provider SetEntryPoints", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
        call MAPL_GridCompSetEntryPoint(gc, ESMF_METHOD_INITIALIZE, Initialize, __RC__)
        call MAPL_GridCompSetEntryPoint(gc, ESMF_METHOD_RUN, Run, __RC__)

        print*, "Provider add exports"
        call ESMF_LogWrite("Provider add exports", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
        call MAPL_AddExportSpec(GC,&
                short_name='var1', &
                long_name='var1' , &
                units = 'na', &
                dims = MAPL_DimsHorzOnly, &
                vlocation = MAPL_VLocationNone, __RC__)

        print*, "Provider Call Generic Set Services"
        call MAPL_GenericSetServices(gc, __RC__)

        print*, "Provider finish SetServices"
        call ESMF_LogWrite("Provider finish SetServices", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
        _RETURN(_SUCCESS)
    end subroutine SetServices

    subroutine Initialize(gc, import, export, clock, rc)
        type(ESMF_GridComp), intent(inout) :: gc
        type(ESMF_State),    intent(inout) :: import
        type(ESMF_State),    intent(inout) :: export
        type(ESMF_Clock),    intent(inout) :: clock
        integer, optional,   intent(  out) :: rc

        character(len=ESMF_MAXSTR) :: comp_name

        __Iam__('Initialize')
        call ESMF_GridCompGet(gc, name=comp_name, __RC__)
        Iam = trim(comp_name) //'::'// Iam

        print*, "Provider start Initialize"
        call ESMF_LogWrite("Provider start Initialize", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)

        print*, "Provider run MAPL_GridCreate"
        call ESMF_LogWrite("Provider run MAPL_GridCreate", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
        call MAPL_GridCreate(gc, __RC__)

        print*, "Provider run GenericInitialize"
        call ESMF_LogWrite("Provider run GenericInitialize", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
        call MAPL_GenericInitialize(gc, import, export, clock, __RC__)
        print*, "Provider run ForceAllocation"
        call ESMF_LogWrite("Provider run ForceAllocation", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
        call ForceAllocation(export, __RC__)

        print*, "Provider finish Initialize"
        call ESMF_LogWrite("Provider finish Initialize", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
        _RETURN(_SUCCESS)
    end subroutine Initialize

    subroutine Run(gc, import, export, clock, rc)
        type(ESMF_GridComp), intent(inout) :: gc
        type(ESMF_State),    intent(inout) :: import
        type(ESMF_State),    intent(inout) :: export
        type(ESMF_Clock),    intent(inout) :: clock
        integer, optional,   intent(  out) :: rc

        character(len=ESMF_MAXSTR) :: comp_name
        real, pointer              :: ptr2d(:,:)

        integer :: num_import, num_export
        character(len=ESMF_MAXSTR), allocatable :: names_import(:), names_export(:)
        character(len=ESMF_MAXSTR) :: name

        type(ESMF_Field) :: field
        type(ESMF_Grid)  :: grid

        __Iam__('Run')
        call ESMF_GridCompGet(gc, name=comp_name, __RC__)
        Iam = trim(comp_name) //'::'// Iam

        print*, "Provider start Run"

        print*,"Provider set export value"
        call MAPL_GetPointer(export, ptr2d, 'var1', __RC__)
        ptr2d = ptr2d + 1.0
        print*, "The value var1 is set to is:", minval(ptr2d), maxval(ptr2d)

        print*,"Provider get number of imports"
        call ESMF_StateGet(import, itemcount=num_import, rc=rc)
        VERIFY_NUOPC_(rc)
        print*, "Provider num import:", num_import

        allocate(names_import(num_import))
        print*,"Provider get import names"
        call ESMF_StateGet(import, itemnamelist=names_import, rc=rc)
        VERIFY_NUOPC_(rc)
        print*,"Provider import names are:", names_import

        print*,"Provider get number of exports"
        call ESMF_StateGet(export, itemcount=num_export, rc=rc)
        VERIFY_NUOPC_(rc)
        print*, "Provider num export:", num_export

        allocate(names_export(num_export))
        print*,"Provider get export names"
        call ESMF_StateGet(export, itemnamelist=names_export, rc=rc)
        VERIFY_NUOPC_(rc)
        print*,"Provider export names are:", names_export

        call ESMF_StateGet(export, field=field, itemName="var1", rc=rc)
        VERIFY_NUOPC_(rc)

        call ESMF_FieldGet(field, grid=grid, rc=rc)
        VERIFY_NUOPC_(rc)

        call ESMF_GridGet(grid, name=name, rc=rc)
        print*,"Var1 grid name:", name

        print*, "Provider finish Run"
        _RETURN(_SUCCESS)
    end subroutine Run

    subroutine ForceAllocation(state, rc)
        type(ESMF_State),  intent(inout) :: state
        integer, optional, intent(  out) :: rc

        integer                                 :: itemCount, i, dims
        character(len=ESMF_MAXSTR), allocatable :: itemNameList(:)
        type(ESMF_StateItem_FLAG),  allocatable :: itemTypeList(:)

        type(ESMF_Field) :: field
        real, pointer    :: ptr2d(:,:), ptr3d(:,:,:)

        __Iam__('ForceAllocation')

        print*, "Provider start ForceAllocation"
        call ESMF_LogWrite("Provider start ForceAllocation", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)

        call ESMF_StateGet(state, itemCount=itemCount, __RC__)
        allocate(itemNameList(itemCount), stat=status)
        VERIFY_(status)
        allocate(itemTypeList(itemCount), stat=status)
        VERIFY_(status)

        call ESMF_StateGet(state, itemNameList=itemNameList, &
                itemTypeList=itemTypeList, __RC__)

        if (itemCount /= 0) then
            do i=1, itemCount
                if (itemTypeList(i) == ESMF_STATEITEM_FIELD) then
                    call ESMF_StateGet(state, trim(itemNameList(i)), field, __RC__)
                    call ESMF_AttributeGet(field, name='DIMS', value=dims, __RC__)
                    if (dims == MAPL_DimsHorzOnly) then
                        call MAPL_GetPointer(state, ptr2d, trim(itemNameList(i)), &
                                alloc=.true., __RC__)
                    else if (dims == MAPL_DimsHorzVert) then
                        call MAPL_GetPointer(state, ptr3d, trim(itemNameList(i)), &
                                alloc=.true., __RC__)
                    else
                        _ASSERT((1 == 2), "invalid field for test defined")
                    end if
                end if
            end do
        end if

        print*, "Provider finish ForceAllocation"
        call ESMF_LogWrite("Provider finish ForceAllocation", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)

        _RETURN(_SUCCESS)
    end subroutine ForceAllocation
end module Provider_GridCompMod

