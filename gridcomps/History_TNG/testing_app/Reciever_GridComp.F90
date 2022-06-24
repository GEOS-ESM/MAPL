#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"

module Reciever_GridCompMod
    use ESMF
    use MAPL

    implicit none
    private

    public SetServices

    include "mpif.h"
contains
    subroutine SetServices(gc, rc)
        type(ESMF_GridComp), intent(inout) :: gc
        integer,             intent(  out) :: rc

        character(len=ESMF_MAXSTR) :: comp_name

        __Iam__('SetServices')
        call ESMF_GridCompGet(gc, name=comp_name, __RC__)
        Iam = trim(comp_name) //'::'// Iam

        print*, "Reciever start Set Services"

        print*, "Reciever Entry Points"
        call MAPL_GridCompSetEntryPoint(gc, ESMF_METHOD_INITIALIZE, Initialize, __RC__)
        call MAPL_GridCompSetEntryPoint(gc, ESMF_METHOD_RUN, Run, __RC__)

        print*, "Reciever set import"
        call MAPL_AddImportSpec(gc, &
                short_name='var1', &
                long_name='var1', &
                units='na', &
                dims=MAPL_DimsHorzOnly, &
                vlocation=MAPL_VLocationNone, __RC__)
!        call MAPL_AddImportSpec(gc, &
!                short_name='var2', &
!                long_name='var2', &
!                units='na', &
!                dims=MAPL_DimsHorzOnly, &
!                vlocation=MAPL_VLocationNone, __RC__)

        print*, "Reciever Call Generic Set Services"
        call MAPL_GenericSetServices(gc, __RC__)

        print*, "Reciever finish Set Services"
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

        print*, "Reciever start Initialize"

        print*, "Provider MAPL_GridCreate"
        call MAPL_GridCreate(gc, __RC__)

        print*, "Reciever Generic initialize"
        call MAPL_GenericInitialize(gc, import, export, clock, __RC__)

        print*, "Reciever Force allocate"
        call ForceAllocation(import, __RC__)

        print*, "Reciever finish Initialize"
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

        __Iam__('Run')
        call ESMF_GridCompGet(gc, name=comp_name, __RC__)
        Iam = trim(comp_name) //'::'// Iam

        print*, "Reciever start Run"

        print*,"Reciever get import value"
        call MAPL_GetPointer(import, ptr2d, 'var1', __RC__)
        print*, 'The value of var1 is:', minval(ptr2d), maxval(ptr2d)

        print*,"Reciever get number of imports"
        call ESMF_StateGet(import, itemcount=num_import, rc=rc)
        VERIFY_NUOPC_(rc)
        print*, "Reciever num import:", num_import

        allocate(names_import(num_import))
        print*,"Reciever get import names"
        call ESMF_StateGet(import, itemnamelist=names_import, rc=rc)
        VERIFY_NUOPC_(rc)
        print*,"Reciever import names are:", names_import

        print*,"Reciever get number of exports"
        call ESMF_StateGet(export, itemcount=num_export, rc=rc)
        VERIFY_NUOPC_(rc)
        print*, "Reciever num export:", num_export

        allocate(names_export(num_export))
        print*,"Reciever get export names"
        call ESMF_StateGet(export, itemnamelist=names_export, rc=rc)
        VERIFY_NUOPC_(rc)
        print*,"Reciever export names are:", names_export

        print*, "Reciever finish Run"
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

        _RETURN(_SUCCESS)
    end subroutine ForceAllocation
end module Reciever_GridCompMod
