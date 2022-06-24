#include "NUOPC_ErrLog.h"
#include "MAPL_Generic.h"

module UFS_Testing_Cap
    use ESMF
    use NUOPC
    use NUOPC_Model, &
            ufsSS => SetServices, &
            ufsDI => label_DataInitialize, &
            ufsLA => label_Advance

    use MAPL

    implicit none
    private

    public SetServices

    integer, parameter :: ImportFieldCount = 1
    character(len=*), dimension(ImportFieldCount), parameter :: &
            ImportFieldNames = [ &
                    "var1" &
                    ]

    ! integer, parameter :: ExportFieldCount = 1
    ! character(len=*), dimension(ExportFieldCount), parameter :: &
    !         ExportFieldNames = [ &
    !                 "var2" &
    !                 ]

    type FieldSpec
        character(len=:), allocatable :: name
        type(ESMF_TYPEKIND_Flag)      :: typekind
        integer, allocatable          :: grid_to_field_map(:), &
                                         ungridded_lbound(:), &
                                         ungridded_ubound(:)
        integer                       :: dims, vlocation
    contains
        procedure :: populate_from_field
        procedure :: complete_field_in_state
        procedure :: complete_field
        procedure :: create_field
    end type FieldSpec
contains
    subroutine populate_from_field(this, field, rc)
        class(FieldSpec), intent(inout) :: this
        type(ESMF_Field), intent(in   ) :: field
        integer,          intent(  out) :: rc

        integer                    :: typekind_int, item_count
        character(len=ESMF_MAXSTR) :: field_name

        rc = ESMF_SUCCESS

        ! Get the field's name
        call ESMF_FieldGet(field, name=field_name, rc=rc)
        VERIFY_NUOPC_(rc)
        this%name = trim(field_name)

        ! Get the field's typekind
        call ESMF_AttributeGet(field, name='TypeKind', convention='NUOPC', &
                purpose='Instance', value=typekind_int, rc=rc)
        VERIFY_NUOPC_(rc)
        this%typekind = typekind_int

        ! Get the field's grid_to_field_map
        call NUOPC_GetAttribute(field, name='GridToFieldMap', &
                itemCount=item_count, rc=rc)
        VERIFY_NUOPC_(rc)
        if  (item_count > 0) then
            allocate(this%grid_to_field_map(item_count))
            call ESMF_AttributeGet(field,  name='GridToFieldMap', &
                    convention='NUOPC', purpose='Instance', &
                    valueList=this%grid_to_field_map, rc=rc)
            VERIFY_NUOPC_(rc)
        end if

        ! Get the field's ungridded lbound
        call NUOPC_GetAttribute(field, name='UngriddedLBound', &
                itemCount=item_count, rc=rc)
        VERIFY_NUOPC_(rc)
        if  (item_count > 0) then
            allocate(this%grid_to_field_map(item_count))
            call ESMF_AttributeGet(field,  name='UngriddedLBound', &
                    convention='NUOPC', purpose='Instance', &
                    valueList=this%ungridded_lbound, rc=rc)
            VERIFY_NUOPC_(rc)
        end if

        ! Get the field's ungridded ubound
        call NUOPC_GetAttribute(field, name='UngriddedUBound', &
                itemCount=item_count, rc=rc)
        VERIFY_NUOPC_(rc)
        if  (item_count > 0) then
            allocate(this%grid_to_field_map(item_count))
            call ESMF_AttributeGet(field,  name='UngriddedUBound', &
                    convention='NUOPC', purpose='Instance', &
                    valueList=this%ungridded_ubound, rc=rc)
            VERIFY_NUOPC_(rc)
        end if
    end subroutine populate_from_field

    subroutine complete_field_in_state(this, state, rc)
        class(FieldSpec), intent(in   ) :: this
        type(ESMF_State), intent(inout) :: state
        integer,          intent(  out) :: rc

        type(ESMF_Field) :: field

        rc = ESMF_SUCCESS

        call ESMF_StateGet(state, field=field, itemName=this%name, rc=rc)
        VERIFY_NUOPC_(rc)

        call this%complete_field(field, rc=rc)
        VERIFY_NUOPC_(rc)
    end subroutine complete_field_in_state

    subroutine complete_field(this, field, rc)
        class(FieldSpec), intent(in   ) :: this
        type(ESMF_Field), intent(inout) :: field
        integer,          intent(  out) :: rc

        rc = ESMF_SUCCESS

        if (allocated(this%ungridded_lbound) .and. &
                allocated(this%ungridded_ubound)) then
            call ESMF_FieldEmptyComplete(field, typekind=this%typekind, &
                    gridToFieldMap=this%grid_to_field_map, &
                    ungriddedLBound=this%ungridded_lbound, &
                    ungriddedUBound=this%ungridded_ubound, &
                    rc=rc)
            VERIFY_NUOPC_(rc)
        else
            call ESMF_FieldEmptyComplete(field, this%typekind, &
                    gridToFieldMap=this%grid_to_field_map, rc=rc)
            VERIFY_NUOPC_(rc)
        end if
    end subroutine complete_field

    function create_field(this, grid, rc) result(field)
        class(FieldSpec), intent(in   ) :: this
        type(ESMF_Grid),  intent(in   ) :: grid
        integer,          intent(  out) :: rc
        type(ESMF_Field)                :: field

        rc = ESMF_SUCCESS

        if (allocated(this%ungridded_lbound) .and. &
                allocated(this%ungridded_ubound)) then
            field = ESMF_FieldCreate(grid, name=this%name, &
                    typekind=this%typekind, &
                    gridToFieldMap=this%grid_to_field_map, &
                    ungriddedLBound=this%ungridded_lbound, &
                    ungriddedUBound=this%ungridded_ubound, &
                    rc=rc)
            VERIFY_NUOPC_(rc)
        else
!            call ESMF_FieldCreate(grid, name=this%name, &
!                    typekind=this%typekind, &
!                    gridToFieldMap=this%grid_to_field_map, rc=rc)
!            VERIFY_NUOPC_(rc)
        end if
    end function create_field

    subroutine SetServices(model, rc)
        type(ESMF_GridComp)  :: model
        integer, intent(out) :: rc

        rc = ESMF_SUCCESS

        print*,"UFS start SetServices"

        ! Register NUOPC generic methods
        call NUOPC_CompDerive(model, ufsSS, rc=rc)
        VERIFY_NUOPC_(rc)

        call ESMF_GridCompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
                userRoutine=initialize_p0, phase=0, rc=rc)
        VERIFY_NUOPC_(rc)

        ! set entry point for methods that require specific implementation
        print*,"UFS register advertise fields"
        call NUOPC_CompSetEntryPoint(model, ESMF_METHOD_INITIALIZE,&
                phaseLabelList=["IPDv05p1"], userRoutine=AdvertiseFields, rc=rc)
        VERIFY_NUOPC_(rc)

        print*,"UFS register realize fields"
        call NUOPC_CompSetEntryPoint(model, ESMF_METHOD_INITIALIZE,&
                phaseLabelList=["IPDv05p6"], userRoutine=RealizeFields, rc=rc)
        VERIFY_NUOPC_(rc)

        call NUOPC_CompSpecialize(model, specLabel=ufsDI, &
                specRoutine=initialize_data, rc=rc)
        VERIFY_NUOPC_(rc)

        ! attach specializing method
        call NUOPC_CompSpecialize(model, specLabel=ufsLA, &
                specRoutine=ModelAdvance, rc=rc)
        VERIFY_NUOPC_(rc)

        call NUOPC_CompSpecialize(model, specLabel=label_Finalize, &
                specRoutine=model_finalize, rc=rc)
        VERIFY_NUOPC_(rc)

        print*,"UFS finish SetServices"
    end subroutine SetServices

    subroutine initialize_p0(model, import_state, export_state, clock, rc)
        type(ESMF_GridComp)  :: model
        type(ESMF_State)     :: import_state, export_state
        type(ESMF_Clock)     :: clock
        integer, intent(out) :: rc

        rc = ESMF_SUCCESS

        print*,"UFS start initialize_p0"

        call NUOPC_CompFilterPhaseMap(model, ESMF_METHOD_INITIALIZE, &
                acceptStringList=(/"IPDv05p"/), rc=rc)
        VERIFY_NUOPC_(rc)

        print*,"UFS finish initialize_p0"
     end subroutine initialize_p0

    subroutine AdvertiseFields(model, import_state, export_state, clock, rc)
        type(ESMF_GridComp)  :: model
        type(ESMF_State)     :: import_state, export_state
        type(ESMF_Clock)     :: clock
        integer, intent(out) :: rc

        _UNUSED_DUMMY(clock)

        print*,"UFS start advertise"

        rc = ESMF_SUCCESS

        ! if (ImportFieldCount > 0) then

        ! print*,"UFS adding imports to dictionary"
        ! if (.not. NUOPC_FieldDictionaryHasEntry("var1")) then
        !     call NUOPC_FieldDictionaryAddEntry(standardName="var1", &
        !             canonicalUnits="na", rc=rc)
        !     VERIFY_NUOPC_(rc)
        ! end if

        ! print*,"UFS advertising imports"
        ! call NUOPC_Advertise(import_state, StandardName="var1", &
        !         TransferOfferGeomObject="cannot provide", rc=rc)
        !     ! call NUOPC_Advertise(import_state, ImportFieldNames, &
        !     !         ! TransferOfferGeomObject="cannot provide", &
        !     !         ! SharePolicyField="share", &
        !     !         rc=rc)
        !     print*,"UFS advertising imports rc:", rc
        !     VERIFY_NUOPC_(rc)
        ! end if

        print*,"UFS adding exports to dictionary"
        if (.not. NUOPC_FieldDictionaryHasEntry("var2")) then
            call NUOPC_FieldDictionaryAddEntry(standardName="var2", &
                    canonicalUnits="na", rc=rc)
            VERIFY_NUOPC_(rc)
        end if
        print*,"UFS advertising exports"
        call NUOPC_Advertise(export_state, StandardName="var2", &
                TransferOfferGeomObject="cannot provide", rc=rc)

        print*,"UFS finish advertise"
    end subroutine AdvertiseFields

    subroutine RealizeFields(model, import_state, export_state, clock, rc)
        type(ESMF_GridComp)  :: model
        type(ESMF_State)     :: import_state, export_state
        type(ESMF_Clock)     :: clock
        integer, intent(out) :: rc

        integer :: num_items_import, num_items_export
        character(len=ESMF_MAXSTR), allocatable :: item_names_import(:), item_names_export(:)
        type(ESMF_Field) :: field_export, field_to_export
        type(ESMF_Grid)  :: grid_export

        _UNUSED_DUMMY(clock)

        rc = ESMF_SUCCESS

        print*,"UFS start realize"

        print*, "UFS get number of import items"
        call ESMF_StateGet(import_state, itemcount=num_items_import, rc=rc)
        VERIFY_NUOPC_(rc)
        print*, "UFS num import:", num_items_import

        allocate(item_names_import(num_items_import))
        print*, "UFS get import names"
        call ESMF_StateGet(import_state, itemnamelist=item_names_import, rc=rc)
        VERIFY_NUOPC_(rc)
        print*, "UFS import names are:", item_names_import

        ! ! call ESMF_StateGet(import_state, field=field_import, &
        ! !     itemName="var1", rc=rc)
        ! ! VERIFY_NUOPC_(rc)
        ! ! call NUOPC_Realize(import_state, field_import, rc=rc)
        ! ! VERIFY_NUOPC_(rc)

        print*, "UFS get number of export items"
        call ESMF_StateGet(export_state, itemcount=num_items_export, rc=rc)
        VERIFY_NUOPC_(rc)
        print*, "UFS num export:", num_items_export

        allocate(item_names_export(num_items_export))
        print*, "UFS get export names"
        call ESMF_StateGet(export_state, itemnamelist=item_names_export, rc=rc)
        VERIFY_NUOPC_(rc)
        print*, "UFS export names are:", item_names_export

        print*, "UFS create grid for var2"
        grid_export = ESMF_GridCreateNoPeriDimUfrm(maxIndex=(/10, 100/), &
                minCornerCoord=(/10._ESMF_KIND_R8, 20._ESMF_KIND_R8/), &
                maxCornerCoord=(/100._ESMF_KIND_R8, 200._ESMF_KIND_R8/), &
                coordSys=ESMF_COORDSYS_CART, &
                staggerLocList=(/ESMF_STAGGERLOC_CENTER/), &
                rc=rc)
        VERIFY_NUOPC_(rc)

        print*, "UFS create field for var2"
        field_to_export = ESMF_FieldCreate(name="var2", grid=grid_export, &
                typekind=ESMF_TYPEKIND_R8, rc=rc)
        VERIFY_NUOPC_(rc)

        ! print*, "UFS complete var2"
        ! call ESMF_FieldEmptyComplete(field_to_export, grid_export, rc=rc)
        ! VERIFY_NUOPC_(rc)

        print*, "UFS realize var2"
        call NUOPC_realize(export_state, field_to_export, rc=rc)
        VERIFY_NUOPC_(rc)

        ! ! print*,"UFS create import grid"
        ! ! grid_import = ESMF_GridEmptyCreate(rc=rc)
        ! ! VERIFY_NUOPC_(rc)

        ! ! print*,"UFS create import grid rc:", rc

        ! ! print*,"UFS create import field"
        ! ! field_import = ESMF_FieldCreate(name='var1', grid=grid_import, typekind=ESMF_TYPEKIND_R8, rc=rc)
        ! ! VERIFY_NUOPC_(rc)

        ! ! print*,"UFS create import field rc:", rc

        ! ! print*,"UFS nuopc realize import field"
        ! ! call NUOPC_Realize(import_state, field_import, rc=rc)
        ! ! VERIFY_NUOPC_(rc)

        print*,"UFS finish realize"
    end subroutine RealizeFields

    subroutine initialize_data(model, rc)
        type(ESMF_GridComp)  :: model
        integer, intent(out) :: rc

        rc = ESMF_SUCCESS

        call NUOPC_CompAttributeSet(model, name="InitializeDataComplete", &
                value="true", rc=rc)
        VERIFY_NUOPC_(rc)

    end subroutine initialize_data

    subroutine ModelAdvance(model, rc)
        type(ESMF_GridComp)  :: model
        integer, intent(out) :: rc

        integer          :: i, num_items_import, num_items_export
        type(ESMF_State) :: import_state, export_state
        type(ESMF_Field) :: field
        real, pointer    :: ptr2d_in(:,:), ptr2d_out(:,:)

        character(len=ESMF_MAXSTR), allocatable :: item_names_import(:), item_names_export(:)

        rc = ESMF_SUCCESS

        print*,"UFS start advance"

        print*, "UFS get import and export states"
        call NUOPC_ModelGet(model, importState=import_state, &
                exportState=export_state, rc=rc)
        VERIFY_NUOPC_(rc)

        print*, "UFS get number of import items"
        call ESMF_StateGet(import_state, itemcount=num_items_import, rc=rc)
        VERIFY_NUOPC_(rc)
        print*, "UFS num import:", num_items_import

        allocate(item_names_import(num_items_import))
        print*, "UFS get import names"
        call ESMF_StateGet(import_state, itemnamelist=item_names_import, rc=rc)
        VERIFY_NUOPC_(rc)
        print*, "UFS import names are:", item_names_import

        print*, "UFS get number of export items"
        call ESMF_StateGet(export_state, itemcount=num_items_export, rc=rc)
        VERIFY_NUOPC_(rc)
        print*, "UFS num export:", num_items_export

        allocate(item_names_export(num_items_export))
        print*, "UFS get export names"
        call ESMF_StateGet(export_state, itemnamelist=item_names_export, rc=rc)
        VERIFY_NUOPC_(rc)
        print*, "UFS export names are:", item_names_export

!        print*, "UFS get imports"
!        do i=1, ImportFieldCount
!            print*, "UFS get field"
!            call ESMF_StateGet(import_state, field=field, &
!                    itemName=trim(ImportFieldNames(i)), rc=rc)
!            VERIFY_NUOPC_(rc)
!            print*, "UFS get ptr"
!            call ESMF_FieldGet(field, localDE=0, farrayPtr=ptr2d_in, rc=rc)
!            VERIFY_NUOPC_(rc)
!
!            print*, 'The value of ', trim(ImportFieldNames(i)), ' is:', minval(ptr2d_in), maxval(ptr2d_in)
!        end do
        print*,"UFS finish advance"
    end subroutine ModelAdvance

    subroutine model_finalize(model, rc)
        type(ESMF_GridComp)  :: model
        integer, intent(out) :: rc

        rc = ESMF_SUCCESS
    end subroutine model_finalize
end module UFS_Testing_Cap
