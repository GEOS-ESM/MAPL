#include "MAPL.h"

module mapl3g_OpenMP_Support
    use ESMF
    use mapl_ErrorHandling
    use mapl_KeywordEnforcer
    use mapl3g_GridGet, only: GridGet
    use mapl3g_Subgrid, only: Interval, make_subgrids, find_bounds
    use mapl3g_StateAddMethod, only: CallbackMap, CallbackMapIterator, CallbackMethodWrapper, get_callbacks
    use mapl3g_StateAddMethod, only: operator(/=)
    !$ use omp_lib

    implicit none(type,external)
    private

    public :: Interval
    public :: make_subgrids
    public :: make_subfields
    public :: make_subFieldBundles
    public :: make_substates
    public :: make_subgridcomps
    public :: get_current_thread
    public :: get_num_threads
    public :: get_callbacks

    interface make_subfields
       procedure make_subfields_from_num_grids
    end interface make_subfields

    interface make_subfieldBundles
       procedure make_subfieldBundles_ordinary
    end interface

    interface make_substates
       procedure make_substates_from_num_grids
    end interface make_substates


    CONTAINS

    integer function get_current_thread() result(current_thread)
        current_thread = 0  ! default if OpenMP is not used
        !$ current_thread = omp_get_thread_num() ! get the actual thread id if OpenMP is used
    end function get_current_thread

    integer function get_num_threads() result(num_threads)
        num_threads = 1  ! default if OpenMP is not used
        !$ num_threads = omp_get_max_threads() ! get the actual number of threads if OpenMP is used
    end function get_num_threads

    function make_subfields_from_num_grids(primary_field, num_subgrids, unusable, rc) result(subfields)
        type(ESMF_Field), allocatable :: subfields(:)
        type(ESMF_Field), intent(in) :: primary_field
        integer, intent(in) :: num_subgrids
        class(KeywordEnforcer), optional, intent(in) :: unusable
        integer, optional, intent(out) :: rc
        integer :: status, i
        real(kind=ESMF_KIND_R4), pointer :: old_ptr_2d_r4(:,:)
        real(kind=ESMF_KIND_R4), pointer :: new_ptr_2d_r4(:,:)
        real(kind=ESMF_KIND_R8), pointer :: old_ptr_2d_r8(:,:)
        real(kind=ESMF_KIND_R8), pointer :: new_ptr_2d_r8(:,:)
        real(kind=ESMF_KIND_R4), pointer :: old_ptr_3d_r4(:,:,:)
        real(kind=ESMF_KIND_R4), pointer :: new_ptr_3d_r4(:,:,:)
        real(kind=ESMF_KIND_R8), pointer :: old_ptr_3d_r8(:,:,:)
        real(kind=ESMF_KIND_R8), pointer :: new_ptr_3d_r8(:,:,:)
        real(kind=ESMF_KIND_R4), pointer :: old_ptr_4d_r4(:,:,:,:)
        real(kind=ESMF_KIND_R4), pointer :: new_ptr_4d_r4(:,:,:,:)
        real(kind=ESMF_KIND_R8), pointer :: old_ptr_4d_r8(:,:,:,:)
        real(kind=ESMF_KIND_R8), pointer :: new_ptr_4d_r8(:,:,:,:)
        integer(kind=ESMF_KIND_I4), pointer :: old_ptr_2d_i4(:,:)
        integer(kind=ESMF_KIND_I4), pointer :: new_ptr_2d_i4(:,:)
        integer(kind=ESMF_KIND_I4), pointer :: old_ptr_3d_i4(:,:,:)
        integer(kind=ESMF_KIND_I4), pointer :: new_ptr_3d_i4(:,:,:)
        integer(kind=ESMF_KIND_I4), pointer :: old_ptr_4d_i4(:,:,:,:)
        integer(kind=ESMF_KIND_I4), pointer :: new_ptr_4d_i4(:,:,:,:)
        integer(kind=ESMF_KIND_I8), pointer :: old_ptr_2d_i8(:,:)
        integer(kind=ESMF_KIND_I8), pointer :: new_ptr_2d_i8(:,:)
        integer(kind=ESMF_KIND_I8), pointer :: old_ptr_3d_i8(:,:,:)
        integer(kind=ESMF_KIND_I8), pointer :: new_ptr_3d_i8(:,:,:)
        integer(kind=ESMF_KIND_I4), pointer :: old_ptr_4d_i8(:,:,:,:)
        integer(kind=ESMF_KIND_I4), pointer :: new_ptr_4d_i8(:,:,:,:)
        type(ESMF_TypeKind_Flag) :: typekind
        integer :: rank
        integer :: jm
        character(len=ESMF_MAXSTR) :: name
        type(ESMF_Grid), allocatable :: subgrids(:)
        type(Interval), allocatable :: bounds(:)
        type(ESMF_Grid) :: primary_grid
        type(ESMF_Info) :: info_in, info_out

        call ESMF_FieldGet(primary_field, grid=primary_grid, typekind=typekind, rank=rank, name=name, _RC)
        call GridGet(primary_grid, jm=jm, _RC)

        bounds = find_bounds(jm, num_subgrids)
        subgrids = make_subgrids(primary_grid, num_subgrids, _RC)
        allocate(subfields(size(bounds)))

        ! 1d, r4 or r8
        if (rank == 1) then
           subfields = spread(primary_field, dim=1, ncopies=num_subgrids)
        ! 2d, r4
        else if (typekind == ESMF_TYPEKIND_R4 .AND. rank == 2) then
           call ESMF_FieldGet(field=primary_field, localDe=0, farrayPtr=old_ptr_2d_r4, _RC)
           do i = 1, size(bounds)
              new_ptr_2d_r4 => old_ptr_2d_r4(:,bounds(i)%min:bounds(i)%max)
              subfields(i) = ESMF_FieldCreate(subgrids(i), new_ptr_2d_r4, name=name, _RC)
              call ESMF_InfoGetFromHost(primary_field, info_in, _RC)
              call ESMF_InfoGetFromHost(subfields(i), info_out, _RC)
              call ESMF_InfoSet(info_out, key="", value=info_in, _RC)
           end do

        ! 2d, r8
        else if (typekind == ESMF_TYPEKIND_R8 .AND. rank == 2) then
           call ESMF_FieldGet(field=primary_field, localDe=0, farrayPtr=old_ptr_2d_r8, _RC)
           do i = 1, size(bounds)
              new_ptr_2d_r8 => old_ptr_2d_r8(:,bounds(i)%min:bounds(i)%max)
              subfields(i) = ESMF_FieldCreate(subgrids(i), new_ptr_2d_r8, name=name, _RC)
              call ESMF_InfoGetFromHost(primary_field, info_in, _RC)
              call ESMF_InfoGetFromHost(subfields(i), info_out, _RC)
              call ESMF_InfoSet(info_out, key="", value=info_in, _RC)
           end do

        ! 3d, r4
        else if (typekind == ESMF_TYPEKIND_R4 .AND. rank == 3) then
           call ESMF_FieldGet(field=primary_field, localDe=0, farrayPtr=old_ptr_3d_r4, _RC)
           do i = 1, size(bounds)
              new_ptr_3d_r4(1:,1:,lbound(old_ptr_3d_r4,3):) => old_ptr_3d_r4(:,bounds(i)%min:bounds(i)%max,lbound(old_ptr_3d_r4,3):)
              subfields(i) = ESMF_FieldCreate(subgrids(i), new_ptr_3d_r4, name=name, _RC)
              call ESMF_InfoGetFromHost(primary_field, info_in, _RC)
              call ESMF_InfoGetFromHost(subfields(i), info_out, _RC)
              call ESMF_InfoSet(info_out, key="", value=info_in, _RC)
           end do

        ! 3d, r8
        else if (typekind == ESMF_TYPEKIND_R8 .AND. rank == 3) then
           call ESMF_FieldGet(field=primary_field, localDe=0, farrayPtr=old_ptr_3d_r8, _RC)
           do i = 1, size(bounds)
              new_ptr_3d_r8(1:,1:,lbound(old_ptr_3d_r8,3):) => old_ptr_3d_r8(:,bounds(i)%min:bounds(i)%max,lbound(old_ptr_3d_r8,3):)
              subfields(i) = ESMF_FieldCreate(subgrids(i), new_ptr_3d_r8, name=name, _RC)
              call ESMF_InfoGetFromHost(primary_field, info_in, _RC)
              call ESMF_InfoGetFromHost(subfields(i), info_out, _RC)
              call ESMF_InfoSet(info_out, key="", value=info_in, _RC)
           end do

        ! 4d, r4
        else if (typekind == ESMF_TYPEKIND_R4 .AND. rank == 4) then
           call ESMF_FieldGet(field=primary_field, localDe=0, farrayPtr=old_ptr_4d_r4, _RC)
           do i = 1, size(bounds)
              new_ptr_4d_r4 => old_ptr_4d_r4(:,bounds(i)%min:bounds(i)%max,:,:)
              subfields(i) = ESMF_FieldCreate(subgrids(i), new_ptr_4d_r4, name=name, _RC)
              call ESMF_InfoGetFromHost(primary_field, info_in, _RC)
              call ESMF_InfoGetFromHost(subfields(i), info_out, _RC)
              call ESMF_InfoSet(info_out, key="", value=info_in, _RC)
           end do

        ! 4d, r8
        else if (typekind == ESMF_TYPEKIND_R8 .AND. rank == 4) then
           call ESMF_FieldGet(field=primary_field, localDe=0, farrayPtr=old_ptr_4d_r8, _RC)
           do i = 1, size(bounds)
              new_ptr_4d_r8 => old_ptr_4d_r8(:,bounds(i)%min:bounds(i)%max,:,:)
              subfields(i) = ESMF_FieldCreate(subgrids(i), new_ptr_4d_r8, name=name, _RC)
              call ESMF_InfoGetFromHost(primary_field, info_in, _RC)
              call ESMF_InfoGetFromHost(subfields(i), info_out, _RC)
              call ESMF_InfoSet(info_out, key="", value=info_in, _RC)
           end do

        ! 2d, i4
        else if (typekind == ESMF_TYPEKIND_I4 .AND. rank == 2) then
           call ESMF_FieldGet(field=primary_field, localDe=0, farrayPtr=old_ptr_2d_i4, _RC)
           do i = 1, size(bounds)
              new_ptr_2d_i4 => old_ptr_2d_i4(:,bounds(i)%min:bounds(i)%max)
              subfields(i) = ESMF_FieldCreate(subgrids(i), new_ptr_2d_i4, name=name, _RC)
              call ESMF_InfoGetFromHost(primary_field, info_in, _RC)
              call ESMF_InfoGetFromHost(subfields(i), info_out, _RC)
              call ESMF_InfoSet(info_out, key="", value=info_in, _RC)
           end do

        ! 3d, i4
        else if (typekind == ESMF_TYPEKIND_I4 .AND. rank == 3) then
           call ESMF_FieldGet(field=primary_field, localDe=0, farrayPtr=old_ptr_3d_i4, _RC)
           do i = 1, size(bounds)
              new_ptr_3d_i4 => old_ptr_3d_i4(:,bounds(i)%min:bounds(i)%max,:)
              subfields(i) = ESMF_FieldCreate(subgrids(i), new_ptr_3d_i4, name=name, _RC)
              call ESMF_InfoGetFromHost(primary_field, info_in, _RC)
              call ESMF_InfoGetFromHost(subfields(i), info_out, _RC)
              call ESMF_InfoSet(info_out, key="", value=info_in, _RC)
           end do

        ! 4d, i4
        else if (typekind == ESMF_TYPEKIND_I4 .AND. rank == 4) then
           call ESMF_FieldGet(field=primary_field, localDe=0, farrayPtr=old_ptr_4d_i4, _RC)
           do i = 1, size(bounds)
              new_ptr_4d_i4 => old_ptr_4d_i4(:,bounds(i)%min:bounds(i)%max,:,:)
              subfields(i) = ESMF_FieldCreate(subgrids(i), new_ptr_4d_i4, name=name, _RC)
              call ESMF_InfoGetFromHost(primary_field, info_in, _RC)
              call ESMF_InfoGetFromHost(subfields(i), info_out, _RC)
              call ESMF_InfoSet(info_out, key="", value=info_in, _RC)
           end do

        ! 2d, i8
        else if (typekind == ESMF_TYPEKIND_I8 .AND. rank == 2) then
           call ESMF_FieldGet(field=primary_field, localDe=0, farrayPtr=old_ptr_2d_i8, _RC)
           do i = 1, size(bounds)
              new_ptr_2d_i8 => old_ptr_2d_i8(:,bounds(i)%min:bounds(i)%max)
              subfields(i) = ESMF_FieldCreate(subgrids(i), new_ptr_2d_i8, name=name, _RC)
              call ESMF_InfoGetFromHost(primary_field, info_in, _RC)
              call ESMF_InfoGetFromHost(subfields(i), info_out, _RC)
              call ESMF_InfoSet(info_out, key="", value=info_in, _RC)
           end do

        ! 3d, i8
        else if (typekind == ESMF_TYPEKIND_I8 .AND. rank == 3) then
           call ESMF_FieldGet(field=primary_field, localDe=0, farrayPtr=old_ptr_3d_i8, _RC)
           do i = 1, size(bounds)
              new_ptr_3d_i8 => old_ptr_3d_i8(:,bounds(i)%min:bounds(i)%max,:)
              subfields(i) = ESMF_FieldCreate(subgrids(i), new_ptr_3d_i8, name=name, _RC)
              call ESMF_InfoGetFromHost(primary_field, info_in, _RC)
              call ESMF_InfoGetFromHost(subfields(i), info_out, _RC)
              call ESMF_InfoSet(info_out, key="", value=info_in, _RC)
           end do

        ! 4d, i8
        else if (typekind == ESMF_TYPEKIND_I8 .AND. rank == 4) then
           call ESMF_FieldGet(field=primary_field, localDe=0, farrayPtr=old_ptr_4d_i8, _RC)
           do i = 1, size(bounds)
              new_ptr_4d_i8 => old_ptr_4d_i8(:,bounds(i)%min:bounds(i)%max,:,:)
              subfields(i) = ESMF_FieldCreate(subgrids(i), new_ptr_4d_i8, name=name, _RC)
              call ESMF_InfoGetFromHost(primary_field, info_in, _RC)
              call ESMF_InfoGetFromHost(subfields(i), info_out, _RC)
              call ESMF_InfoSet(info_out, key="", value=info_in, _RC)
           end do

        end if

        _RETURN(ESMF_SUCCESS)
        _UNUSED_DUMMY(unusable)
    end function make_subfields_from_num_grids

    function make_subFieldBundles_ordinary(bundle, num_grids, unusable, rc) result(sub_bundles)
       type(ESMF_FieldBundle), allocatable :: sub_bundles(:)
       type(ESMF_FieldBundle), intent(in) :: bundle
       integer, intent(in) :: num_grids
       class(KeywordEnforcer), optional, intent(in) :: unusable
       integer, optional, intent(out) :: rc
       integer :: i, j, num_fields, status
       type(ESMF_Field), allocatable :: field_list(:)
       type(ESMF_Field), allocatable :: subfields(:)
       character(len=ESMF_MAXSTR) :: name
       type(ESMF_Info) :: info_in, info_out

       allocate(sub_bundles(num_grids))

       ! get number of fields and field list from field bundle
       call ESMF_FieldBundleGet(bundle, fieldCount=num_fields, name=name, _RC)
       allocate(field_list(num_fields))
       call ESMF_FieldBundleGet(bundle, fieldList=field_list, _RC)

       ! make subfields for each field and add each subfield to corresponding field bundle
       do i = 1, num_grids
          sub_bundles(i) = ESMF_FieldBundleCreate(name=name, _RC)
          call ESMF_InfoGetFromHost(bundle, info_in, _RC)
          call ESMF_InfoGetFromHost(sub_bundles(i), info_out, _RC)
          call ESMF_InfoSet(info_out, key="", value=info_in, _RC)
       end do
       do i = 1, size(field_list)
          subfields = make_subfields(field_list(i), num_grids, _RC)
          do j = 1, size(subfields)
             call ESMF_FieldBundleAdd(sub_bundles(j), subfields(j:j), _RC)
          end do
       end do

       _RETURN(ESMF_SUCCESS)
       _UNUSED_DUMMY(unusable)
    end function make_subFieldBundles_ordinary

    recursive function make_substates_from_num_grids(state, num_subgrids, unusable, rc) result(substates)
      type(ESMF_State), allocatable :: substates(:)
      type(ESMF_State), intent(inout) :: state
      integer, intent(in) :: num_subgrids
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      character(len=ESMF_MAXSTR) :: name
      integer :: count, status, i, j
      character(len=ESMF_MAXSTR), allocatable :: item_names(:)
      type(ESMF_StateItem_Flag), allocatable :: item_types(:)
      type(ESMF_Field), allocatable :: subfields(:)
      type(ESMF_FieldBundle), allocatable :: sub_bundles(:)
      type(ESMF_State), allocatable :: sub_nested_states(:)
      type(ESMF_Field) :: field
      type(ESMF_FieldBundle) :: bundle
      type(ESMF_State) :: nested_state
      type(ESMF_FieldStatus_Flag) :: fieldStatus
      type(ESMF_Info) :: info_in, info_out

      allocate(substates(num_subgrids))
      ! get information about state contents in order they were added
      call ESMF_StateGet(state, itemCount=count, name=name, _RC)

      allocate(item_names(count))
      allocate(item_types(count))
      call ESMF_StateGet(state, itemOrderFlag=ESMF_ITEMORDER_ADDORDER, itemNameList=item_names, &
           itemTypeList=item_types, _RC)

      do i = 1, num_subgrids
         substates(i) = ESMF_StateCreate(name=name, _RC)
         call ESMF_InfoGetFromHost(state, info_in, _RC)
         call ESMF_InfoGetFromHost(substates(i), info_out, _RC)
         call ESMF_InfoSet(info_out, key="", value=info_in, _RC)
      end do

      do i = 1, count
         if (item_types(i) == ESMF_STATEITEM_FIELD) then
            call ESMF_StateGet(state, item_names(i), field, _RC)
            call ESMF_FieldGet(field, status=fieldStatus, _RC)
            if (fieldStatus /= ESMF_FIELDSTATUS_COMPLETE) then
               subfields = spread(field, dim=1, ncopies=num_subgrids)
            else
               subfields = make_subfields(field, num_subgrids, _RC)
            end if
            ! add subfields to appropriate substate
            do j = 1, size(subfields)
               call ESMF_StateAdd(substates(j), subfields(j:j), _RC)
            end do
         else if (item_types(i) == ESMF_STATEITEM_FIELDBUNDLE) then
            call ESMF_StateGet(state, item_names(i), bundle, _RC)
            sub_bundles = make_subFieldBundles(bundle, num_subgrids, _RC)
            ! add sub_bundles to appropriate substate
            do j = 1, size(sub_bundles)
                call ESMF_StateAdd(substates(j), sub_bundles(j:j), _RC)
            end do
         else if (item_types(i) == ESMF_STATEITEM_STATE) then
            call ESMF_StateGet(state, item_names(i), nested_state, _RC)
            sub_nested_states = make_substates(nested_state, num_subgrids, _RC)
            ! add the nested substates to appropriate larger substate
            do j = 1, size(sub_nested_states)
               call ESMF_StateAdd(substates(j), sub_nested_states(j:j), _RC)
            end do
         end if
      end do

      call copy_callbacks(state, substates, _RC)

      _RETURN(0)
      _UNUSED_DUMMY(unusable)
    end function make_substates_from_num_grids

    function make_subgridcomps(GridComp, run_entry_points, num_grids, unusable, rc) result(subgridcomps)
        use mapl3g_RunEntryPoint
        use mapl3g_EntryPointVector
        type(ESMF_GridComp), allocatable :: subgridcomps(:)
        type(ESMF_GridComp), intent(in)  :: GridComp
        type(entryPointVector), intent(in) :: run_entry_points
        integer, intent(in) :: num_grids
        class(KeywordEnforcer), optional, intent(in) :: unusable
        integer, optional, intent(out) :: rc

        integer :: status, user_status
        type(ESMF_VM) :: vm
        integer :: myPet, i, ilabel
        logical :: has_private_state
        type(runEntryPoint), pointer :: run_entry_point
        procedure(), pointer :: user_method => null()

        type :: MAPL_GenericWrap
           type(ESMF_Clock), pointer :: dummy
        end type MAPL_GenericWrap

        type(MAPL_GenericWrap) :: wrap
        character(len=ESMF_MAXSTR) :: comp_name
        character(len=:), allocatable :: labels(:)
        integer :: phase
        type(ESMF_Config) :: CF

        allocate(subgridcomps(num_grids))

        call ESMF_VMGetCurrent(vm, _RC)
        call ESMF_VMGet(vm, localPET=myPET, _RC)

        call ESMF_GridCompGet(GridComp, config=CF, name=comp_name, _RC)
        call ESMF_InternalStateGet(GridComp, labelList=labels, _RC)

        do i = 1, num_grids
          associate (gc => subgridcomps(i) )
            gc = ESMF_GridCompCreate(name=trim(comp_name), config=CF, petlist=[myPet], &
                 & contextflag=ESMF_CONTEXT_OWN_VM, _RC)
            call ESMF_GridCompSetServices(gc, set_services, userrc=user_status, _RC)
            _VERIFY(user_status)
          end associate
        end do

        do ilabel = 1, size(labels)
           call ESMF_UserCompGetInternalState(GridComp, trim(labels(ilabel)), wrap, status)
           has_private_state = (status == ESMF_SUCCESS)
           do i = 1, num_grids
              associate (gc => subgridcomps(i) )
                if (has_private_state) then
                   call ESMF_UserCompSetInternalState(gc, trim(labels(ilabel)), wrap, status)
                   _VERIFY(status)
                end if
              end associate
           end do
        end do

        _RETURN(ESMF_SUCCESS)
        _UNUSED_DUMMY(unusable)

        contains

        subroutine set_services(gc, rc)
           type(ESMF_GridComp) :: gc
           integer, intent(out):: rc
           integer :: status
           integer :: phase
            do phase = 1, run_entry_points%size()
               run_entry_point => run_entry_points%of(phase)
               if(associated(run_entry_point%run_entry_point)) then
                  user_method => run_entry_point%run_entry_point
                  call ESMF_GridCompSetEntryPoint(gc, ESMF_METHOD_RUN, phase=phase, userroutine=user_method, _RC)
                end if
            end do
           _RETURN(ESMF_SUCCESS)
        end subroutine set_services

    end function make_subgridcomps

    subroutine copy_callbacks(state, multi_states, rc)
       type(ESMF_State), intent(inout) :: state
       type(ESMF_State), intent(inout) :: multi_states(:)
       integer, optional, intent(out) :: rc

       integer :: n_multi, i
       integer :: status
       type(CallbackMethodWrapper), pointer :: wrapper
       type(CallbackMap), pointer :: callbacks
       type(CallbackMapIterator) :: iter
       procedure(), pointer :: userRoutine

       n_multi = size(multi_states)
       call get_callbacks(state, callbacks, _RC)
       _ASSERT(associated(callbacks), 'callbacks must be associated')
       associate( e => callbacks%end())
          iter = callbacks%begin()
          do while (iter /= e)
             wrapper => iter%second()
             do i = 1, n_multi
                userRoutine => wrapper%userRoutine
                call ESMF_MethodAdd(multi_states(i), label=iter%first(), userRoutine=userRoutine, _RC)
             end do
             call iter%next()
          end do
       end associate

       _RETURN(ESMF_SUCCESS)

     end subroutine copy_callbacks

 end module mapl3g_OpenMP_Support
