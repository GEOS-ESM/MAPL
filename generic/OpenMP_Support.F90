#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"

module MAPL_OpenMP_Support
    use ESMF
    use MAPL_maplgrid
    use MAPL_ExceptionHandling
    use mapl_KeywordEnforcerMod
    use MAPL_BaseMod, only : MAPL_Grid_Interior
    !$ use omp_lib

    implicit none
    private

    public :: Interval
    public :: make_subgrids
    public :: make_subfields
    public :: make_subFieldBundles
    public :: make_substates
    public :: make_subgridcomps
    public :: find_bounds
    public :: subset_array
    public :: get_current_thread
    public :: get_num_threads
    public :: get_callbacks

    type :: Interval
        integer :: min
        integer :: max
    end type Interval

    interface  make_subgrids
        module procedure make_subgrids_from_num_grids
        module procedure make_subgrids_from_bounds
    end interface make_subgrids


    interface  make_subfields
       module procedure make_subfields_from_num_grids
    end interface make_subfields

    interface make_subfieldBundles
       module procedure make_subfieldBundles_ordinary
    end interface

    interface make_substates
       module procedure make_substates_from_num_grids
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

    function make_subgrids_from_num_grids(primary_grid, num_grids, unusable, rc) result(subgrids)
        type(ESMF_Grid), allocatable :: subgrids(:)
        type(ESMF_Grid), intent(inout) :: primary_grid
        integer, intent(in) :: num_grids
        class(KeywordEnforcer), optional, intent(in) :: unusable
        integer, optional, intent(out) :: rc
        integer :: local_count(3)
        integer :: status
        type(Interval), allocatable :: bounds(:)

        call MAPL_GridGet(primary_grid,localcellcountPerDim=local_count, _rc)
        bounds = find_bounds(local_count(2), num_grids)
        subgrids = make_subgrids(primary_grid, bounds, _rc)
        _return(ESMF_SUCCESS)
        _unused_dummy(unusable)
    end function make_subgrids_from_num_grids

    function make_subgrids_from_bounds(primary_grid, bounds, unusable, rc) result(subgrids)
        use MAPL_BaseMod, only : MAPL_GridGetCorners
        type(ESMF_Grid), allocatable :: subgrids(:)
        type(ESMF_Grid), intent(inout) :: primary_grid ! inout to use MAPL_GridGetCorners
        type(Interval), intent(in) :: bounds(:)
        class(KeywordEnforcer), optional, intent(in) :: unusable
        integer, optional, intent(out) :: rc
        integer :: local_count(3), global_count(3)
        integer :: status
        integer :: petMap(1,1,1)
        integer :: myPet, section, i, j, k, count, size_
        type(ESMF_VM) :: vm
        real(kind=ESMF_KIND_R8), pointer :: new_lats(:,:), new_lons(:,:)
        real(kind=ESMF_KIND_R8), pointer :: lats(:,:), lons(:,:)
        real(kind=ESMF_KIND_R8), pointer :: new_corner_lats(:,:), new_corner_lons(:,:)
        real(kind=ESMF_KIND_R8), allocatable :: corner_lats(:,:), corner_lons(:,:)
        real(kind=ESMF_KIND_R8), allocatable :: lats1d(:), lons1d(:)
        character(len=ESMF_MAXSTR) :: name
        logical :: isPresent

        call ESMF_GridGet(primary_grid, name=name, _rc)
         !print*, 'Printing bounds for ', trim(name)
        !do i = 1, size(bounds)
        !   print*, trim(name), ',', i, ':', 'Bounds min:', bounds(i)%min, 'Bounds max:', bounds(i)%max
        !end do

        allocate(subgrids(size(bounds)))
        call MAPL_GridGet(primary_grid,localcellcountPerDim=local_count, globalCellCountPerDim=global_count, _rc)
        call ESMF_VMGetCurrent(vm, _rc)
        call ESMF_VMGet(vm, localPET=myPET, _rc)

        petMap(1,1,1) = myPet
        do i = 1, size(bounds)
           section = bounds(i)%max - bounds(i)%min + 1
           ! make each grid
           subgrids(i) = ESMF_GridCreateNoPeriDim( &
                countsPerDEDim1 = [local_count(1)], &
                countsPerDEDim2 = [section], &
                indexFlag=ESMF_INDEX_DELOCAL, &
                coordDep1=[1,2], &
                coordDep2=[1,2], &
                coordSys=ESMF_COORDSYS_SPH_RAD, &
                petMap = petMap, &
                name = name, &
                _rc)

           call ESMF_GridAddCoord(grid=subgrids(i), staggerloc=ESMF_STAGGERLOC_CENTER, _rc)
           call ESMF_AttributeCopy(primary_grid, subgrids(i), attcopy=ESMF_ATTCOPY_VALUE, _rc)

           ! delete corner lon/lat atttributes in the subgrid
           call ESMF_AttributeGet(subgrids(i), name='GridCornerLons:', isPresent=isPresent, _rc)
           if (isPresent) then
              call ESMF_AttributeRemove(subgrids(i), name='GridCornerLons:')
           end if
           call ESMF_AttributeGet(subgrids(i), name='GridCornerLats:', isPresent=isPresent, _rc)
           if (isPresent) then
              call ESMF_AttributeRemove(subgrids(i), name='GridCornerLats:')
           end if
        end do

        ! get lons/lats from original grid
        call ESMF_GridGetCoord(grid=primary_grid, coordDim=1, localDE=0, &
             staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=lons, _rc)
        call ESMF_GridGetCoord(grid=primary_grid, coordDim=2, localDE=0, &
             staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=lats, _rc)

        ! get corner lons/lats from original grid
        allocate(corner_lons(local_count(1)+1,local_count(2)+1))
        allocate(corner_lats(local_count(1)+1,local_count(2)+1))
        call MAPL_GridGetCorners(primary_grid, corner_lons, corner_lats, _rc)

        do i = 1, size(bounds)
           call ESMF_GridGetCoord(grid=subgrids(i), coordDim=1, localDE=0, &
                staggerloc=ESMF_STAGGERLOC_CENTER, &
                farrayPtr=new_lons, _rc)
           new_lons = subset_array(lons, bounds(i))

           call ESMF_GridGetCoord(grid=subgrids(i), coordDim=2, localDE=0, &
                staggerloc=ESMF_STAGGERLOC_CENTER, &
                farrayPtr=new_lats, _rc)
           new_lats = subset_array(lats, bounds(i))

           allocate(new_corner_lons(size(new_lons,1)+1,size(new_lons,2)+1))
           allocate(new_corner_lats(size(new_lats,1)+1,size(new_lats,2)+1))

           new_corner_lons = corner_lons(:,bounds(i)%min:bounds(i)%max+1)
           new_corner_lats = corner_lats(:,bounds(i)%min:bounds(i)%max+1)

           ! translate the 2d arrays into 1D arrays, lines 2462 to 2468 in base/Base/Base_implementation.F90
           size_ = size(new_corner_lons, 1) * size(new_corner_lons, 2)
           allocate(lons1d(size_))
           allocate(lats1d(size_))
           count = 0
           do k=1,size(new_corner_lons,2)
              do j=1,size(new_corner_lons,1)
                 count=count+1
                 lons1d(count)=new_corner_lons(j,k)
                 lats1d(count)=new_corner_lats(j,k)
              end do
           end do

           ! add the these arrays as attributes in the subgrids
           call ESMF_AttributeSet(subgrids(i), name='GridCornerLons:', &
                itemCount = count, valueList=lons1d, _rc)
           call ESMF_AttributeSet(subgrids(i), name='GridCornerLats:', &
                itemCount = count, valueList=lats1d, _rc)
           block
              integer :: global_grid_info(11)
              integer :: i1,i2,j1,j2
              call MAPL_Grid_Interior(primary_grid,i1,i2,j1,j2)
              global_grid_info(1:3) = global_count
              !global_grid_info(4:6) = local_count
              global_grid_info(4) = size(new_lons,1)
              global_grid_info(5) = size(new_lons,2)
              global_grid_info(6) = local_count(3)
              global_grid_info(7) = i1
              global_grid_info(8) = i2
              global_grid_info(9) = j1 + bounds(i)%min - 1
              global_grid_info(10) = j1 + bounds(i)%max - 1
              global_grid_info(11) = bounds(i)%min
              call ESMF_AttributeSet(subgrids(i), name="GLOBAL_GRID_INFO",  &
                   itemCount=11, valueList=global_grid_info, _rc)
           end block

            deallocate(lons1d, lats1d)
            deallocate(new_corner_lons, new_corner_lats)
        end do
        _return(ESMF_SUCCESS)
        _unused_dummy(unusable)
    end function make_subgrids_from_bounds


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
        integer :: local_count(3)
        character(len=ESMF_MAXSTR) :: name
        type(ESMF_Grid), allocatable :: subgrids(:)
        type(Interval), allocatable :: bounds(:)
        type(ESMF_Grid) :: primary_grid


        call ESMF_FieldGet(primary_field, grid=primary_grid, typekind=typekind, rank=rank, name=name,  _rc)
        !print*, 'No failure with field named:', name
        call MAPL_GridGet(primary_grid,localcellcountPerDim=local_count, _rc)

        bounds = find_bounds(local_count(2), num_subgrids)
        subgrids = make_subgrids(primary_grid, num_subgrids, _rc)
        allocate(subfields(size(bounds)))
        !print *, __FILE__,__LINE__, num_subgrids, size(bounds), trim(name)

        ! 1d, r4 or r8
        if (rank == 1) then
           subfields = spread(primary_field, dim=1, ncopies=num_subgrids)
        ! 2d, r4
        else if (typekind == ESMF_TYPEKIND_R4 .AND. rank == 2) then
           call ESMF_FieldGet(field=primary_field, localDe=0, farrayPtr=old_ptr_2d_r4,  _rc)
           do i = 1, size(bounds)
              new_ptr_2d_r4 => old_ptr_2d_r4(:,bounds(i)%min:bounds(i)%max)
              subfields(i) = ESMF_FieldCreate(subgrids(i), new_ptr_2d_r4, name=name,  _rc)
              call ESMF_AttributeCopy(primary_field, subfields(i), attcopy=ESMF_ATTCOPY_VALUE, _rc)
           end do

        ! 2d, r8
        else if (typekind == ESMF_TYPEKIND_R8 .AND. rank == 2) then
           call ESMF_FieldGet(field=primary_field, localDe=0, farrayPtr=old_ptr_2d_r8,  _rc)
           do i = 1, size(bounds)
              new_ptr_2d_r8 => old_ptr_2d_r8(:,bounds(i)%min:bounds(i)%max)
              subfields(i) = ESMF_FieldCreate(subgrids(i), new_ptr_2d_r8, name=name,  _rc)
              call ESMF_AttributeCopy(primary_field, subfields(i), attcopy=ESMF_ATTCOPY_VALUE, _rc)
           end do

        ! 3d, r4
        else if (typekind == ESMF_TYPEKIND_R4 .AND. rank == 3) then
           call ESMF_FieldGet(field=primary_field, localDe=0, farrayPtr=old_ptr_3d_r4,  _rc)
           do i = 1, size(bounds)
              new_ptr_3d_r4(1:,1:,lbound(old_ptr_3d_r4,3):) => old_ptr_3d_r4(:,bounds(i)%min:bounds(i)%max,lbound(old_ptr_3d_r4,3):)
              subfields(i) = ESMF_FieldCreate(subgrids(i), new_ptr_3d_r4, name=name, _rc)
              call ESMF_AttributeCopy(primary_field, subfields(i), attcopy=ESMF_ATTCOPY_VALUE, _rc)
           end do

        ! 3d, r8
        else if (typekind == ESMF_TYPEKIND_R8 .AND. rank == 3) then
           call ESMF_FieldGet(field=primary_field, localDe=0, farrayPtr=old_ptr_3d_r8,  _rc)
           do i = 1, size(bounds)
              new_ptr_3d_r8(1:,1:,lbound(old_ptr_3d_r8,3):) => old_ptr_3d_r8(:,bounds(i)%min:bounds(i)%max,lbound(old_ptr_3d_r8,3):)
              subfields(i) = ESMF_FieldCreate(subgrids(i), new_ptr_3d_r8, name=name,  _rc)
              call ESMF_AttributeCopy(primary_field, subfields(i), attcopy=ESMF_ATTCOPY_VALUE, _rc)
           end do

        ! 4d, r4
        else if (typekind == ESMF_TYPEKIND_R4 .AND. rank == 4) then
           call ESMF_FieldGet(field=primary_field, localDe=0, farrayPtr=old_ptr_4d_r4,  _rc)
           do i = 1, size(bounds)
              new_ptr_4d_r4 => old_ptr_4d_r4(:,bounds(i)%min:bounds(i)%max,:,:)
              subfields(i) = ESMF_FieldCreate(subgrids(i), new_ptr_4d_r4, name=name,  _rc)
              call ESMF_AttributeCopy(primary_field, subfields(i), attcopy=ESMF_ATTCOPY_VALUE, _rc)
           end do

        ! 4d, r8
        else if (typekind == ESMF_TYPEKIND_R8 .AND. rank == 4) then
           call ESMF_FieldGet(field=primary_field, localDe=0, farrayPtr=old_ptr_4d_r8,  _rc)
           do i = 1, size(bounds)
              new_ptr_4d_r8 => old_ptr_4d_r8(:,bounds(i)%min:bounds(i)%max,:,:)
              subfields(i) = ESMF_FieldCreate(subgrids(i), new_ptr_4d_r8, name=name,  _rc)
              call ESMF_AttributeCopy(primary_field, subfields(i), attcopy=ESMF_ATTCOPY_VALUE, _rc)
           end do

        ! 2d, i4
        else if (typekind == ESMF_TYPEKIND_I4 .AND. rank == 2) then
           call ESMF_FieldGet(field=primary_field, localDe=0, farrayPtr=old_ptr_2d_i4, _rc)
           do i = 1, size(bounds)
              new_ptr_2d_i4 => old_ptr_2d_i4(:,bounds(i)%min:bounds(i)%max)
              subfields(i) = ESMF_FieldCreate(subgrids(i), new_ptr_2d_i4, name=name, _rc)
              call ESMF_AttributeCopy(primary_field, subfields(i), attcopy=ESMF_ATTCOPY_VALUE, _rc)
           end do

        ! 3d, i4
        else if (typekind == ESMF_TYPEKIND_I4 .AND. rank == 3) then
           call ESMF_FieldGet(field=primary_field, localDe=0, farrayPtr=old_ptr_3d_i4,  _rc)
           do i = 1, size(bounds)
              new_ptr_3d_i4 => old_ptr_3d_i4(:,bounds(i)%min:bounds(i)%max,:)
              subfields(i) = ESMF_FieldCreate(subgrids(i), new_ptr_3d_i4, name=name, _rc)
              call ESMF_AttributeCopy(primary_field, subfields(i), attcopy=ESMF_ATTCOPY_VALUE, _rc)
           end do

        ! 4d, i4
        else if (typekind == ESMF_TYPEKIND_I4 .AND. rank == 4) then
           call ESMF_FieldGet(field=primary_field, localDe=0, farrayPtr=old_ptr_4d_i4,  _rc)
           do i = 1, size(bounds)
              new_ptr_4d_i4 => old_ptr_4d_i4(:,bounds(i)%min:bounds(i)%max,:,:)
              subfields(i) = ESMF_FieldCreate(subgrids(i), new_ptr_4d_i4, name=name, _rc)
              call ESMF_AttributeCopy(primary_field, subfields(i), attcopy=ESMF_ATTCOPY_VALUE, _rc)
           end do

        ! 2d, i8
        else if (typekind == ESMF_TYPEKIND_I8 .AND. rank == 2) then
            call ESMF_FieldGet(field=primary_field, localDe=0, farrayPtr=old_ptr_2d_i8,  _rc)
           do i = 1, size(bounds)
              new_ptr_2d_i8 => old_ptr_2d_i8(:,bounds(i)%min:bounds(i)%max)
              subfields(i) = ESMF_FieldCreate(subgrids(i), new_ptr_2d_i8, name=name, _rc)
              call ESMF_AttributeCopy(primary_field, subfields(i), attcopy=ESMF_ATTCOPY_VALUE, _rc)
           end do

        ! 3d, i8
        else if (typekind == ESMF_TYPEKIND_I8 .AND. rank == 3) then
            call ESMF_FieldGet(field=primary_field, localDe=0, farrayPtr=old_ptr_3d_i8,  _rc)
           do i = 1, size(bounds)
              new_ptr_3d_i8 => old_ptr_3d_i8(:,bounds(i)%min:bounds(i)%max,:)
              subfields(i) = ESMF_FieldCreate(subgrids(i), new_ptr_3d_i8, name=name, _rc)
              call ESMF_AttributeCopy(primary_field, subfields(i), attcopy=ESMF_ATTCOPY_VALUE, _rc)
           end do

        ! 4d, i8
        else if (typekind == ESMF_TYPEKIND_I8 .AND. rank == 4) then
            call ESMF_FieldGet(field=primary_field, localDe=0, farrayPtr=old_ptr_4d_i8,  _rc)
           do i = 1, size(bounds)
              new_ptr_4d_i8 => old_ptr_4d_i8(:,bounds(i)%min:bounds(i)%max,:,:)
              subfields(i) = ESMF_FieldCreate(subgrids(i), new_ptr_4d_i8, name=name, _rc)
              call ESMF_AttributeCopy(primary_field, subfields(i), attcopy=ESMF_ATTCOPY_VALUE, _rc)
           end do

        end if

        _return(ESMF_SUCCESS)
        _unused_dummy(unusable)
    end function make_subfields_from_num_grids


    function find_bounds(yDim, num_grids) result(bounds)
        integer, intent(in) :: yDim
        integer, intent(in) :: num_grids
        type(Interval), allocatable :: bounds(:)
        integer :: i, step
        integer :: count, numOfFirstSize, numOfSecondSize, firstSize, secondSize
        allocate(bounds(num_grids))

        ! if the size of each grid is the same
        if (modulo(yDim, num_grids) == 0) then
            step = yDim/num_grids
            count = 1
            ! go from 1-yDim incrementing by step size
            do i = 1, yDim, step
            bounds(count)%min = i
            bounds(count)%max = i + step - 1
            count = count + 1
            end do
        ! if at least one grid is a different size
        else
            firstSize = yDim/num_grids
            numOfSecondSize = modulo(yDim, num_grids)
            numOfFirstSize = num_grids - numOfSecondSize
            secondSize = (yDim - firstSize * numOfFirstSize) / numOfSecondSize

            count = 1
            do i = 1, numOfFirstSize * firstSize, firstSize
            bounds(count)%min = i
            bounds(count)%max = i + firstSize - 1
            count = count + 1
            end do

            do i = numOfFirstSize * firstSize + 1, yDim, secondSize
            bounds(count)%min = i
            bounds(count)%max = i + secondSize - 1
            count = count + 1
            end do
        end if
    end function

    function subset_array(input_array, bounds) result(output_array)
        real(kind=ESMF_KIND_R8), pointer, intent(in) :: input_array(:,:)
        type(Interval), intent(in) :: bounds
        real(kind=ESMF_KIND_R8), pointer :: output_array(:,:)

        allocate(output_array(size(input_array,1), bounds%max - bounds%min + 1))
        output_array(:,:) = input_array(:,bounds%min:bounds%max)

    end function

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

       allocate(sub_bundles(num_grids))

       ! get number of fields and field list from field bundle
       call ESMF_FieldBundleGet(bundle, fieldCount=num_fields, name=name, _rc)
       allocate(field_list(num_fields))
       call ESMF_FieldBundleGet(bundle, fieldList=field_list, _rc)

       ! make subfields for each field and add each subfield to corresponding field bundle
       do i = 1, num_grids
          sub_bundles(i) = ESMF_FieldBundleCreate(name=name, _rc)
          call ESMF_AttributeCopy(bundle, sub_bundles(i), attcopy=ESMF_ATTCOPY_VALUE, _rc)
       end do
       do i = 1, size(field_list)
          subfields = make_subfields(field_list(i), num_grids, _rc)
          do j = 1, size(subfields)
             call ESMF_FieldBundleAdd(sub_bundles(j), subfields(j:j), _rc)
          end do
       end do

       _return(ESMF_SUCCESS)
       _unused_dummy(unusable)
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
      type (ESMF_FieldStatus_Flag) :: fieldStatus

      allocate(substates(num_subgrids))
      ! get information about state contents in order they were added
      call ESMF_StateGet(state, itemCount=count, name=name, _rc)

      allocate(item_names(count))
      allocate(item_types(count))
      call ESMF_StateGet(state, itemOrderFlag=ESMF_ITEMORDER_ADDORDER, itemNameList=item_names, &
           itemTypeList=item_types,  _rc)

      do i = 1, num_subgrids
         substates(i) = ESMF_StateCreate(name=name, _rc)
         call ESMF_AttributeCopy(state, substates(i), attcopy=ESMF_ATTCOPY_VALUE, _rc)
      end do

      do i = 1, count
         if (item_types(i) == ESMF_STATEITEM_FIELD) then
            call ESMF_StateGet(state, item_names(i), field, _rc)
            call ESMF_FieldGet(field, status=fieldStatus, _rc)
            if (fieldStatus /= ESMF_FIELDSTATUS_COMPLETE) then
               subfields = spread(field, dim=1, ncopies=num_subgrids)
            else
               subfields = make_subfields(field, num_subgrids, _rc)
            end if
            ! add subfields to appropriate substate
            do j = 1, size(subfields)
               call ESMF_StateAdd(substates(j), subfields(j:j), _rc)
            end do
         else if (item_types(i) == ESMF_STATEITEM_FIELDBUNDLE) then
            call ESMF_StateGet(state, item_names(i), bundle, _rc)
            sub_bundles = make_subFieldBundles(bundle, num_subgrids, _rc)
            ! add sub_bundles to appropriate substate
            do j = 1, size(sub_bundles)
                call ESMF_StateAdd(substates(j), sub_bundles(j:j), _rc)
            end do
         else if (item_types(i) == ESMF_STATEITEM_STATE) then
            call ESMF_StateGet(state, item_names(i), nested_state, _rc)
            sub_nested_states = make_substates(nested_state, num_subgrids, _rc)
            ! add the nested substates to appropriate larger substate
            do j = 1, size(sub_nested_states)
               call ESMF_StateAdd(substates(j), sub_nested_states(j:j), _rc)
            end do
         end if
      end do

      call copy_callbacks(state, substates, _rc)

      _return(0)
      _unused_dummy(unusable)
    end function make_substates_from_num_grids

    function make_subgridcomps(GridComp, run_entry_points, num_grids, unusable, rc) result(subgridcomps)
        use mapl_RunEntryPoint
        use mapl_EntryPointVector
        type(ESMF_GridComp), allocatable :: subgridcomps(:)
        type(ESMF_GridComp), intent(in)  :: GridComp
        type(EntryPointVector), intent(in) :: run_entry_points
        integer, intent(in) :: num_grids
        class(KeywordEnforcer), optional, intent(in) :: unusable
        integer, optional, intent(out) :: rc

        integer :: status, user_status
        type(ESMF_VM) :: vm
        integer :: myPet, i, ilabel
        logical :: has_private_state
        type(runEntryPoint), pointer :: run_entry_point
        procedure(), pointer :: user_method => null()

        type MAPL_GenericWrap
           type(ESMF_Clock), pointer :: dummy
        end type MAPL_GenericWrap

        type (MAPL_GenericWrap) :: wrap !, wrap_private
        character(len=ESMF_MAXSTR) :: comp_name
        character(len=:), allocatable :: labels(:)
        integer :: phase
        type(ESMF_Config) :: CF

        allocate(subgridcomps(num_grids))

        call ESMF_VMGetCurrent(vm, _rc)
        call ESMF_VMGet(vm, localPET=myPET, _rc)

        call ESMF_GridCompGet(GridComp, config=CF, name=comp_name, _rc)
        call ESMF_InternalStateGet(GridComp, labelList=labels, _rc)

        do i = 1, num_grids
          associate (gc => subgridcomps(i) )
            gc = ESMF_GridCompCreate(name=trim(comp_name), config=CF, petlist=[myPet], &
                 & contextflag=ESMF_CONTEXT_OWN_VM, _rc)
            call ESMF_GridCompSetServices(gc, set_services, userrc=user_status, _rc)
            _verify(user_status)
          end associate
        end do

        do ilabel = 1, size(labels)
           call ESMF_UserCompGetInternalState(GridComp, trim(labels(ilabel)), wrap, status)
           has_private_state = (status == ESMF_SUCCESS)
           do i = 1, num_grids
              associate (gc => subgridcomps(i) )
                if (has_private_state) then
                   call ESMF_UserCompSetInternalState(gc, trim(labels(ilabel)), wrap, status)
                   _verify(status)
                end if
              end associate
           end do
        end do

        _return(ESMF_SUCCESS)
        _unused_dummy(unusable)

        contains

        subroutine set_services(gc, rc)
           type(ESMF_GridComp) :: gc
           integer, intent(out):: rc
           integer :: status
            do phase = 1, run_entry_points%size()
               run_entry_point => run_entry_points%of(phase)
               if(associated(run_entry_point%run_entry_point)) then
                  user_method => run_entry_point%run_entry_point

                  call ESMF_GridCompSetEntryPoint(gc, ESMF_METHOD_RUN, phase=phase, userroutine=user_method, _rc)
                end if
            end do
           _return(ESMF_SUCCESS)
        end subroutine set_services

    end function make_subgridcomps

    subroutine copy_callbacks(state, multi_states, rc)
       use mapl_ESMF_Interfaces
       use mapl_CallbackMap
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
       call get_callbacks(state, callbacks, _rc)
       _assert(associated(callbacks), 'callbacks must be associated')
       associate( e => callbacks%end())
          iter = callbacks%begin()
          do while (iter /= e)
             wrapper => iter%second()
             do i = 1, n_multi
                userRoutine => wrapper%userRoutine
                call ESMF_MethodAdd(multi_states(i), label=iter%first(), userRoutine=userRoutine, _rc)
             end do
             call iter%next()
          end do
       end associate

       _return(ESMF_SUCCESS)

    end subroutine copy_callbacks

    subroutine get_callbacks(state, callbacks, rc)
       use mapl_ESMF_Interfaces
       use mapl_CallbackMap
       type(ESMF_State), intent(inout) :: state
       type(CallbackMap), pointer, intent(out) :: callbacks
       integer, optional, intent(out) :: rc

       integer :: status
       integer(kind=ESMF_KIND_I4), allocatable :: valueList(:)
       logical :: isPresent

       type CallbackMapWrapper
          type(CallbackMap), pointer :: map
       end type
       type(CallbackMapWrapper) :: wrapper

       call ESMF_AttributeGet(state, name='MAPL_CALLBACK_MAP', isPresent=isPresent, _rc)
       if (.not. isPresent) then ! create callback map for this state
          allocate(callbacks)
          wrapper%map => callbacks
          valueList = transfer(wrapper, [1])
          call ESMF_AttributeSet(state, name='MAPL_CALLBACK_MAP', valueList=valueList, _rc)
       end if

       ! Ugly hack to decode ESMF attribute as a gFTL map
       valueList = transfer(wrapper, [1])
       call ESMF_AttributeGet(state, name='MAPL_CALLBACK_MAP', valueList=valueList, _rc)
       wrapper = transfer(valueList, wrapper)
       callbacks => wrapper%map

       _return(ESMF_SUCCESS)

    end subroutine get_callbacks

end module MAPL_OpenMP_Support
