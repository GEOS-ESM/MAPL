#include "MAPL.h"

module mapl3g_Subgrid
    use ESMF
    use mapl3g_GridGet, only: GridGet, GridGetCoordinates
    use mapl_ErrorHandling
    use mapl_KeywordEnforcer

    implicit none(type,external)
    private

    public :: Interval
    public :: make_subgrids
    public :: find_bounds

    type :: Interval
        integer :: min
        integer :: max
    end type Interval

    interface make_subgrids
        procedure make_subgrids_from_num_grids
        procedure make_subgrids_from_bounds
    end interface make_subgrids

CONTAINS

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
    end function find_bounds

    function subset_array(input_array, bounds) result(output_array)
        real(kind=ESMF_KIND_R8), pointer, intent(in) :: input_array(:,:)
        type(Interval), intent(in) :: bounds
        real(kind=ESMF_KIND_R8), pointer :: output_array(:,:)

        allocate(output_array(size(input_array,1), bounds%max - bounds%min + 1))
        output_array(:,:) = input_array(:,bounds%min:bounds%max)
    end function subset_array

    function make_subgrids_from_num_grids(primary_grid, num_grids, unusable, rc) result(subgrids)
        type(ESMF_Grid), allocatable :: subgrids(:)
        type(ESMF_Grid), intent(inout) :: primary_grid
        integer, intent(in) :: num_grids
        class(KeywordEnforcer), optional, intent(in) :: unusable
        integer, optional, intent(out) :: rc
        integer :: im, jm
        integer :: status
        type(Interval), allocatable :: bounds(:)

        call GridGet(primary_grid, im=im, jm=jm, _RC)
        bounds = find_bounds(jm, num_grids)
        subgrids = make_subgrids(primary_grid, bounds, _RC)
        _RETURN(ESMF_SUCCESS)
        _UNUSED_DUMMY(unusable)
    end function make_subgrids_from_num_grids

    function make_subgrids_from_bounds(primary_grid, bounds, unusable, rc) result(subgrids)
        type(ESMF_Grid), allocatable :: subgrids(:)
        type(ESMF_Grid), intent(inout) :: primary_grid ! inout to use GridGetCoordinates
        type(Interval), intent(in) :: bounds(:)
        class(KeywordEnforcer), optional, intent(in) :: unusable
        integer, optional, intent(out) :: rc
        integer :: im, jm
        integer, allocatable :: global_count(:), interior(:)
        integer :: status
        integer :: petMap(1,1,1)
        integer :: myPet, section, i, j, k, count, size_
        type(ESMF_VM) :: vm
        real(kind=ESMF_KIND_R8), pointer :: new_lats(:,:), new_lons(:,:)
        real(kind=ESMF_KIND_R8), pointer :: lons(:,:), lats(:,:)
        real(kind=ESMF_KIND_R8), pointer :: new_corner_lats(:,:), new_corner_lons(:,:)
        real(kind=ESMF_KIND_R8), allocatable :: corners(:,:,:)
        real(kind=ESMF_KIND_R8), allocatable :: lats1d(:), lons1d(:)
        character(len=ESMF_MAXSTR) :: name
        type(ESMF_Info) :: info_in, info_out, infoh
        logical :: isPresent

        call ESMF_GridGet(primary_grid, name=name, _RC)

        allocate(subgrids(size(bounds)))
        call GridGet(primary_grid, im=im, jm=jm, corners=corners, interior=interior, _RC)
        call ESMF_VMGetCurrent(vm, _RC)
        call ESMF_VMGet(vm, localPET=myPET, _RC)

        ! global count: use GridGet on an undecomposed view — for now derive from
        ! the GLOBAL_GRID_INFO attribute if present, else query tile extents directly
        block
            integer :: tileCount, dimCount
            integer, allocatable :: minIndex(:), maxIndex(:)
            call ESMF_GridGet(primary_grid, tileCount=tileCount, dimCount=dimCount, _RC)
            allocate(global_count(dimCount+1))
            allocate(minIndex(dimCount), maxIndex(dimCount))
            call ESMF_GridGet(primary_grid, tile=1, &
                staggerloc=ESMF_STAGGERLOC_CENTER_VCENTER, &
                minIndex=minIndex, maxIndex=maxIndex, _RC)
            global_count(1:dimCount) = maxIndex - minIndex + 1
            global_count(dimCount+1) = 1 ! default lev count
        end block

        petMap(1,1,1) = myPet
        do i = 1, size(bounds)
           section = bounds(i)%max - bounds(i)%min + 1
           ! make each subgrid
           subgrids(i) = ESMF_GridCreateNoPeriDim( &
                countsPerDEDim1 = [im], &
                countsPerDEDim2 = [section], &
                indexFlag=ESMF_INDEX_DELOCAL, &
                coordDep1=[1,2], &
                coordDep2=[1,2], &
                coordSys=ESMF_COORDSYS_SPH_RAD, &
                petMap = petMap, &
                name = name, &
                _RC)

           call ESMF_GridAddCoord(grid=subgrids(i), staggerloc=ESMF_STAGGERLOC_CENTER, _RC)
           call ESMF_InfoGetFromHost(primary_grid, info_in, _RC)
           call ESMF_InfoGetFromHost(subgrids(i), info_out, _RC)
           call ESMF_InfoSet(info_out, key="", value=info_in, _RC)

           ! delete corner lon/lat attributes in the subgrid
           isPresent = ESMF_InfoIsPresent(info_out,'GridCornerLons:',_RC)
           if (isPresent) then
              call ESMF_InfoRemove(info_out,'GridCornerLons:',_RC)
           end if
           isPresent = ESMF_InfoIsPresent(info_out,'GridCornerLats:',_RC)
           if (isPresent) then
              call ESMF_InfoRemove(info_out,'GridCornerLats:',_RC)
           endif
        end do

        ! get center lons/lats from original grid
        call GridGetCoordinates(primary_grid, longitudes=lons, latitudes=lats, _RC)

        do i = 1, size(bounds)
           call ESMF_GridGetCoord(grid=subgrids(i), coordDim=1, localDE=0, &
                staggerloc=ESMF_STAGGERLOC_CENTER, &
                farrayPtr=new_lons, _RC)
           new_lons = subset_array(lons, bounds(i))

           call ESMF_GridGetCoord(grid=subgrids(i), coordDim=2, localDE=0, &
                staggerloc=ESMF_STAGGERLOC_CENTER, &
                farrayPtr=new_lats, _RC)
           new_lats = subset_array(lats, bounds(i))

           allocate(new_corner_lons(size(new_lons,1)+1,size(new_lons,2)+1))
           allocate(new_corner_lats(size(new_lats,1)+1,size(new_lats,2)+1))

           new_corner_lons = corners(:,bounds(i)%min:bounds(i)%max+1,1)
           new_corner_lats = corners(:,bounds(i)%min:bounds(i)%max+1,2)

           ! translate the 2d arrays into 1D arrays
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

           call ESMF_InfoGetFromHost(subgrids(i), infoh, _RC)
           ! add these arrays as attributes in the subgrids
           call ESMF_InfoSet(infoh, key='GridCornerLons:', &
               values=lons1d, _RC)
           call ESMF_InfoSet(infoh, key='GridCornerLats:', &
               values=lats1d, _RC)
           block
              integer :: global_grid_info(11)
              integer :: i1, i2, j1, j2
              i1 = interior(1)
              i2 = interior(2)
              j1 = interior(3)
              j2 = interior(4)
              global_grid_info(1:2) = global_count(1:2)
              global_grid_info(3)   = global_count(3)
              global_grid_info(4)   = size(new_lons,1)
              global_grid_info(5)   = size(new_lons,2)
              global_grid_info(6)   = global_count(3)
              global_grid_info(7)   = i1
              global_grid_info(8)   = i2
              global_grid_info(9)   = j1 + bounds(i)%min - 1
              global_grid_info(10)  = j1 + bounds(i)%max - 1
              global_grid_info(11)  = bounds(i)%min
              call ESMF_InfoSet(infoh, key="GLOBAL_GRID_INFO", values=global_grid_info, _RC)
           end block

           deallocate(lons1d, lats1d)
           deallocate(new_corner_lons, new_corner_lats)
        end do
        _RETURN(ESMF_SUCCESS)
        _UNUSED_DUMMY(unusable)
    end function make_subgrids_from_bounds

end module mapl3g_Subgrid
