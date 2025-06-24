!------------------------------------------------------------------------------
!               Global Modeling and Assimilation Office (GMAO)                !
!                    Goddard Earth Observing System (GEOS)                    !
!                                 MAPL Component                              !
!------------------------------------------------------------------------------
!
#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
#include "unused_dummy.H"
!
!>
!### SUBMODULE: `(MAPL_Base) Base_Implementation`
!
! Author: GMAO SI-Team
!
! MAPL_BaseMod --- A Collection of Assorted MAPL Utilities
!
submodule (MAPL_Base) Base_Implementation

  !BOP
  !
  ! !MODULE: MAPL_BaseMod --- A Collection of Assorted MAPL Utilities

  ! !USES:
  !
  use ESMF
  use ESMFL_Mod

  use MAPL_FieldUtils
  use MAPL_Constants
  use MAPL_RangeMod
  use MAPL_SphericalGeometry
  use mapl_MaplGrid, only: MAPL_GridGet, MAPL_DistGridGet, MAPL_GetImsJms, MAPL_GridHasDE
  use MAPL_ExceptionHandling
  implicit NONE

contains

  module subroutine MAPL_AllocateCoupling(field, rc)

    type(ESMF_Field),  intent(INOUT) :: field
    integer, optional, intent(  OUT) :: rc

    integer                                 :: status
    character(len=ESMF_MAXSTR), parameter   :: IAm='MAPL_AllocateCouplingFromField'

    type(ESMF_FieldStatus_Flag)             :: fieldStatus

    integer          :: dims
    integer          :: location
    integer          :: knd
    integer, pointer :: ungrd(:)
    integer          :: hw
    integer          :: ungrd_cnt
    logical          :: has_ungrd
    logical          :: defaultProvided
    real             :: default_value

    call ESMF_FieldGet(field, status=fieldStatus, __RC)

    if (fieldStatus /= ESMF_FIELDSTATUS_COMPLETE) then

       !ALT: if the attributeGet calls fail, this would very likely indicate
       !     that the field was NOT created by MAPL (or something terrible happened)
       !     For now we just abort
       call ESMF_AttributeGet(FIELD, NAME='DIMS', VALUE=DIMS, __RC)
       call ESMF_AttributeGet(FIELD, NAME='VLOCATION', VALUE=LOCATION, __RC)
       call ESMF_AttributeGet(FIELD, NAME='HALOWIDTH', VALUE=HW, __RC)
       call ESMF_AttributeGet(FIELD, NAME='PRECISION', VALUE=KND, __RC)
       call ESMF_AttributeGet(FIELD, NAME='DEFAULT_PROVIDED', value=defaultProvided, __RC)
       if(defaultProvided) then
          call ESMF_AttributeGet(FIELD, NAME='DEFAULT_VALUE', value=default_value, __RC)
       end if

       call ESMF_AttributeGet(FIELD, NAME='UNGRIDDED_DIMS', isPresent=has_ungrd, __RC)
       if (has_ungrd) then
          call ESMF_AttributeGet(FIELD, NAME='UNGRIDDED_DIMS', itemcount=UNGRD_CNT, __RC)
          allocate(ungrd(UNGRD_CNT), __STAT)
          call ESMF_AttributeGet(FIELD, NAME='UNGRIDDED_DIMS', valueList=UNGRD, __RC)
          if (defaultProvided) then
             call MAPL_FieldAllocCommit(field, dims=dims, location=location, typekind=knd, &
                  hw=hw, ungrid=ungrd, default_value=default_value, __RC)
          else
             call MAPL_FieldAllocCommit(field, dims=dims, location=location, typekind=knd, &
                  hw=hw, ungrid=ungrd, __RC)
          end if
       else
          if (defaultProvided) then
             call MAPL_FieldAllocCommit(field, dims=dims, location=location, typekind=knd, &
                  hw=hw, default_value=default_value, __RC)
          else
             call MAPL_FieldAllocCommit(field, dims=dims, location=location, typekind=knd, &
                  hw=hw, __RC)
          end if
       end if

       if (has_ungrd) then
          deallocate(ungrd)
       end if

    end if

    __RETURN(ESMF_SUCCESS)
  end subroutine MAPL_AllocateCoupling

  module subroutine MAPL_FieldAllocCommit(field, dims, location, typekind, &
       hw, ungrid, default_value, rc)
    type(ESMF_Field),               intent(INOUT) :: field
    integer,                        intent(IN   ) :: dims
    integer,                        intent(IN   ) :: location
    integer,                        intent(IN   ) :: typekind
    integer,                        intent(IN   ) :: hw !halowidth
    integer,              optional, intent(IN   ) :: ungrid(:)
    real,                 optional, intent(IN   ) :: default_value
    integer,              optional, intent(  OUT) :: rc


    integer                               :: status
    character(len=ESMF_MAXSTR), parameter :: IAm='MAPL_FieldAllocCommit'

    real(kind=ESMF_KIND_R4), pointer      :: VAR_1D(:), VAR_2D(:,:), VAR_3D(:,:,:), VAR_4D(:,:,:,:)
    real(kind=ESMF_KIND_R8), pointer      :: VR8_1D(:), VR8_2D(:,:), VR8_3D(:,:,:), VR8_4D(:,:,:,:)
    type(ESMF_Grid)                       :: GRID
    integer                               :: COUNTS(ESMF_MAXDIM)
    integer                               :: SZUNGRD
    integer                               :: RANK
    integer                               :: gridRank
    integer                               :: I
    real                                  :: init_value
    integer, allocatable                  :: gridToFieldMap(:)
    integer, allocatable                  :: haloWidth(:)
    integer                               :: griddedDims
    integer                               :: lb1, lb2, lb3
    integer                               :: ub1, ub2, ub3

! SSI
    type(ESMF_Pin_Flag) :: pinflag
    type(ESMF_VM)       :: vm
    logical             :: ssiSharedMemoryEnabled
! SSI

    call ESMF_FieldGet(field, grid=GRID, __RC)
    call MAPL_GridGet(GRID, localCellCountPerDim=COUNTS, __RC)
    call ESMF_GridGet(GRID, dimCount=gridRank, __RC)
    __ASSERT(gridRank <= 3,' MAPL restriction - only 2 and 3d are supported')
    allocate(gridToFieldMap(gridRank), __STAT)
    gridToFieldMap = 0
    do I = 1, gridRank
       gridToFieldMap(I) = I
    end do
    ! ALT: the next allocation should have been griddedDims,
    !      but this compilcates the code unnecessery
    allocate(haloWidth(gridRank), __STAT)
    haloWidth = (/HW,HW,0/)

    if(present(default_value)) then
       init_value = default_value
    else
       init_value = 0.0
    end if

    szungrd = 0
    if (present(UNGRID)) then
       szungrd = size(UNGRID)
    end if

! SSI
    call ESMF_VMGetCurrent(vm, __RC)

    call ESMF_VMGet(vm, ssiSharedMemoryEnabledFlag=ssiSharedMemoryEnabled, __RC)

    ! call pinflag getter
    pinflag = MAPL_PinFlagGet()
    
    if (any(pinflag == [ESMF_PIN_DE_TO_SSI,ESMF_PIN_DE_TO_SSI_CONTIG])) then
       __ASSERT(ssiSharedMemoryEnabled, 'SSI shared memory is NOT supported')
    end if

! SSI

    Dimensionality: select case(DIMS)

       ! Horizontal and vertical
       ! -----------------------

    case(MAPL_DimsNone)
       rank = szungrd

       !ALT: This is special case - array does not map any gridded dims
       gridToFieldMap= 0
       if (typekind == ESMF_KIND_R4) then
          select case (rank)
          case (1)
             allocate(VAR_1D(UNGRID(1)), __STAT)
             VAR_1D = INIT_VALUE
             call ESMF_FieldEmptyComplete(FIELD, farray=VAR_1D,    &
                  indexflag=ESMF_INDEX_DELOCAL, &
                  datacopyFlag = ESMF_DATACOPY_REFERENCE,             &
                  gridToFieldMap=gridToFieldMap,                      &
                  ungriddedLBound=[1],&
                  ungriddedUBound=[ungrid(1)], &
                  __RC)
          case default
             __FAIL( 'unsupported rank > 1')
          end select

       else
          select case (rank)
          case (1)
             allocate(VR8_1D(UNGRID(1)), __STAT)
             VR8_1D = INIT_VALUE
             call ESMF_FieldEmptyComplete(FIELD, farray=VR8_1D,    &
                  indexflag=ESMF_INDEX_DELOCAL, &
                  datacopyFlag = ESMF_DATACOPY_REFERENCE,             &
                  gridToFieldMap=gridToFieldMap,                      &
                  ungriddedLBound=[1],&
                  ungriddedUBound=[ungrid(1)], &
                  __RC)
          case default
             __FAIL( 'unsupported rank > 1')
          end select

       endif

       ! Vertical only
       ! -------------

    case(MAPL_DimsVertOnly)
       !ALT: This is special case - array does not map any gridded dims
       gridToFieldMap = 0
       rank=1
       lb1 = 1
       ub1 = COUNTS(3)

       select case(LOCATION)

       case(MAPL_VLocationEdge  )
          lb1 = 0
       case(MAPL_VLocationCenter)
          lb1 = 1
       case default
          __RETURN(ESMF_FAILURE)
       end select

       if (typekind == ESMF_KIND_R4) then
          allocate(VAR_1D(lb1:ub1), __STAT)
          VAR_1D = INIT_VALUE

          call ESMF_FieldEmptyComplete(FIELD, farray=var_1d,  &
               indexflag=ESMF_INDEX_DELOCAL, &
               datacopyFlag = ESMF_DATACOPY_REFERENCE,             &
               gridToFieldMap=gridToFieldMap,                           &
               ungriddedLBound=[lb1],&
               ungriddedUBound=[ub1], &
               __RC)
       else
          allocate(VR8_1D(lb1:ub1), __STAT)
          VR8_1D = INIT_VALUE

          call ESMF_FieldEmptyComplete(FIELD, farray=vr8_1d,  &
               indexflag=ESMF_INDEX_DELOCAL, &
               datacopyFlag = ESMF_DATACOPY_REFERENCE,             &
               gridToFieldMap=gridToFieldMap,                           &
               ungriddedLBound=[lb1],&
               ungriddedUBound=[ub1], &
               __RC)
       end if

       ! Horizontal only
       ! ---------------

    case(MAPL_DimsHorzOnly)
       rank = 2 + szungrd
       __ASSERT(rank <= 4, 'unsupported rank > 4 (UNGRD not fully implemented)')
       if (gridRank == 3 .and. rank == 2) gridToFieldMap(3)=0
       griddedDims = gridRank - count(gridToFieldMap == 0)

       lb1 = 1-HW
       ub1 = COUNTS(1)+HW
       lb2 = 1-HW
       ub2 = COUNTS(2)+HW

       if (typekind == ESMF_KIND_R4) then
          RankCase2d: select case (rank)
          case (2)
             call ESMF_FieldEmptyComplete(FIELD, typekind=ESMF_TYPEKIND_R4, &
                  gridToFieldMap=gridToFieldMap,              &
                  totalLWidth=haloWidth(1:griddedDims),     &
                  totalUWidth=haloWidth(1:griddedDims),     &
                  pinflag=pinflag, __RC)
             call ESMF_FieldGet(FIELD, farrayPtr=VAR_2D, __RC)
             VAR_2D = INIT_VALUE
          case (3)
             call ESMF_FieldEmptyComplete(FIELD, typekind=ESMF_TYPEKIND_R4, &
                  gridToFieldMap=gridToFieldMap,              &
                  totalLWidth=haloWidth(1:griddedDims),     &
                  totalUWidth=haloWidth(1:griddedDims),     &
                  ungriddedLBound=(/1/), ungriddedUBound=(/UNGRID(1)/),  &
                  pinflag=pinflag,__RC)
             call ESMF_FieldGet(FIELD, farrayPtr=VAR_3D, __RC)
             VAR_3D = INIT_VALUE
          case (4)
             call ESMF_FieldEmptyComplete(FIELD, typekind=ESMF_TYPEKIND_R4, &
                  gridToFieldMap=gridToFieldMap,              &
                  totalLWidth=haloWidth(1:griddedDims),     &
                  totalUWidth=haloWidth(1:griddedDims),     &
                  ungriddedLBound=(/1,1/), ungriddedUBound=(/UNGRID(1),UNGRID(2)/), &
                pinflag=pinflag, __RC)
             call ESMF_FieldGet(FIELD, farrayPtr=VAR_4D, __RC)
             VAR_4D = INIT_VALUE
          case default
             __FAIL('only up to 4D are supported')
          end select RankCase2d
       else
          select case (rank)
          case (2)
          call ESMF_FieldEmptyComplete(FIELD, typekind=ESMF_TYPEKIND_R8, &
               gridToFieldMap=gridToFieldMap,                   &
               totalLWidth=haloWidth(1:griddedDims),            &
               totalUWidth=haloWidth(1:griddedDims),            &
                pinflag=pinflag, __RC)
          call ESMF_FieldGet(FIELD, farrayPtr=VR8_2D, __RC)
          VR8_2D = INIT_VALUE
          case (3)
          call ESMF_FieldEmptyComplete(FIELD, typekind=ESMF_TYPEKIND_R8, &
               gridToFieldMap=gridToFieldMap,                   &
               totalLWidth=haloWidth(1:griddedDims),            &
               totalUWidth=haloWidth(1:griddedDims),            &
               ungriddedLBound=(/1/), ungriddedUBound=(/UNGRID(1)/),  &
               pinflag=pinflag, __RC)
          call ESMF_FieldGet(FIELD, farrayPtr=VR8_3D, __RC)
          VR8_3D = INIT_VALUE
          case (4)
          call ESMF_FieldEmptyComplete(FIELD, typekind=ESMF_TYPEKIND_R8, &
               gridToFieldMap=gridToFieldMap,              &
               totalLWidth=haloWidth(1:griddedDims),     &
               totalUWidth=haloWidth(1:griddedDims),     &
               ungriddedLBound=(/1,1/), ungriddedUBound=(/UNGRID(1),UNGRID(2)/), &
               pinflag=pinflag, __RC)
          call ESMF_FieldGet(FIELD, farrayPtr=VR8_4D, __RC)
          VR8_4D = INIT_VALUE
          case default
             __FAIL('only up to 4D are supported')
          end select
       end if

       ! Horz + Vert
       ! -----------
    case(MAPL_DimsHorzVert)
       lb1 = 1-HW
       ub1 = COUNTS(1)+HW
       lb2 = 1-HW
       ub2 = COUNTS(2)+HW
       ub3 = COUNTS(3)

       griddedDims = gridRank - count(gridToFieldMap == 0)
       if (gridRank == 3) gridToFieldMap(3)=0

       rank = 3 + szungrd

       select case(LOCATION)

       case(MAPL_VLocationCenter)
          lb3 = 1
       case(MAPL_VLocationEdge  )
          lb3 = 0
       case default
          __RETURN(ESMF_FAILURE)
       end select

       RankCase3d: select case(rank)
       case (3)
          if (typekind == ESMF_KIND_R4) then
             call ESMF_FieldEmptyComplete(FIELD, typekind=ESMF_TYPEKIND_R4, &
                  gridToFieldMap=gridToFieldMap,              &
                  totalLWidth=haloWidth(1:griddedDims),     &
                  totalUWidth=haloWidth(1:griddedDims),     &
                  ungriddedLBound=(/lb3/), ungriddedUBound=(/ub3/),  &
                  pinflag=pinflag, __RC)
             call ESMF_FieldGet(FIELD, farrayPtr=VAR_3D, __RC)
             VAR_3D = INIT_VALUE
          else
             call ESMF_FieldEmptyComplete(FIELD, typekind=ESMF_TYPEKIND_R8, &
                  gridToFieldMap=gridToFieldMap,              &
                  totalLWidth=haloWidth(1:griddedDims),     &
                  totalUWidth=haloWidth(1:griddedDims),     &
                  ungriddedLBound=(/lb3/), ungriddedUBound=(/ub3/),  &
                  pinflag=pinflag, __RC)
             call ESMF_FieldGet(FIELD, farrayPtr=VR8_3D, __RC)
             VR8_3D = INIT_VALUE
          endif

       case (4)
          if (typekind == ESMF_KIND_R4) then
             call ESMF_FieldEmptyComplete(FIELD, typekind=ESMF_TYPEKIND_R4, &
                  gridToFieldMap=gridToFieldMap,              &
                  totalLWidth=haloWidth(1:griddedDims),     &
                  totalUWidth=haloWidth(1:griddedDims),     &
                  ungriddedLBound=(/lb3,1/), ungriddedUBound=(/ub3,ungrid(1)/),  &
                   pinflag=pinflag, __RC)
             call ESMF_FieldGet(FIELD, farrayPtr=VAR_4D, __RC)
             VAR_4D = INIT_VALUE
          else
             call ESMF_FieldEmptyComplete(FIELD, typekind=ESMF_TYPEKIND_R8, &
                  gridToFieldMap=gridToFieldMap,              &
                  totalLWidth=haloWidth(1:griddedDims),     &
                  totalUWidth=haloWidth(1:griddedDims),     &
                  ungriddedLBound=(/lb3,1/), ungriddedUBound=(/ub3,ungrid(1)/),  &
                   pinflag=pinflag, __RC)
             call ESMF_FieldGet(FIELD, farrayPtr=VR8_4D, __RC)
             VR8_4D = INIT_VALUE
          endif

       case default
          __RETURN(ESMF_FAILURE)
       end select RankCase3d

       ! Tiles
       ! -----
    case(MAPL_DimsTileOnly)
       rank = 1 + szungrd
       __ASSERT(gridRank == 1, 'gridRank /= 1')

       if (typekind == ESMF_KIND_R4) then
          select case (rank)
          case (1)
             allocate(VAR_1D(COUNTS(1)), __STAT)
             VAR_1D = INIT_VALUE
             call ESMF_FieldEmptyComplete(FIELD, farray=VAR_1D,    &
                  indexflag=ESMF_INDEX_DELOCAL, &
                  datacopyFlag = ESMF_DATACOPY_REFERENCE,         &
                  __RC)
          case (2)
             allocate(VAR_2D(COUNTS(1),UNGRID(1)), __STAT)
             VAR_2D = INIT_VALUE
             call ESMF_FieldEmptyComplete(FIELD, farray=VAR_2D,    &
                  indexflag=ESMF_INDEX_DELOCAL, &
                  gridToFieldMap=gridToFieldMap,                      &
                  datacopyFlag = ESMF_DATACOPY_REFERENCE,             &
                  __RC)
          case (3)
             allocate(VAR_3D(COUNTS(1), UNGRID(1), UNGRID(2)), __STAT)
             VAR_3D = INIT_VALUE
             call ESMF_FieldEmptyComplete(FIELD, farray=VAR_3D,    &
                  indexflag=ESMF_INDEX_DELOCAL, &
                  gridToFieldMap=gridToFieldMap,                      &
                  datacopyFlag = ESMF_DATACOPY_REFERENCE,             &
                  __RC)
          case default
             __FAIL( 'only 2D and 3D are supported')
          end select

       else
          select case (rank)
          case (1)
             allocate(VR8_1D(COUNTS(1)), __STAT)
             VR8_1D = INIT_VALUE
             call ESMF_FieldEmptyComplete(FIELD, farray=VR8_1D,    &
                  indexflag=ESMF_INDEX_DELOCAL, &
                  datacopyFlag = ESMF_DATACOPY_REFERENCE,         &
                  __RC)
          case (2)
             allocate(VR8_2D(COUNTS(1),UNGRID(1)), __STAT)
             VR8_2D = INIT_VALUE
             call ESMF_FieldEmptyComplete(FIELD, farray=VR8_2D,    &
                  indexflag=ESMF_INDEX_DELOCAL, &
                  gridToFieldMap=gridToFieldMap,                      &
                  datacopyFlag = ESMF_DATACOPY_REFERENCE,             &
                  __RC)
          case (3)
             allocate(VR8_3D(COUNTS(1), UNGRID(1), UNGRID(2)), __STAT)
             VR8_3D = INIT_VALUE
             call ESMF_FieldEmptyComplete(FIELD, farray=VR8_3D,    &
                  indexflag=ESMF_INDEX_DELOCAL, &
                  gridToFieldMap=gridToFieldMap,                      &
                  datacopyFlag = ESMF_DATACOPY_REFERENCE,             &
                  __RC)
          case default
             __FAIL( 'only 2D and 3D are supported')
          end select

       endif

    case(MAPL_DimsTileTile)
       rank=2
       __ASSERT(gridRank == 1, 'gridRank /= 1')

       if (typekind == ESMF_KIND_R4) then
          allocate(VAR_2D(COUNTS(1), COUNTS(2)), __STAT)
          VAR_2D = INIT_VALUE
          call ESMF_FieldEmptyComplete(FIELD, farray=VAR_2D,    &
               indexflag=ESMF_INDEX_DELOCAL, &
               datacopyFlag = ESMF_DATACOPY_REFERENCE,             &
                                !                  ungriddedLBound = (/1/),                      &
                                !                  ungriddedUBound = (/counts(2)/),              &
               __RC)
       else
          allocate(VR8_2D(COUNTS(1), COUNTS(2)), __STAT)
          VR8_2D = INIT_VALUE
          call ESMF_FieldEmptyComplete(FIELD, farray=VR8_2D,    &
               indexflag=ESMF_INDEX_DELOCAL, &
               datacopyFlag = ESMF_DATACOPY_REFERENCE,             &
                                !                  ungriddedLBound = (/1/),                      &
                                !                  ungriddedUBound = (/counts(2)/),              &
               __RC)
       endif

       ! Invalid dimensionality
       ! ----------------------

    case default
       __RETURN(ESMF_FAILURE)

    end select Dimensionality

    if (present(default_value)) then
       call MAPL_AttributeSet(field, NAME="MAPL_InitStatus", &
            VALUE=MAPL_InitialDefault, __RC)
    end if

    ! Clean up
    deallocate(haloWidth)
    deallocate(gridToFieldMap)


    __RETURN(ESMF_SUCCESS)
  end subroutine MAPL_FieldAllocCommit

  module subroutine MAPL_FieldF90Deallocate(field, rc)
    type(ESMF_Field),  intent(INOUT) :: field
    integer, optional, intent(  OUT) :: rc

    integer                                 :: status
    character(len=ESMF_MAXSTR), parameter   :: IAm='MAPL_FieldF90Deallocate'

    type(ESMF_Array)                        :: array
    type(ESMF_FieldStatus_Flag)             :: fieldStatus
    type (ESMF_LocalArray), target          :: larrayList(1)
    type (ESMF_LocalArray), pointer         :: larray
    integer                                 :: localDeCount
    integer                                 :: rank
    type(ESMF_TypeKind_Flag)                :: tk

    call ESMF_FieldGet(field, status=fieldStatus, __RC)

    if (fieldStatus == ESMF_FIELDSTATUS_COMPLETE) then
       call ESMF_FieldGet(field, Array=array, __RC)

       call ESMF_ArrayGet(array, localDeCount=localDeCount, __RC)
       __ASSERT(localDeCount == 1, 'currently MAPL supports only 1 local array')
       call ESMF_ArrayGet(array, localarrayList=larrayList, __RC)
       larray => lArrayList(1) ! alias

       call ESMF_LocalArrayGet(larray, rank=rank, typekind=tk, &
            __RC)

       call ESMF_LocalArrayF90Deallocate(larray, typekind=tk, rank=rank, __RC)
    end if

    __RETURN(ESMF_SUCCESS)
  end subroutine MAPL_FieldF90Deallocate

  module subroutine MAPL_SetPointer2DR4(state, ptr, name, rc)
    type(ESMF_State),               intent(INOUT) :: state
    real,                           pointer       :: ptr(:,:)
    character(len=*),               intent(IN   ) :: name
    integer,              optional, intent(  OUT) :: rc


    integer                               :: status
    character(len=ESMF_MAXSTR), parameter :: IAm='MAPL_SetPointer2DR4'

    type(ESMF_Field)                      :: field
    type(ESMF_FieldBundle)                :: bundle
    type(ESMF_Grid)                       :: GRID
    integer                               :: COUNTS(ESMF_MAXDIM)
    integer                               :: gridRank
    integer                               :: I
    integer                               :: loc
    integer, allocatable                  :: gridToFieldMap(:)
    type(ESMF_FieldStatus_Flag)           :: fieldStatus

    __ASSERT(associated(ptr), 'unassociated pointer')

    ! Get Field from state

    loc = index(name,';;')

    if(loc/=0) then
       call ESMF_StateGet(state, name(:loc-1), Bundle, __RC)
       call ESMF_StateGet(state, name(loc+2:), Field, __RC)
    else
       call ESMF_StateGet(state, name, Field, __RC)
    end if

    call ESMF_FieldGet(field, status=fieldStatus, __RC)
    __ASSERT(fieldStatus /= ESMF_FIELDSTATUS_COMPLETE, 'fieldStatus == ESMF_FIELDSTATUS_COMPLETE')

    call ESMF_FieldGet(field, grid=GRID, __RC)
    call MAPL_GridGet(GRID, localCellCountPerDim=COUNTS, __RC)

    __ASSERT(size(ptr,1) == COUNTS(1), 'shape mismatch dim=1')
    __ASSERT(size(ptr,2) == COUNTS(2), 'shape mismatch dim=2')
    call ESMF_GridGet(GRID, dimCount=gridRank, __RC)
    ! MAPL restriction (actually only the first 2 dims are distributted)
    __ASSERT(gridRank <= 3, 'gridRank > 3 not supported')
    allocate(gridToFieldMap(gridRank), __STAT)
    do I = 1, gridRank
       gridToFieldMap(I) = I
    end do

    ! this is 2d case
    if (gridRank == 3) gridToFieldMap(3) = 0

    call ESMF_FieldEmptyComplete(FIELD, farrayPtr=ptr, &
         datacopyFlag = ESMF_DATACOPY_REFERENCE,       &
         gridToFieldMap=gridToFieldMap,                &
         __RC)

    ! Clean up
    deallocate(gridToFieldMap)


    __RETURN(ESMF_SUCCESS)
  end subroutine MAPL_SetPointer2DR4

  module subroutine MAPL_SetPointer3DR4(state, ptr, name, rc)
    type(ESMF_State),               intent(INOUT) :: state
    real,                           pointer       :: ptr(:,:,:)
    character(len=*),               intent(IN   ) :: name
    integer,              optional, intent(  OUT) :: rc


    integer                               :: status
    character(len=ESMF_MAXSTR), parameter :: IAm='MAPL_SetPointer3DR4'

    type(ESMF_Field)                      :: field
    type(ESMF_FieldBundle)                :: bundle
    type(ESMF_Grid)                       :: GRID
    integer                               :: COUNTS(ESMF_MAXDIM)
    integer                               :: gridRank
    integer                               :: I
    integer                               :: loc
    integer, allocatable                  :: gridToFieldMap(:)
    type(ESMF_FieldStatus_Flag)             :: fieldStatus

    __ASSERT(associated(ptr), 'unassociated pointer')

    ! Get Field from state

    loc = index(name,';;')

    if(loc/=0) then
       call ESMF_StateGet(state, name(:loc-1), Bundle, __RC)
       call ESMF_StateGet(state, name(loc+2:), Field, __RC)
    else
       call ESMF_StateGet(state, name, Field, __RC)
    end if

    call ESMF_FieldGet(field, status=fieldStatus, __RC)
    __ASSERT(fieldStatus /= ESMF_FIELDSTATUS_COMPLETE, 'fieldStatus == ESMF_FIELDSTATUS_COMPLETE')

    call ESMF_FieldGet(field, grid=GRID, __RC)
    call MAPL_GridGet(GRID, localCellCountPerDim=COUNTS, __RC)

    __ASSERT(size(ptr,1) == COUNTS(1), 'shape mismatch dim=1')
    __ASSERT(size(ptr,2) == COUNTS(2), 'shape mismatch dim=2')
    call ESMF_GridGet(GRID, dimCount=gridRank, __RC)
    ! MAPL restriction (actually only the first 2 dims are distributted)
    __ASSERT(gridRank <= 3, 'gridRank > 3 not supported')
    allocate(gridToFieldMap(gridRank), __STAT)
    do I = 1, gridRank
       gridToFieldMap(I) = I
    end do

    call ESMF_FieldEmptyComplete(FIELD, farrayPtr=ptr, &
         datacopyFlag = ESMF_DATACOPY_REFERENCE,       &
         gridToFieldMap=gridToFieldMap,                &
         __RC)

    ! Clean up
    deallocate(gridToFieldMap)


    __RETURN(ESMF_SUCCESS)
  end subroutine MAPL_SetPointer3DR4

  module subroutine MAPL_DecomposeDim ( dim_world,dim,NDEs, unusable, symmetric, min_DE_extent )
    use MAPL_KeywordEnforcerMod

    integer, intent(in) :: dim_world, NDEs
    integer, intent(out) :: dim(0:NDEs-1)
    class (KeywordEnforcer), optional, intent(in) :: unusable
    logical, intent(in), optional :: symmetric
    integer, optional, intent(in) :: min_DE_extent

    integer    im,rm
    logical :: do_symmetric
    integer :: is,ie,isg,ieg
    integer :: ndiv,ndivs,imax,ndmax,ndmirror,n
    integer :: ibegin(0:NDEs-1)
    integer :: iend(0:NDEs-1)
    logical :: symmetrize
    integer :: NDEs_used

    __UNUSED_DUMMY(unusable)

    if (present(symmetric)) then
       do_symmetric=symmetric
    else
       do_symmetric=.false.
    end if

    if (present(min_DE_extent)) then
       NDEs_used = min(NDEs, dim_world / min_DE_extent)
    else
       NDEs_used = NDEs
    end if

    if (do_symmetric) then

       isg = 1
       ieg = dim_world
       ndivs = NDEs_used

       is = isg
       n = 0
       do ndiv=0,ndivs-1
          !modified for mirror-symmetry
          !original line
          !                 ie = is + CEILING( real(ieg-is+1)/(ndivs-ndiv) ) - 1

          !problem of dividing nx points into n domains maintaining symmetry
          !i.e nx=18 n=4 4554 and 5445 are solutions but 4455 is not.
          !this will always work for nx even n even or odd
          !this will always work for nx odd, n odd
          !this will never  work for nx odd, n even: for this case we supersede the mirror calculation
          !                 symmetrize = .NOT. ( mod(ndivs,2).EQ.0 .AND. mod(ieg-isg+1,2).EQ.1 )
          !nx even n odd fails if n>nx/2
          symmetrize = ( even(ndivs) .AND. even(ieg-isg+1) ) .OR. &
               (  odd(ndivs) .AND.  odd(ieg-isg+1) ) .OR. &
               (  odd(ndivs) .AND. even(ieg-isg+1) .AND. ndivs.LT.(ieg-isg+1)/2 )

          !mirror domains are stored in the list and retrieved if required.
          if( ndiv.EQ.0 )then
             !initialize max points and max domains
             imax = ieg
             ndmax = ndivs
          end if
          !do bottom half of decomposition, going over the midpoint for odd ndivs
          if( ndiv.LT.(ndivs-1)/2+1 )then
             !domain is sized by dividing remaining points by remaining domains
             ie = is + CEILING( REAL(imax-is+1)/(ndmax-ndiv) ) - 1
             ndmirror = (ndivs-1) - ndiv !mirror domain
             if( ndmirror.GT.ndiv .AND. symmetrize )then !only for domains over the midpoint
                !mirror extents, the max(,) is to eliminate overlaps
                ibegin(ndmirror) = max( isg+ieg-ie, ie+1 )
                iend(ndmirror)   = max( isg+ieg-is, ie+1 )
                imax = ibegin(ndmirror) - 1
                ndmax = ndmax - 1
             end if
          else
             if( symmetrize )then
                !do top half of decomposition by retrieving saved values
                is = ibegin(ndiv)
                ie = iend(ndiv)
             else
                ie = is + CEILING( REAL(imax-is+1)/(ndmax-ndiv) ) - 1
             end if
          end if
          dim(ndiv) = ie-is+1
          is = ie + 1
       end do
    else
       im = dim_world/NDEs_used
       rm = dim_world-NDEs_used*im
       do n = 0,NDEs_used-1
          dim(n) = im
          if( n.le.rm-1 ) dim(n) = im+1
       enddo
    end if

    dim(NDEs_used:) = 0

  contains

    logical function even(n)
      integer, intent(in) :: n
      even = mod(n,2).EQ.0
    end function even

    logical function odd(n)
      integer, intent(in) :: n
      odd = mod(n,2).EQ.1
    end function odd

  end subroutine MAPL_DecomposeDim

  module subroutine MAPL_MakeDecomposition(nx, ny, unusable, reduceFactor, rc)
    use ESMF
    use MAPL_KeywordEnforcerMod
    integer, intent(out) :: nx
    integer, intent(out) :: ny
    class (KeywordEnforcer), optional, intent(in) :: unusable
    integer, optional, intent(in) :: reduceFactor
    integer, optional, intent(out) :: rc

    type (ESMF_VM) :: vm
    integer :: pet_count

    character(len=*), parameter :: Iam= __FILE__ // '::MAPL_MakeDecomposition()'
    integer :: status

    __UNUSED_DUMMY(unusable)

    call ESMF_VMGetCurrent(vm, __RC)
    call ESMF_VMGet(vm, petCount=pet_count, __RC)
    if (present(reduceFactor)) pet_count=pet_count/reduceFactor

    ! count down from sqrt(n)
    ! Note: inal iteration (nx=1) is guaranteed to succeed.
    do nx = floor(sqrt(real(2*pet_count))), 1, -1
       if (mod(pet_count, nx) == 0) then ! found a decomposition
          ny = pet_count / nx
          exit
       end if
    end do

    __RETURN(ESMF_SUCCESS)

  end subroutine MAPL_MakeDecomposition

  module subroutine MAPL_Interp_Fac (TIME0, TIME1, TIME2, FAC1, FAC2, RC)

    !------------------------------------------------------------

    !  PURPOSE:
    !  ========
    !
    !    Compute interpolation factors, fac, to be used
    !    in the calculation of the instantaneous boundary
    !    conditions, ie:
    !
    !     q(i,j) = fac1*q1(i,j) + (1.-fac1)*q2(i,j)
    !
    !    where:
    !     q(i,j)  => Boundary Data valid    at time0
    !     q1(i,j) => Boundary Data centered at time1
    !     q2(i,j) => Boundary Data centered at time2

    !  INPUT:
    !  ======
    !    time0    : Time of current timestep
    !    time1    : Time of boundary data 1
    !    time2    : Time of boundary data 2

    !  OUTPUT:
    !  =======
    !     fac1    : Interpolation factor for Boundary Data 1
    !
    ! ------------------------------------------------------------
    !               GODDARD LABORATORY FOR ATMOSPHERES
    ! ------------------------------------------------------------

    type(ESMF_Time),   intent(in ) :: TIME0, TIME1, TIME2
    real,              intent(out) :: FAC1
    real,    optional, intent(out) :: FAC2
    integer, optional, intent(out) :: RC

    type(ESMF_TimeInterval)        :: TimeDif1
    type(ESMF_TimeInterval)        :: TimeDif

    TimeDif1 = TIME2-TIME0
    TimeDif  = TIME2-TIME1

    FAC1 = TimeDif1/TimeDif

    if(present(FAC2)) FAC2 = 1.-FAC1
    if(present(RC  )) RC   = ESMF_SUCCESS

  end subroutine MAPL_Interp_Fac

  module subroutine MAPL_ClimInterpFac (CLOCK,I1,I2,FAC, RC)

    !------------------------------------------------------------

    type(ESMF_CLOCK),  intent(in ) :: CLOCK
    integer,           intent(OUT) :: I1, I2
    real,              intent(out) :: FAC
    integer, optional, intent(out) :: RC

    integer                                 :: STATUS
    character(len=ESMF_MAXSTR), parameter   :: IAm='MAPL_ClimInterpFac'

    type (ESMF_Time)                  :: CurrTime
    type (ESMF_Time)                  :: midMonth
    type (ESMF_Time)                  :: BEFORE, AFTER
    type (ESMF_TimeInterval)          :: oneMonth
    type (ESMF_Calendar)              :: cal

    call ESMF_ClockGet       ( CLOCK,    CurrTime=CurrTime, calendar=cal, __RC )
    call ESMF_TimeGet        ( CurrTime, midMonth=midMonth,               __RC )
    call ESMF_TimeIntervalSet( oneMonth, MM = 1, calendar=cal,            __RC )

    if( CURRTIME < midMonth ) then
       AFTER    = midMonth
       midMonth = midMonth - oneMonth
       call ESMF_TimeGet (midMonth, midMonth=BEFORE, __RC )
    else
       BEFORE   = midMonth
       midMonth = midMonth + oneMonth
       call ESMF_TimeGet (midMonth, midMonth=AFTER , __RC )
    endif

    call MAPL_Interp_Fac( CURRTIME, BEFORE, AFTER, FAC, __RC)

    call ESMF_TimeGet (BEFORE, MM=I1, __RC )
    call ESMF_TimeGet (AFTER , MM=I2, __RC )


    __RETURN(ESMF_SUCCESS)
  end subroutine MAPL_ClimInterpFac


  module subroutine MAPL_TimeStringGet(TIMESTRING,YY,MM,DD,H,M,S)
    character(len=*),  intent (IN ) :: TIMESTRING
    integer, optional, intent (OUT) :: YY
    integer, optional, intent (OUT) :: MM
    integer, optional, intent (OUT) :: DD
    integer, optional, intent (OUT) :: H
    integer, optional, intent (OUT) :: M
    integer, optional, intent (OUT) :: S

    integer :: IYY, IMM, IDD, IHH, IMN, ISS

    read(TIMESTRING,'(I4,1X,I2,1X,I2,1X,I2,1X,I2,1X,I2)') IYY,IMM,IDD,IHH,IMN,ISS

    !ALT: SGI compiler does not like this format  read(TIMESTRING,'(I4,"-",I2,"-",I2,"T",I2,":",I2,":",I2)') IYY,IMM,IDD,IHH,IMN,ISS
    if(present(YY)) YY = IYY
    if(present(MM)) MM = IMM
    if(present(DD)) DD = IDD
    if(present(H )) H  = IHH
    if(present(M )) M  = IMN
    if(present(S )) S  = ISS

    return
  end subroutine MAPL_TimeStringGet


  module subroutine MAPL_UnpackTime(TIME,IYY,IMM,IDD)
    integer, intent (IN ) :: TIME
    integer, intent (OUT) :: IYY
    integer, intent (OUT) :: IMM
    integer, intent (OUT) :: IDD
    IYY = TIME/10000
    IMM = mod(TIME/100,100)
    IDD = mod(TIME,100)
  end subroutine MAPL_UnpackTime


  module subroutine MAPL_PackTime(TIME,IYY,IMM,IDD)
    integer, intent (OUT) :: TIME
    integer, intent (IN ) :: IYY
    integer, intent (IN ) :: IMM
    integer, intent (IN ) :: IDD
    TIME=IYY*10000+IMM*100+IDD
  end subroutine MAPL_PackTime


  module subroutine MAPL_PackDateTime(date_time, yy, mm, dd, h, m, s)
    integer, intent(in) :: yy, mm, dd, h, m, s
    integer, intent(out) :: date_time(:)

    date_time(1) = (10000 * yy) + (100 * mm) + dd
    date_time(2) = (10000 * h) + (100 * m) + s
  end subroutine MAPL_PackDateTime


  module subroutine MAPL_UnpackDateTime(date_time, yy, mm, dd, h, m, s)
    integer, intent(in) :: date_time(:)
    integer, intent(out) :: yy, mm, dd, h, m, s

    yy =     date_time(1) / 10000
    mm = mod(date_time(1), 10000) / 100
    dd = mod(date_time(1), 100)
    h  =     date_time(2) / 10000
    m  = mod(date_time(2), 10000) / 100
    s  = mod(date_time(2), 100)
  end subroutine MAPL_UnpackDateTime


  integer module function MAPL_nsecf(nhms)
    integer, intent(in) :: nhms
    MAPL_nsecf = nhms/10000*3600 + mod(nhms,10000)/100*60 + mod(nhms,100)
  end function MAPL_nsecf

  module subroutine MAPL_tick (nymd,nhms,ndt)
    integer nymd,nhms,ndt,nsec
    IF(NDT.NE.0) THEN
       NSEC = MAPL_NSECF(NHMS) + NDT
       IF (NSEC.GT.86400)  THEN
          DO WHILE (NSEC.GT.86400)
             NSEC = NSEC - 86400
             NYMD = MAPL_INCYMD (NYMD,1)
          ENDDO
       ENDIF
       IF (NSEC.EQ.86400)  THEN
          NSEC = 0
          NYMD = MAPL_INCYMD (NYMD,1)
       ENDIF
       IF (NSEC.LT.00000)  THEN
          DO WHILE (NSEC.LT.0)
             NSEC = 86400 + NSEC
             NYMD = MAPL_INCYMD (NYMD,-1)
          ENDDO
       ENDIF
       NHMS = MAPL_NHMSF (NSEC)
    ENDIF
    RETURN
  end subroutine MAPL_tick

  integer module function MAPL_nhmsf (nsec)
    implicit none
    integer  nsec
    MAPL_nhmsf =  nsec/3600*10000 + mod(nsec,3600)/60*100 + mod(nsec,60)
  end function MAPL_nhmsf

  ! A year is a leap year if
  ! 1) it is divible by 4, and
  ! 2) it is not divisible by 100, unless
  ! 3) it is also divisible by 400.
  logical module function MAPL_LEAP(NY)
    integer, intent(in) :: NY

    MAPL_LEAP = mod(NY,4)==0 .and. (mod(NY,100)/=0 .or. mod(NY,400)==0)

  end function MAPL_LEAP


  integer module function MAPL_incymd (NYMD,M)
    integer nymd,ny,nm,nd,m
    INTEGER NDPM(12)
    DATA    NDPM /31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/
    NY = NYMD / 10000
    NM = MOD(NYMD,10000) / 100
    ND = MOD(NYMD,100) + M
    IF (ND.EQ.0) THEN
       NM = NM - 1
       IF (NM.EQ.0) THEN
          NM = 12
          NY = NY - 1
       ENDIF
       ND = NDPM(NM)
       IF (NM.EQ.2 .AND. MAPL_LEAP(NY))  ND = 29
    ENDIF
    IF (ND.EQ.29 .AND. NM.EQ.2 .AND. MAPL_LEAP(NY))  GO TO 20
    IF (ND.GT.NDPM(NM)) THEN
       ND = 1
       NM = NM + 1
       IF (NM.GT.12) THEN
          NM = 1
          NY = NY + 1
       ENDIF
    ENDIF
20  CONTINUE
    MAPL_INCYMD = NY*10000 + NM*100 + ND
    RETURN
  end function MAPL_incymd


  module subroutine MAPL_PICKEM(II,JJ,IM,JM,COUNT)
    integer, intent(IN ) :: IM, JM, COUNT
    integer, intent(OUT) :: II(COUNT), JJ(COUNT)

    integer, parameter :: NT=3

    logical :: MASK(IM,JM)
    integer :: L, NN, IX, JX
    real    :: IIR(NT*COUNT), JJR(NT*COUNT)

    MASK=.true.

    NN=1

    call RANDOM_NUMBER(IIR)
    call RANDOM_NUMBER(JJR)


    do L=1, COUNT

       do
          IX=IIR(NN)*(IM-1)+2
          JX=JJR(NN)*(JM-2)+2

          NN = NN + 1

          if(MASK(IX,JX)) then
             II(L) = IX
             JJ(L) = JX
             MASK(IX-1:IX+1,JX-1:JX+1) = .false.
             exit
          endif

          if(NN>NT*COUNT) stop 222

       enddo
    enddo

!!$   DO L=1,JM
!!$      PRINT '(144L1)',MASK(:,L)
!!$   ENDDO
!!$
!!$   PRINT *, COUNT, NN

    return
  end subroutine MAPL_PICKEM




  module subroutine MAPL_GetFieldTimeFromField ( FIELD, TIME, RC )
    type(ESMF_Field),        intent(INOUT) :: FIELD ! ALT: IN
    type(ESMF_Time),         intent(  OUT) :: TIME
    integer, optional,       intent(  OUT) :: RC

    character(len=ESMF_MAXSTR),parameter   :: IAm=" MAPL_GetFieldTimeFromField"
    integer                                :: STATUS

    integer                                :: YEAR, MONTH, DAY
    integer                                :: HOUR, MINUTE, SCND
    character(len=ESMF_MAXSTR)             :: TIMESTAMP
    logical                                :: isPresent

    call ESMF_AttributeGet(FIELD, NAME="TimeStamp", isPresent=isPresent, __RC)
    if(.not. isPresent) then
       call ESMF_TimeSet          (TIME,      YY=0,                __RC)
    else
       call ESMF_AttributeGet(FIELD, NAME="TimeStamp", VALUE=TIMESTAMP, __RC)

       call MAPL_TimeStringGet    (TIMESTAMP, YY=YEAR, MM=MONTH,  DD=DAY,   &
            H =HOUR, M =MINUTE, S =SCND   )
       call ESMF_TimeSet          (TIME,      YY=YEAR, MM=MONTH,  DD=DAY,   &
            H =HOUR, M =MINUTE, S =SCND,  &
            __RC)
    end if

    __RETURN(ESMF_SUCCESS)
  end subroutine MAPL_GetFieldTimeFromField

  ! ------------------------------------------------------------------------------

  module subroutine  MAPL_SetFieldTimeFromField (FIELD, TIME, RC )
    type(ESMF_FIELD),        intent(INOUT) :: FIELD
    type(ESMF_TIME),         intent(INOUT) :: TIME !ALT: IN
    integer, optional,       intent(  OUT) :: RC

    character(len=ESMF_MAXSTR),parameter   :: IAm=" MAPL_SetFieldTimeFromField"
    integer                                :: STATUS

    character(len=ESMF_MAXSTR)             :: TIMESTAMP

    call ESMF_TimeGet          (TIME,  timeString=TIMESTAMP,             __RC)
    call ESMF_AttributeSet(FIELD, NAME="TimeStamp", VALUE=TIMESTAMP, __RC)

    __RETURN(ESMF_SUCCESS)
  end subroutine  MAPL_SetFieldTimeFromField


  module subroutine  MAPL_GetFieldTimeFromState ( STATE, Fieldname, TIME, RC )
    type(ESMF_STATE),        intent(IN   ) :: STATE
    character(len=*),        intent(IN   ) :: Fieldname
    type(ESMF_Time),         intent(  OUT) :: TIME
    integer, optional,       intent(  OUT) :: RC

    character(len=ESMF_MAXSTR),parameter   :: IAm=" MAPL_GetFieldTimeFromState"
    integer                                :: STATUS

    type(ESMF_FIELD)                       :: FIELD

    call ESMF_StateGet (STATE, FIELDNAME, FIELD, __RC )
    call MAPL_FieldGetTime  (FIELD, TIME,             __RC)

    __RETURN(ESMF_SUCCESS)
  end subroutine  MAPL_GetFieldTimeFromState

  ! ------------------------------------------------------------------------------

  module subroutine  MAPL_SetFieldTimeFromState ( STATE, Fieldname, TIME, RC )
    type(ESMF_STATE),        intent(INOUT) :: STATE
    character(len=*),        intent(IN   ) :: Fieldname
    type(ESMF_Time),         intent(INOUT) :: TIME !ALT: IN
    integer, optional,       intent(  OUT) :: RC

    character(len=ESMF_MAXSTR),parameter   :: IAm=" MAPL_SetFieldTimeFromState"
    integer                                :: STATUS

    type(ESMF_FIELD)                       :: FIELD

    call ESMF_StateGet (STATE, FIELDNAME, FIELD, __RC)
    call MAPL_FieldSetTime  (FIELD, TIME,             __RC)

    __RETURN(ESMF_SUCCESS)
  end subroutine  MAPL_SetFieldTimeFromState


  module function MAPL_FieldCreateRename(FIELD, NAME, DoCopy, RC) RESULT(F)
    type (ESMF_Field), intent(INOUT) :: FIELD !ALT: IN
    character(len=*),  intent(IN   ) :: NAME
    logical, optional, intent(IN   ) :: DoCopy
    integer, optional, intent(  OUT) :: RC
    type (ESMF_Field)                :: F

    !   we are creating new field so that we can change the name of the field;
    !   the important thing is that the data (ESMF_Array) and the grid (ESMF_Grid)
    !   are the SAME as the one in the original Field, if DoCopy flag is present
    !   and set to true we create a new array and copy the data, not just reference it

    integer                 :: status
    character(len=ESMF_MAXSTR), parameter :: Iam='MAPL_FieldCreateRename'
    logical                 :: DoCopy_
    type(ESMF_DataCopy_Flag):: datacopy

    DoCopy_ = .false.
    if (present(DoCopy) ) then
       DoCopy_ = DoCopy
    end if

    if (doCopy_) then
       datacopy = ESMF_DATACOPY_VALUE
    else
       datacopy = ESMF_DATACOPY_REFERENCE
    end if

    f = ESMF_FieldCreate(field, datacopyflag=datacopy, name=NAME, __RC)

    call MAPL_FieldCopyAttributes(FIELD_IN=field, FIELD_OUT=f, __RC)

    __RETURN(ESMF_SUCCESS)
  end function MAPL_FieldCreateRename

  module function MAPL_FieldCreateNewgrid(FIELD, GRID, LM, NEWNAME, RC) RESULT(F)
    type (ESMF_Field), intent(INOUT) :: FIELD !ALT: intent(IN)
    type (ESMF_Grid),  intent(INout) :: GRID
    integer, optional, intent(IN   ) :: lm
    character(len=*), optional, intent(IN) :: newName
    integer, optional, intent(  OUT) :: RC
    type (ESMF_Field)                :: F

    !   we are creating new field so that we can change the grid of the field
    !   (and allocate array accordingly);
    !ALT: This function is currently used only in History for regridding on an output grid

    !ALT halowidth assumed 0

    !      type(ESMF_FieldDataMap) :: datamap
    type (ESMF_Grid)        :: fGRID
    type(ESMF_Array)        :: array
    type (ESMF_LocalArray), target          :: larrayList(1)
    type (ESMF_LocalArray), pointer         :: larray
    integer                                 :: localDeCount
    integer                 :: rank
    integer                 :: newRank
    integer                 :: COUNTS(3)
    !real, pointer           :: VAR_2D(:,:), VAR_3D(:,:,:), VAR_4D(:,:,:,:)
    character(len=ESMF_MAXSTR) :: NAME
    integer                 :: status
    integer                 :: DIMS
    integer                 :: I
    integer, allocatable    :: gridToFieldMap(:)
    integer                 :: gridRank
    integer                 :: fgridRank
    integer                 :: griddedDims
    integer                 :: ungriddedDims
    integer                 :: lb, ub
    integer                 :: lbnds(ESMF_MAXDIM), ubnds(ESMF_MAXDIM)
    character(len=ESMF_MAXSTR) :: newName_
    character(len=ESMF_MAXSTR), parameter :: Iam='MAPL_FieldCreateNewgrid'
    real, pointer :: ptr1d(:)

    call ESMF_FieldGet(FIELD, grid=fgrid, __RC)

    call ESMF_GridGet(fGRID, dimCount=fgridRank, __RC)
    allocate(gridToFieldMap(fgridRank), __STAT)
    call ESMF_FieldGet(FIELD, Array=Array, name=name, &
         gridToFieldMap=gridToFieldMap, __RC)
    griddedDims = fgridRank - count(gridToFieldMap == 0)

    call ESMF_GridGet(GRID, dimCount=gridRank, __RC)

    call ESMF_ArrayGet(array, rank=rank, __RC)
    ungriddedDims = rank - griddedDims

    call MAPL_GridGet(GRID, localCellCountPerDim=COUNTS, __RC)

    call ESMF_ArrayGet(array, localDeCount=localDeCount, __RC)
    __ASSERT(localDeCount == 1, 'MAPL supports only 1 local array')
    call ESMF_ArrayGet(array, localarrayList=larrayList, __RC)
    larray => lArrayList(1) ! alias

    call ESMF_LocalArrayGet(larray, totalLBound=lbnds, totalUBound=ubnds, __RC)

    newRank = rank
    if (griddedDims == 1 .and. gridRank > 1) then
       deallocate(gridToFieldMap)
       allocate(gridToFieldMap(gridRank), __STAT)
       gridToFieldMap = 0
       do I = 1, 2
          gridToFieldMap(I) = I
       end do
       newRank = rank + 1
    end if

    if (present(newName)) then
       newName_=newName
    else
       newName_=name
    end if

    if (newRank == 2) then
       F = ESMF_FieldCreate(GRID, typekind=ESMF_TYPEKIND_R4, &
            indexflag=ESMF_INDEX_DELOCAL, &
            name=newName_, gridToFieldMap=gridToFieldMap, __RC )
       DIMS = MAPL_DimsHorzOnly
    else if (newRank == 3) then

       if (present(lm)) then
          lb=1
          ub=lm
       else
          lb = lbnds(griddedDims+1)
          ub = ubnds(griddedDims+1)
       end if
       F = ESMF_FieldCreate(GRID, typekind=ESMF_TYPEKIND_R4, &
            indexflag=ESMF_INDEX_DELOCAL, &
            name=newName_, gridToFieldMap=gridToFieldMap, &
            ungriddedLBound=[lb],ungriddedUBound=[ub],__RC )
       if (ungriddedDims > 0) then
          DIMS = MAPL_DimsHorzOnly
       else
          DIMS = MAPL_DimsHorzVert
       end if
    else if (newRank == 4) then
       F = ESMF_FieldCreate(GRID, typekind=ESMF_TYPEKIND_R4, &
            indexflag=ESMF_INDEX_DELOCAL, &
            name=newName_, gridToFieldMap=gridToFieldMap, &
            ungriddedLBound=[lbnds(griddedDims+1),lbnds(griddedDims+2)], &
            ungriddedUBound=[ubnds(griddedDims+1),ubnds(griddedDims+2)],__RC )
       if (ungriddedDims > 0) then
          DIMS = MAPL_DimsHorzOnly
       else
          DIMS = MAPL_DimsHorzVert
       end if
    else
       __FAIL( 'rank > 4 not supported')
    end if

    deallocate(gridToFieldMap)

    call MAPL_FieldCopyAttributes(FIELD_IN=field, FIELD_OUT=f, __RC)
    ! we are saving DIMS attribute in case the FIELD did not contain one
    ! otherwise we will overwrite it
    call ESMF_AttributeSet(F, NAME='DIMS', VALUE=DIMS, __RC)

    call assign_fptr(f, ptr1d, __RC)
    ptr1d = 0.0    

    __RETURN(ESMF_SUCCESS)
  end function MAPL_FieldCreateNewgrid

  module function MAPL_FieldCreateR4(FIELD, RC) RESULT(F)
    type (ESMF_Field), intent(INOUT) :: FIELD !ALT: IN
    integer, optional, intent(  OUT) :: RC
    type (ESMF_Field)                :: F

    !   we are creating new field so that we can change the name of the field;
    !   the important thing is that the data (ESMF_Array) and the grid (ESMF_Grid)
    !   are the SAME as the one in the original Field, if DoCopy flag is present
    !   and set to true we create a new array and copy the data, not just reference it

    type(ESMF_Grid)                  :: grid
    character(len=ESMF_MAXSTR)       :: fieldName
    integer, allocatable    :: gridToFieldMap(:)
    integer                 :: fieldRank
    integer                 :: gridRank
    integer                 :: status
    character(len=ESMF_MAXSTR), parameter :: Iam='MAPL_FieldCreateR4'
    type(ESMF_DataCopy_Flag):: datacopy
    real, pointer           :: var_1d(:)
    real, pointer           :: var_2d(:,:)
    real, pointer           :: var_3d(:,:,:)
    real(kind=REAL64), pointer :: vr8_1d(:)
    real(kind=REAL64), pointer :: vr8_2d(:,:)
    real(kind=REAL64), pointer :: vr8_3d(:,:,:)
    type(ESMF_TypeKind_Flag)  :: tk

    call ESMF_FieldGet(FIELD, grid=GRID, dimCount=fieldRank, &
         name=fieldName, typekind=tk, __RC)
    __ASSERT(tk == ESMF_TypeKind_R8, 'tk /= ESMF_TypeKind_R8')
    call ESMF_GridGet(GRID, dimCount=gridRank, __RC)
    allocate(gridToFieldMap(gridRank), __STAT)
    call ESMF_FieldGet(FIELD, gridToFieldMap=gridToFieldMap, __RC)

    datacopy = ESMF_DATACOPY_REFERENCE

    select case (fieldRank)
    case (1)
       call ESMF_FieldGet(field, farrayPtr=vr8_1d, __RC)
       allocate(var_1d(lbound(vr8_1d,1):ubound(vr8_1d,1)), __STAT)
       var_1d=vr8_1d
       f = MAPL_FieldCreateEmpty(name=fieldNAME, grid=grid, __RC)
       call ESMF_FieldEmptyComplete(F, farrayPtr=VAR_1D,    &
            gridToFieldMap=gridToFieldMap,                      &
            datacopyFlag = datacopy,             &
            __RC)
    case (2)
       call ESMF_FieldGet(field, farrayPtr=vr8_2d, __RC)
       allocate(var_2d(lbound(vr8_2d,1):ubound(vr8_2d,1), &
            lbound(vr8_2d,2):ubound(vr8_2d,2)), &
            __STAT)
       var_2d=vr8_2d
       f = MAPL_FieldCreateEmpty(name=fieldNAME, grid=grid, __RC)
       call ESMF_FieldEmptyComplete(F, farrayPtr=VAR_2D,    &
            gridToFieldMap=gridToFieldMap,                      &
            datacopyFlag = datacopy,             &
            __RC)
    case (3)
       call ESMF_FieldGet(field, farrayPtr=vr8_3d, __RC)
       allocate(var_3d(lbound(vr8_3d,1):ubound(vr8_3d,1), &
            lbound(vr8_3d,2):ubound(vr8_3d,2), &
            lbound(vr8_3d,3):ubound(vr8_3d,3)), &
            __STAT)
       var_3d=vr8_3d
       f = MAPL_FieldCreateEmpty(name=fieldNAME, grid=grid, __RC)
       call ESMF_FieldEmptyComplete(F, farrayPtr=VAR_3D,    &
            gridToFieldMap=gridToFieldMap,                      &
            datacopyFlag = datacopy,             &
            __RC)
    case default
       __FAIL( 'only 2D and 3D are supported')
    end select

    deallocate(gridToFieldMap)

    call MAPL_FieldCopyAttributes(FIELD_IN=field, FIELD_OUT=f, __RC)

    __RETURN(ESMF_SUCCESS)
  end function MAPL_FieldCreateR4

  module function MAPL_FieldCreateEmpty(NAME, GRID, RC) RESULT(FIELD)
    character(len=*),  intent(IN   ) :: NAME
    type (ESMF_Grid),  intent(INout) :: GRID
    integer, optional, intent(  OUT) :: RC
    type (ESMF_Field)                :: FIELD

    character(len=ESMF_MAXSTR),parameter   :: IAm=" MAPL_FieldCreateEmpty"
    integer                                :: STATUS

    FIELD = ESMF_FieldEmptyCreate(name=name, __RC)

    call ESMF_FieldEmptySet(FIELD, &
         grid=GRID, &
         staggerloc = ESMF_STAGGERLOC_CENTER,        &
         __RC)

    __RETURN(ESMF_SUCCESS)

  end function MAPL_FieldCreateEmpty

  module subroutine MAPL_FieldCopyAttributes(FIELD_IN, FIELD_OUT, RC)
    type (ESMF_Field), intent(INOUT) :: FIELD_IN !ALT: intent(in)
    type (ESMF_Field), intent(INOUT) :: FIELD_OUT
    integer, optional, intent(  OUT) :: RC
    integer                          :: status

    call ESMF_AttributeCopy(field_in, field_out, attcopy=ESMF_ATTCOPY_VALUE, __RC)
    __RETURN(ESMF_SUCCESS)
  end subroutine MAPL_FieldCopyAttributes

  module subroutine MAPL_FieldCopy(from, to, RC)
    type (ESMF_Field), intent(INOUT) :: FROM !ALT: IN
    type (ESMF_Field), intent(INOUT) :: TO !ALT: OUT
    integer, optional, intent(  OUT) :: RC

    !   we are creating new field so that we can change the name of the field;
    !   the important thing is that the data (ESMF_Array) and the grid (ESMF_Grid)
    !   are the SAME as the one in the original Field, if DoCopy flag is present
    !   and set to true we create a new array and copy the data, not just reference it

    integer                 :: fieldRank
    integer                 :: status
    character(len=ESMF_MAXSTR), parameter :: Iam='MAPL_FieldCopy'
    real, pointer           :: var_1d(:)
    real, pointer           :: var_2d(:,:)
    real, pointer           :: var_3d(:,:,:)
    real(kind=REAL64), pointer :: vr8_1d(:)
    real(kind=REAL64), pointer :: vr8_2d(:,:)
    real(kind=REAL64), pointer :: vr8_3d(:,:,:)
    type(ESMF_TypeKind_Flag)  :: tk

    call ESMF_FieldGet(from, dimCount=fieldRank, &
         typekind=tk, __RC)
    __ASSERT(tk == ESMF_TypeKind_R8, 'inconsistent typekind (should be ESMF_TypeKind_R8)')

    select case (fieldRank)
    case (1)
       call ESMF_FieldGet(from, farrayPtr=vr8_1d, __RC)
       call ESMF_FieldGet(to, dimCount=fieldRank, typekind=tk, __RC)
       __ASSERT(tk == ESMF_TypeKind_R4, 'inconsistent typekind (should be ESMF_TypeKind_R4)')
       __ASSERT(fieldRank==1, 'inconsistent fieldrank (should be 1)')
       call ESMF_FieldGet(to, farrayPtr=var_1d, __RC)
       var_1d = vr8_1d
    case (2)
       call ESMF_FieldGet(from, farrayPtr=vr8_2d, __RC)
       call ESMF_FieldGet(to, dimCount=fieldRank, typekind=tk, __RC)
       __ASSERT(tk == ESMF_TypeKind_R4, 'inconsistent typekind (should be ESMF_TypeKind_R4)')
       __ASSERT(fieldRank==2, 'inconsistent fieldRank (should be 2)')
       call ESMF_FieldGet(to, farrayPtr=var_2d, __RC)
       var_2d = vr8_2d
    case (3)
       call ESMF_FieldGet(from, farrayPtr=vr8_3d, __RC)
       call ESMF_FieldGet(to, dimCount=fieldRank, typekind=tk, __RC)
       __ASSERT(tk == ESMF_TypeKind_R4, 'inconsistent typekind (should be ESMF_TypeKind_R4)')
       __ASSERT(fieldRank==3,'inconsistent fieldRank (should be 3)')
       call ESMF_FieldGet(to, farrayPtr=var_3d, __RC)
       var_3d = vr8_3d
    case default
       __FAIL( 'unsupported fieldRank (> 3)')
    end select

    __RETURN(ESMF_SUCCESS)
  end subroutine MAPL_FieldCopy


  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  module function MAPL_RemapBounds_3dr4(A, LB1, LB2, LB3) result(ptr)
    integer,      intent(IN) :: LB1, LB2, LB3
    real, target, intent(IN) :: A(LB1:,LB2:,LB3:)
    real, pointer            :: ptr(:,:,:)

    ptr => A
  end function MAPL_RemapBounds_3dr4

  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  module function MAPL_RemapBounds_3dr8(A, LB1, LB2, LB3) result(ptr)
    integer,      intent(IN) :: LB1, LB2, LB3
    real(kind=REAL64), target, intent(IN) :: A(LB1:,LB2:,LB3:)
    real(kind=REAL64), pointer            :: ptr(:,:,:)

    ptr => A
  end function MAPL_RemapBounds_3dr8

  module subroutine MAPL_GRID_INTERIOR(GRID,I1,IN,J1,JN)
    type (ESMF_Grid), intent(IN) :: grid
    integer, intent(OUT)         :: I1, IN, J1, JN

    ! local vars
    integer                               :: status
    !    character(len=ESMF_MAXSTR)            :: IAm='MAPL_Grid_Interior'

    type (ESMF_DistGrid)                  :: distGrid
    type(ESMF_DELayout)                   :: LAYOUT
    integer,               allocatable    :: AL(:,:)
    integer,               allocatable    :: AU(:,:)
    integer                               :: nDEs,localDECount
    integer                               :: deId
    integer                               :: gridRank
    integer,               allocatable    :: localDeToDeMap(:)
    integer :: rc
    logical :: isPresent
    integer, allocatable  :: global_grid_info(:)
    integer :: itemCount

    i1=-1
    j1=-1
    in=-1
    jn=-1

    call ESMF_AttributeGet(grid, name="GLOBAL_GRID_INFO", isPresent=isPresent, __RC)
    if (isPresent) then
      call ESMF_AttributeGet(grid, name="GLOBAL_GRID_INFO", itemCount=itemCount, __RC)
      allocate(global_grid_info(itemCount), __STAT)
      call ESMF_AttributeGet(grid, name="GLOBAL_GRID_INFO", valueList=global_grid_info, __RC)
      I1 = global_grid_info(7)
      IN = global_grid_info(8)
      j1 = global_grid_info(9)
      JN = global_grid_info(10)
      deallocate(global_grid_info, __STAT)
      __RETURN(__SUCCESS)
    end if

    call ESMF_GridGet    (GRID, dimCount=gridRank, distGrid=distGrid, __RC)
    call ESMF_DistGridGet(distGRID, delayout=layout, __RC)
    call ESMF_DELayoutGet(layout, deCount = nDEs, localDeCount=localDeCount,__RC)
    if (localDeCount > 0) then
       allocate(localDeToDeMap(localDeCount),__STAT)
       call ESMF_DELayoutGet(layout, localDEtoDeMap=localDeToDeMap,__RC)
       deId=localDeToDeMap(1)

       allocate (AL(gridRank,0:nDEs-1),  __STAT)
       allocate (AU(gridRank,0:nDEs-1),  __STAT)

       call MAPl_DistGridGet(distgrid, &
            minIndex=AL, maxIndex=AU, __RC)

       I1 = AL(1, deId)
       IN = AU(1, deId)
       !    __ASSERT(gridRank > 1, 'tilegrid is 1d (without RC this only for info')
       J1 = AL(2, deId)
       JN = AU(2, deId)
       deallocate(AU, AL, localDeToDeMap)
    end if

  end subroutine MAPL_GRID_INTERIOR

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!>
! `MAPL_LatLonGridCreate` creates regular Lat/Lon Grid.
!
! This routine creates a distributed ESMF grid where the horizontal
!  coordinates are regular longitudes and latitudes. The grid is
!  created on the user specified  **VM**, or on the current VM if the user
!  does not specify one. The layout and the coordinate information can
!  be provided with a `ESMF_Config attribute`, a resource file name
!  or specified through the argument list.
!
!### Using resource files
! The {\bf resource file} {\tt ConfigFile} has a syntax similar to a GrADS
! control file.  Here is an example defining a typical GEOS-5 1x1.25
! grid with 72 layers:
!%
!```
!    GDEF: LatLon
!    IDEF: 32
!    JDEF: 16
!    LDEF:  1
!    XDEF: 288 LINEAR -180. 1.25
!    YDEF: 181 LINEAR -90. 1.
!    ZDEF:  72 LINEAR 1 1
!```
!
! More generally,
!```
!    GDEF: LatLon
!    IDEF: Nx
!    JDEF: Ny
!    LDEF: Nz
!    XDEF: IM_World XCoordType BegLon, DelLon
!    YDEF: JM_World YCoordType BegLat, DelLat
!    ZDEF: LM_World ZCoordType 1        1
!```
!
! The attribute **GDEF** must always be *LatLon* for  Lat/Lon grids.
! The remaining parameters are:
!
!- **Nx** is the number of processors used to decompose the X dimension
!- **Ny** is the number of processors used to decompose the Y dimension
!- **Nz** is the number of processors used to decompose the Z dimension;  must be 1 for now.
!- **IM_World** is the number of longitudinal grid points; if `IM_World=0` then the
!   grid has no zonal dimension.
!- **XCoordType** must be set to LINEAR
!- **BegLon** is the longitude (in degrees) of the *center* of the first
!   gridbox
!- **DelLon** is the constant mesh size (in degrees); if `DelLon<1` then a
!   global grid is assumed.
!- **JM_World** is the number of meridional grid points if `JM_World=0` then
!   the grid has no meridional dimension.
!- **YCoordType** must be set to LINEAR
!- **BegLat** is the latitude (in degrees) of the *center* of the first
!   gridbox
!- **DelLat** is the constant mesh size (in degrees); if `DelLat<1` then a
!   global grid is assumed.
!- **LM_World** is the number of vertical grid points; if `LM_World=0` then the
!   grid has no vertical dimension.
!
! As of this writing, only the size of the vertical grid ({\tt LM\_World})
! needs to be specified.
!
!### Passing an ESMF Config
!
! The **ESMF_Config** object `Config`, when specified, must
! contain the same information as the resource file above.
!
!### Providing parameters explicitly through the argument list
!
! Alternatively, one can specify coordinate information in the argument
! list; their units and meaning is as in the resource file above. In
! this case you must specify at least `Nx, Ny, IM_World, JM_World`, and
! `LM\_World`. The other parameters have default values
!- **BegLon** defaults to -180. (the date line)
!- **DelLon** defaults to -1. (meaning a global grid)
!- **BegLat** defaults to -90. (the south pole)
!- **DelLat** deaults to -1. (meaning a global grid)
!
!### Restrictions
! The current implementation imposes the following restrictions:
!1. Only uniform longitude/latitude grids are supported (no Gaussian grids).
!2. Only 2D Lon-Lat or 3D Lon-Lat-Lev grids are currently supported
!   (no Lat-Lev or Lon-Lev grids supprted yet).
!3. No vertical decomposition yet (`Nz=1`).
!
!### Future enhancements
! The `IDEF/JDEF/LDEF` records in the resource file should be
! extended as to allow specification of a more general distribution.
! For consistency with the `XDEF/YDEF/ZDEF` records a similar
! syntax could be adopted. For example,
!
!```
! IDEF 4   LEVELS  22 50 50 22
! XDEF 144 LINEAR -180 2.5
!```
! would indicate that longitudes would be decomposed in 4 PETs,
! with the first PET having 22 grid points, the second 50 gridpoints,
! and so on.
!
  module function MAPL_LatLonGridCreate (Name, vm,                 &
       Config, ConfigFile,       &
       Nx, Ny,                   &
       IM_World, BegLon, DelLon, &
       JM_World, BegLat, DelLat, &
       LM_World,                 &
       rc)                       &
       result(Grid)

    ! !INPUT PARAMETERS:

    character(len=*),            intent(in)  :: Name
    type (ESMF_VM),    OPTIONAL, target,     &
         intent(in)  :: VM


    !   There are 3 possibilities to provide the coordinate information:

    ! 1) Thru Config object:
    type(ESMF_Config), OPTIONAL, target,     &
         intent(in)  :: Config

    ! 2) Thru a resource file:
    character(len=*),  OPTIONAL, intent(in)  :: ConfigFile


    ! 3) Thru argument list:
    integer,           OPTIONAL, intent(in)  :: Nx, Ny          ! Layout
    integer,           OPTIONAL, intent(in)  :: IM_World        ! Zonal
    real,              OPTIONAL, intent(in)  :: BegLon, DelLon  ! in degrees

    integer,           OPTIONAL, intent(in)  :: JM_World        ! Meridional
    real,              OPTIONAL, intent(in)  :: BegLat, DelLat  ! in degrees

    integer,           OPTIONAL, intent(in)  :: LM_World        ! Vertical

    ! !OUTPUT PARAMETERS:

    type (ESMF_Grid)                         :: Grid  ! Distributed grid
    integer,           OPTIONAL, intent(out) :: rc    ! return code

    !   Internal version of the input arguments
    !   ---------------------------------------
    type(ESMF_Config), pointer :: Config_
    integer           :: IM_World_
    real(kind=REAL64) :: BegLon_
    real(kind=REAL64) :: DelLon_
    integer           :: JM_World_
    real(kind=REAL64) :: BegLat_
    real(kind=REAL64) :: DelLat_
    integer           :: LM_World_
    integer           :: Nx_, Ny_, Nz_

    integer, allocatable            :: IMs(:), JMs(:), LMs(:)
    real(ESMF_KIND_R8)              :: minCoord(3)
    real(ESMF_KIND_R8)              :: deltaX, deltaY
    type (ESMF_VM), pointer         :: VM_
    integer                         :: I, J, I1, IN, J1, JN

    real(ESMF_KIND_R8), pointer     :: centerX(:,:)
    real(ESMF_KIND_R8), pointer     :: centerY(:,:)
    real(ESMF_KIND_R8), allocatable :: cornerX(:)
    real(ESMF_KIND_R8), allocatable :: cornerY(:)

    real                            :: FirstOut(2)
    real                            :: LastOut(2)

    integer                         :: STATUS

    !                                ------

    !  Defaults
    !  --------
    BegLon_ = -180.0  ! centered at date line
    DelLon_ =   -1.0  ! means global grid
    BegLat_ =  -90.0  ! centered at south pole
    DelLat_ =   -1.0  ! means global grid
    Nz_     =  1      ! place holder for now

    !  Either user specified VM or current one
    !  ---------------------------------------
    if ( present(vm) ) then
       vm_ => vm
    else
       allocate(vm_, __STAT)
       call ESMF_VMGetCurrent(vm_, __RC)
    end if

    ! Grid info via resources
    ! -----------------------
    if ( present(Config) .or. present(ConfigFile) ) then

       !    Either use supplied Config or load resource file
       !    ------------------------------------------------
       if ( present(ConfigFile) ) then
          allocate(Config_,__STAT)
          Config_ = ESMF_ConfigCreate (__RC )
          call ESMF_ConfigLoadFile (Config_, ConfigFile, __RC )
       else if ( present(Config) ) then
          Config_ => Config
       else
          STATUS = 100
       end if

       !    Get relevant parameters from Config
       !    -----------------------------------
       call parseConfig_()                            ! internal routine

       !  Grid info thru argument list
       !  ----------------------------
    else if ( present(IM_World) .AND. &
         present(JM_World) .AND. &
         present(LM_World) .AND. &
         present(Nx)       .AND. &
         present(Ny)             ) then

       IM_World_ = IM_World
       JM_World_ = JM_World
       LM_World_ = LM_World

       Nx_ = Nx
       Ny_ = Ny

       if ( present(BegLon) ) BegLon_ = BegLon
       if ( present(DelLon) ) DelLon_ = DelLon
       if ( present(BegLat) ) BegLat_ = BegLat
       if ( present(DelLat) ) DelLat_ = DelLat

       continue  ! all is well

       !  Something is missing
       !  --------------------
    else

       STATUS = 300

    end if

    !  Global grids
    !  ------------
    if ( IM_World_ < 1 .OR. JM_World_ < 1 ) then
       STATUS = 400
    end if
    if ( DelLon_ < 0.0 ) then  ! convention for global grids
       if ( IM_World_ == 1 ) then
          DelLon_ = 0.0
       else
          DelLon_ = 360.d0 / IM_World_
       end if
    end if
    if ( DelLat_ < 0.0 ) then  ! convention for global grids
       if ( JM_World_ == 1 ) then
          DelLat_ = 0.0
       else
          DelLat_ = 180.d0 / ( JM_World_ - 1)
       end if
    end if

    !  Give the IMs, JMs and LMs the MAPL default distribution
    !  -------------------------------------------------------
    allocate( IMs(0:Nx_-1), JMs(0:Ny_-1), LMs(0:Nz_-1), __STAT)
    call MAPL_DecomposeDim ( IM_World_, IMs, Nx_ )
    call MAPL_DecomposeDim ( JM_World_, JMs, Ny_ )
    call MAPL_DecomposeDim ( LM_World_, LMs, Nz_ )

    !  ------------------------------------------------------------
    !  TO DO: implement IMs/JMs/LMs as part of the IDEF/JDEF record
    !         our thru command line
    !  ------------------------------------------------------------

    !  3D Lat-Lon-Lev Grid
    !  -------------------
    if ( LM_World_>0 .AND. IM_World_>0 .AND. JM_World_>0 ) then
       !ALT creat actually 2-d grid the SAME way MAPL_GridCreate
#if 0
       Grid = ESMF_GridCreateShapeTile (     &
            name=Name,                     &
            countsPerDEDim1=IMs,           &
            countsPerDEDim2=JMs,           &
            countsPerDEDim3=LMs,           &
            coordDep1 = (/1,2/),           &
            coordDep2 = (/1,2/),           &
            coordDep3 = (/3/),             &
            gridEdgeLWidth = (/0,0,0/),    &
            gridEdgeUWidth = (/0,0,0/),    &
            __RC)
#else
       Grid = ESMF_GridCreate(             &
            name=Name,                     &
            countsPerDEDim1=IMs,           &
            countsPerDEDim2=JMs,           &
            indexFlag = ESMF_INDEX_DELOCAL,&
            gridEdgeLWidth = (/0,0/),      &
            gridEdgeUWidth = (/0,0/),      &
            coordDep1 = (/1,2/),           &
            coordDep2 = (/1,2/),           &
            __RC)

       call ESMF_AttributeSet(grid, name='GRID_LM', value=LM_World, __RC)

#endif

       !  2D Lat-Lon Grid
       !  ---------------
    else if ( LM_World_==0 .AND. IM_World_>0 .AND. JM_World>0 ) then
       Grid = ESMF_GridCreate(             &
            name=Name,                     &
            countsPerDEDim1=IMs,           &
            countsPerDEDim2=JMs,           &
            coordDep1 = (/1,2/),           &
            coordDep2 = (/1,2/),           &
            gridEdgeLWidth = (/0,0/),      &
            gridEdgeUWidth = (/0,0/),      &
            __RC)

       !  Other possibilities not implemented yet
       !  ---------------------------------------
    else

       STATUS = 300

    endif

    !  -------------------------------------------------------------------
    !  NOTE: In the remaining part of this routine it is assumed that the
    !        1st and 2nd axes correspond to lat/lon; revise this for other
    !        arrangements (say, YZ grids)
    !  -------------------------------------------------------------------

    !  Allocate coords at default stagger location
    !  -------------------------------------------
    call ESMF_GridAddCoord(Grid, __RC)

    !  Compute the coordinates (the corner/center is for backward compatibility)
    !  -------------------------------------------------------------------------
    deltaX      = MAPL_DEGREES_TO_RADIANS_R8 * DelLon_
    deltaY      = MAPL_DEGREES_TO_RADIANS_R8 * DelLat_
    minCoord(1) = MAPL_DEGREES_TO_RADIANS_R8 * BegLon_ - deltaX/2
    minCoord(2) = MAPL_DEGREES_TO_RADIANS_R8 * BegLat_ - deltaY/2

    allocate(cornerX(IM_World_+1),cornerY(JM_World_+1), __STAT)

    cornerX(1) = minCoord(1)
    do i = 1,IM_World_
       cornerX(i+1) = cornerX(1) + deltaX * i
    enddo

    cornerY(1) = minCoord(2)
    do j = 1,JM_World_
       cornerY(j+1) = cornerY(1) + deltaY * j
    enddo

    !  Retrieve the coordinates so we can set them
    !  -------------------------------------------
    call ESMF_GridGetCoord (Grid, coordDim=1, localDE=0, &
         staggerloc=ESMF_STAGGERLOC_CENTER, &
         farrayPtr=centerX, __RC)

    call ESMF_GridGetCoord (Grid, coordDim=2, localDE=0, &
         staggerloc=ESMF_STAGGERLOC_CENTER, &
         farrayPtr=centerY, __RC)

    FirstOut(1)=BegLon_
    FirstOut(2)=-90.
    LastOut(1)=360.+BegLon_ - 360./im_world_
    LastOut(2)=90.

    block
      use MAPL_Constants, only: MAPL_DEGREES_TO_RADIANS_R8
      use iso_fortran_env, only: REAL64
      real(kind=REAL64), allocatable :: lons(:)
      real(kind=REAL64), allocatable :: lats(:)

      lons = MAPL_Range(FirstOut(1), LastOut(1), im_world_, &
           & conversion_factor=MAPL_DEGREES_TO_RADIANS_R8)
      lats = MAPL_Range(FirstOut(2), LastOut(2), JM_WORLD, &
           & conversion_factor=MAPL_DEGREES_TO_RADIANS_R8)

      call MAPL_GRID_INTERIOR(grid, i1, in, j1, jn)

      do i = 1,size(centerX,1)
         centerX(I,:) = lons(i1+i-1)
      end do

      do j = 1,size(centerY,2)
         centerY(:,J) = lats(j1+j-1)
      enddo

    end block


    !  Make sure we've got it right
    !  ----------------------------
    call ESMF_GridValidate(Grid,__RC)

    !  Clean up
    !  --------
    deallocate(cornerY,cornerX)
    deallocate(IMs,JMs,LMs)
    if ( present(ConfigFile) ) deallocate(Config_)
    if ( .not. present(vm) )   deallocate(vm_)

    !  All Done
    !  --------
    __RETURN(STATUS)

  Contains

    subroutine parseConfig_()
      !
      !    Internal routine to parse the ESMF_Config.
      !
      STATUS = 200     ! not implemented yet

    end subroutine parseConfig_

  end function MAPL_LatLonGridCreate

  !............................................................................

  module subroutine MAPL_GridGetCorners(grid,gridCornerLons, gridCornerLats, RC)
    type (ESMF_Grid), intent(INOUT) :: GRID
    real(ESMF_KIND_R8), intent(INOUT) :: gridCornerLons(:,:)
    real(ESMF_KIND_R8), intent(INOUT) :: gridCornerLats(:,:)
    integer, optional, intent(  OUT) :: RC
    integer :: status

    type(ESMF_RouteHandle) :: rh
    type(ESMF_Field) :: field
    integer :: counts(3),lsz
    real(ESMF_KIND_R8), pointer :: ptr(:,:)
    real(ESMF_KIND_R8), pointer :: corner(:,:)
    integer :: im,jm,imc,jmc,idx,i,j
    logical :: hasLons,hasLats
    real(ESMF_KIND_R8), allocatable :: r8ptr(:),lons1d(:),lats1d(:)
    type(ESMF_CoordSys_Flag) :: coordSys

    call MAPL_GridGet(grid,localCellCountPerDim=counts,__RC)
    im=counts(1)
    jm=counts(2)
    ! check if we have corners
    call ESMF_AttributeGet(grid,  NAME='GridCornerLons:', &
         isPresent=hasLons, __RC)
    call ESMF_AttributeGet(grid,  NAME='GridCornerLats:', &
         isPresent=hasLats, __RC)
    if (hasLons .and. hasLats) then
       call ESMF_AttributeGet(grid,  NAME='GridCornerLons:', &
            itemcount=lsz, __RC)
       __ASSERT(size(gridCornerLons,1)*size(gridCornerLons,2)==lsz,"stored corner sizes to not match grid")
       call ESMF_AttributeGet(grid,  NAME='GridCornerLats:', &
            itemcount=lsz, __RC)
       __ASSERT(size(gridCornerLats,1)*size(gridCornerLats,2)==lsz,"stored corner sizes to not match grid")
       allocate(r8ptr(lsz),__STAT)

       call ESMF_AttributeGet(grid,  NAME='GridCornerLons:', &
            VALUELIST=r8ptr, __RC)

       idx = 0
       do j = 1, size(gridCornerLons,2)
          do i = 1, size(gridCornerLons,1)
             idx = idx+1
             gridCornerLons(i,j) = r8ptr(idx)
          end do
       end do

       call ESMF_AttributeGet(grid,  NAME='GridCornerLats:', &
            VALUELIST=r8ptr, __RC)

       idx = 0
       do j = 1, size(gridCornerLons,2)
          do i = 1, size(gridCornerLons,1)
             idx = idx+1
             gridCornerLats(i,j) = r8ptr(idx)
          end do
       end do
       deallocate(r8ptr)
    else

       call ESMF_GridGetCoord(grid,localDE=0,coordDim=1,staggerloc=ESMF_STAGGERLOC_CORNER, &
            farrayPtr=corner, __RC)
       imc=size(corner,1)
       jmc=size(corner,2)
       allocate(ptr(0:imc+1,0:jmc+1),source=0.0d0,__STAT)
       field = ESMF_FieldCreate(grid,ptr,staggerLoc=ESMF_STAGGERLOC_CORNER,totalLWidth=[1,1],totalUWidth=[1,1],__RC)
       call ESMF_FieldHaloStore(field,rh,__RC)

       ptr(1:imc,1:jmc)=corner
       call ESMF_FieldHalo(field,rh,__RC)
       gridCornerLons=ptr(1:im+1,1:jm+1)

       call ESMF_GridGetCoord(grid,localDE=0,coordDim=2,staggerloc=ESMF_STAGGERLOC_CORNER, &
            farrayPtr=corner, __RC)
       ptr(1:imc,1:jmc)=corner
       call ESMF_FieldHalo(field,rh,__RC)
       gridCornerLats=ptr(1:im+1,1:jm+1)

       deallocate(ptr)
       call ESMF_FieldDestroy(field,__RC)
       call ESMF_FieldHaloRelease(rh,__RC)

       call ESMF_GridGet(grid,coordSys=coordSys,__RC)
       if (coordSys==ESMF_COORDSYS_SPH_DEG) then
          gridCornerLons=gridCornerLons*MAPL_DEGREES_TO_RADIANS_R8
          gridCornerLats=gridCornerLats*MAPL_DEGREES_TO_RADIANS_R8
       else if (coordSys==ESMF_COORDSYS_CART) then
          __FAIL('Unsupported coordinate system:  ESMF_COORDSYS_CART')
       end if
       allocate(lons1d(size(gridCornerLons,1)*size(gridCornerLons,2)),__STAT)
       allocate(lats1d(size(gridCornerLons,1)*size(gridCornerLons,2)),__STAT)
       idx = 0
       do j=1,size(gridCornerLons,2)
          do i=1,size(gridCornerLons,1)
             idx=idx+1
             lons1d(idx)=gridCornerLons(i,j)
             lats1d(idx)=gridCornerLats(i,j)
          enddo
       enddo
       call ESMF_AttributeSet(grid, name='GridCornerLons:', &
            itemCount = idx, valueList=lons1d, __RC)
       call ESMF_AttributeSet(grid, name='GridCornerLats:', &
            itemCount = idx, valueList=lats1d, __RC)
       deallocate(lons1d,lats1d)
    end if

    __RETURN(ESMF_SUCCESS)

  end subroutine MAPL_GridGetCorners

  !............................................................................


  !
  ! Note: The routine below came from ESMFL; it has been moved here to
  !       avoid circular dependencies (Arlindo).
  !
  module subroutine MAPL_GridGetInterior(GRID,I1,IN,J1,JN)
    type (ESMF_Grid), intent(IN) :: grid
    integer, intent(OUT)         :: I1, IN, J1, JN

    ! local vars
    integer                               :: status
    !    character(len=ESMF_MAXSTR)            :: IAm='MAPL_GridGetInterior'

    type (ESMF_DistGrid)                  :: distGrid
    type(ESMF_DELayout)                   :: LAYOUT
    type (ESMF_VM)                        :: vm
    integer,               allocatable    :: AL(:,:)
    integer,               allocatable    :: AU(:,:)
    integer                               :: nDEs
    integer                               :: deId
    integer                               :: gridRank
    integer                               :: rc
    logical                               :: isPresent
    integer, allocatable                  :: global_grid_info(:)
    integer                               :: itemCount

    i1=-1
    j1=-1
    in=-1
    jn=-1

    call ESMF_AttributeGet(grid, name="GLOBAL_GRID_INFO", isPresent=isPresent, __RC)
    if (isPresent) then
      call ESMF_AttributeGet(grid, name="GLOBAL_GRID_INFO", itemCount=itemCount, __RC)
      allocate(global_grid_info(itemCount), __STAT)
      call ESMF_AttributeGet(grid, name="GLOBAL_GRID_INFO", valueList=global_grid_info, __RC)
      I1 = global_grid_info(7)
      IN = global_grid_info(8)
      j1 = global_grid_info(9)
      JN = global_grid_info(10)
      deallocate(global_grid_info, __STAT)
      __RETURN(__SUCCESS)
    end if


    call ESMF_GridGet    (GRID, dimCount=gridRank, distGrid=distGrid, __RC)
    call ESMF_DistGridGet(distGRID, delayout=layout, __RC)
    call ESMF_DELayoutGet(layout, vm=vm, __RC)
    call ESMF_VmGet(vm, localPet=deId, petCount=nDEs, __RC)

    allocate (AL(gridRank,0:nDEs-1),  __STAT)
    allocate (AU(gridRank,0:nDEs-1),  __STAT)

    call MAPL_DistGridGet(distgrid, &
         minIndex=AL, maxIndex=AU, __RC)

    I1 = AL(1, deId)
    IN = AU(1, deId)
    !    __ASSERT(gridRank > 1, 'tilegrid is 1d (without RC this only for info')
    J1 = AL(2, deId)
    JN = AU(2, deId)
    deallocate(AU, AL)

  end subroutine MAPL_GridGetInterior

  !.......................................................................

  module function MAPL_RmQualifier(str, del) result(new)

    character(len=*),           intent(in)  :: str
    character(len=*), optional, intent(in)  :: del ! optional delimiter

    character(len=len(str)) :: new

    !
    !     Simple function to remove qualifier from a string. For example,
    !     MAPL_RmQualifier('GOCART::du001') yields "du001". By default,
    !     '::' is used as the qualifier delimiter.
    !
    character(len=len(str)) :: del_
    integer :: i
    if ( present(del) ) then
       del_ = del
    else
       del_ = '::'
    end if
    new = adjustl(str)
    i = index(str,trim(del_))
    if ( i > 0 ) new = new(i+2:)
  end function MAPL_RmQualifier



  module function MAPL_StrUpCase(str) result(new)
    character(len=*), intent(IN) :: str
    character(len=len(str))      :: new

    integer, parameter :: a = iachar('a')
    integer, parameter :: z = iachar('z')
    integer, parameter :: dd = iachar('z') - iachar('Z')

    integer i,c

    new = str
    do i=1,len(new)
       c = iachar(new(i:i))
       if( c >= a .and. c <= z ) new(i:i) = achar(c-dd)
    enddo

    return
  end function MAPL_StrUpCase

  module function MAPL_StrDnCase(str) result(new)
    character(len=*), intent(IN) :: str
    character(len=len(str))      :: new

    integer, parameter :: A = iachar('A')
    integer, parameter :: Z = iachar('Z')
    integer, parameter :: dd = iachar('z') - iachar('Z')

    integer i,c

    new = str
    do i=1,len(new)
       c = iachar(new(i:i))
       if( c >= A .and. c <= Z ) new(i:i) = achar(c+dd)
    enddo

    return
  end function MAPL_StrDnCase


  ! ========================================
  recursive module subroutine MAPL_StateAttSetI4(STATE, NAME, VALUE, RC)
    type(ESMF_State),                 intent(INOUT) :: STATE
    character(len=*),                 intent(IN   ) :: NAME
    integer,                          intent(IN   ) :: VALUE
    integer, optional,                intent(  OUT) :: RC

    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_StateAttSet"
    integer                               :: STATUS

    type(ESMF_State)                      :: nestedSTATE
    type(ESMF_Field)                      :: FIELD
    type(ESMF_FieldBundle)                :: BUNDLE
    type (ESMF_StateItem_Flag), pointer   :: ITEMTYPES(:)
    character(len=ESMF_MAXSTR ), pointer  :: ITEMNAMES(:)
    integer                               :: ITEMCOUNT
    integer                               :: I

    call ESMF_AttributeSet(STATE, NAME, VALUE, __RC)

    call ESMF_StateGet(STATE,ITEMCOUNT=ITEMCOUNT,__RC)

    IF (ITEMCOUNT>0) then
       allocate(ITEMNAMES(ITEMCOUNT),__STAT)
       allocate(ITEMTYPES(ITEMCOUNT),__STAT)
       call ESMF_StateGet(STATE, ITEMNAMELIST=ITEMNAMES, &
            ITEMTYPELIST=ITEMTYPES, __RC)

       do I = 1, ITEMCOUNT
          if(itemtypes(I)==ESMF_StateItem_State) then
             call ESMF_StateGet(STATE, itemNames(I), nestedState, __RC)
             call MAPL_AttributeSet(nestedState, NAME, VALUE, __RC)
          else if(itemtypes(I)==ESMF_StateItem_FieldBundle) then
             call ESMF_StateGet(STATE, itemNames(I), BUNDLE, __RC)
             call MAPL_AttributeSet(BUNDLE, NAME, VALUE, __RC)
          else if(itemtypes(I)==ESMF_StateItem_Field) then
             call ESMF_StateGet(STATE, itemNames(I), FIELD, __RC)
             call MAPL_AttributeSet(FIELD, NAME, VALUE, __RC)
          end if
       end do

       deallocate(ITEMNAMES)
       deallocate(ITEMTYPES)
    end if

    __RETURN(ESMF_SUCCESS)
  end subroutine MAPL_StateAttSetI4

  ! ========================================
  module subroutine MAPL_BundleAttSetI4(BUNDLE, NAME, VALUE, RC)
    type(ESMF_FieldBundle),           intent(INOUT) :: BUNDLE
    character(len=*),                 intent(IN   ) :: NAME
    integer,                          intent(IN   ) :: VALUE
    integer, optional,                intent(  OUT) :: RC

    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_BundleAttSet"
    integer                               :: STATUS

    type(ESMF_Field)                      :: FIELD
    integer                               :: FIELDCOUNT
    integer                               :: I

    call ESMF_AttributeSet(BUNDLE, NAME, VALUE, __RC)

    call ESMF_FieldBundleGet(BUNDLE, FieldCount=FIELDCOUNT, __RC)

    do I = 1, FIELDCOUNT
       call ESMF_FieldBundleGet(BUNDLE, I, FIELD, __RC)
       call ESMF_AttributeSet(FIELD, NAME, VALUE, __RC)
    end do

    __RETURN(ESMF_SUCCESS)
  end subroutine MAPL_BundleAttSetI4

  ! ========================================
  module subroutine MAPL_FieldAttSetI4(FIELD, NAME, VALUE, RC)
    type(ESMF_Field),                 intent(INOUT) :: FIELD
    character(len=*),                 intent(IN   ) :: NAME
    integer,                          intent(IN   ) :: VALUE
    integer, optional,                intent(  OUT) :: RC

    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_FieldAttSet"
    integer                               :: STATUS

    type(ESMF_Array)                        :: array
    type(ESMF_FieldStatus_Flag)             :: fieldStatus


    call ESMF_AttributeSet(FIELD, NAME, VALUE, __RC)

    call ESMF_FieldGet(field, status=fieldStatus, __RC)

    if(fieldStatus == ESMF_FIELDSTATUS_COMPLETE) then
       call ESMF_FieldGet(field, Array=array, __RC)
       call ESMF_AttributeSet(array, NAME, VALUE, __RC)
    end if

    __RETURN(ESMF_SUCCESS)
  end subroutine MAPL_FieldAttSetI4
  ! ========================================

  module subroutine MAPL_FieldBundleDestroy(Bundle,RC)
    type(ESMF_FieldBundle),    intent(INOUT) :: Bundle
    integer, optional,         intent(OUT  ) :: RC

    integer                               :: I
    integer                               :: FieldCount
    type(ESMF_Field)                      :: Field
    logical                               :: isCreated

    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_FieldBundleDestroy"
    integer                               :: STATUS


    isCreated = ESMF_FieldBundleIsCreated(bundle,__RC)
    if(isCreated) then
       call ESMF_FieldBundleGet(BUNDLE, FieldCount=FIELDCOUNT, __RC)

       do I = 1, FIELDCOUNT
          call ESMF_FieldBundleGet(BUNDLE, I, FIELD, __RC)
          call MAPL_FieldDestroy(FIELD, __RC)
       end do
    end if

    __RETURN(ESMF_SUCCESS)

  end subroutine MAPL_FieldBundleDestroy

  module subroutine MAPL_StateAddField(State, Field, RC)
    type(ESMF_State),  intent(inout) :: State
    type(ESMF_Field),  intent(in   ) :: Field
    integer, optional, intent(  out) :: rc

    ! ErrLog vars
    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_StateAddField"
    integer                               :: STATUS

    ! Local var
    character(len=ESMF_MAXSTR), parameter   :: attrName = MAPL_StateItemOrderList
    character(len=ESMF_MAXSTR)              :: name
    character(len=ESMF_MAXSTR), allocatable :: currList(:)
    character(len=ESMF_MAXSTR), allocatable :: thisList(:)
    integer                                 :: natt
    integer                                 :: na
    type(ESMF_Field)                        :: Fields(1)
    logical                                 :: haveAttr

    fields(1) = field
    call ESMF_StateAdd(state, fields, __RC)
    !=================
!!!ALT Example to add one field at the time (not used anymore)
!!!      call ESMF_StateAdd(STATE, FIELD, proxyflag=.false., &
!!!           addflag=.true., replaceflag=.false., __RC )
    !=================

    ! check for attribute

    call ESMF_AttributeGet(state, NAME=attrName, isPresent=haveAttr, __RC)
    if (haveAttr) then
       call ESMF_AttributeGet(state, NAME=attrName, itemcount=natt, __RC)
    else
       natt = 0
    end if
    allocate(currList(natt), __STAT)

    if (natt > 0) then
       ! get the current list
       call ESMF_AttributeGet(state, NAME=attrName, VALUELIST=currList, __RC)
       !ALT delete/destroy this attribute to prevent memory leaks
       call ESMF_AttributeRemove(state, NAME=attrName, __RC)
    end if

    na = natt+1
    allocate(thisList(na), __STAT)

    thisList(1:natt) = currList

    call ESMF_FieldGet(field, name=name, __RC)

    thisList(na) = name

    call ESMF_AttributeSet(state, NAME=attrName, itemcount=na, VALUELIST=thisList, __RC)

    deallocate(thisList)
    deallocate(currList)

    __RETURN(ESMF_SUCCESS)
  end subroutine MAPL_StateAddField

  module subroutine MAPL_StateAddBundle(State, Bundle, RC)
    type(ESMF_State),  intent(inout) :: State
    type(ESMF_FieldBundle),  intent(in   ) :: Bundle
    integer, optional, intent(  out) :: rc


    ! ErrLog vars
    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_StateAddBundles"
    integer                               :: STATUS

    ! Local var
    character(len=ESMF_MAXSTR), parameter   :: attrName = MAPL_StateItemOrderList
    character(len=ESMF_MAXSTR)              :: name
    character(len=ESMF_MAXSTR), allocatable :: currList(:)
    character(len=ESMF_MAXSTR), allocatable :: thisList(:)
    integer                                 :: natt
    integer                                 :: na
    type(ESMF_FieldBundle)                  :: Bundles(1)
    logical                                 :: haveAttr

    bundles(1) = bundle
    call ESMF_StateAdd(state, Bundles, __RC)

    ! check for attribute

    call ESMF_AttributeGet(state, NAME=attrName, isPresent=haveAttr, __RC)
    if (haveAttr) then
       call ESMF_AttributeGet(state, NAME=attrName, itemcount=natt, __RC)
    else
       natt = 0
    end if
    allocate(currList(natt), __STAT)

    if (natt > 0) then
       ! get the current list
       call ESMF_AttributeGet(state, NAME=attrName, VALUELIST=currList, __RC)
       !ALT delete/destroy this attribute to prevent memory leaks
       call ESMF_AttributeRemove(state, NAME=attrName, __RC)
    end if

    na = natt+1
    allocate(thisList(na), __STAT)

    thisList(1:natt) = currList

    call ESMF_FieldBundleGet(bundle, name=name, __RC)

    thisList(na) = name

    call ESMF_AttributeSet(state, NAME=attrName, itemcount=na, VALUELIST=thisList, __RC)

    deallocate(thisList)
    deallocate(currList)

    __RETURN(ESMF_SUCCESS)
  end subroutine MAPL_StateAddBundle

  module subroutine MAPL_FieldBundleAddField(Bundle, Field, multiflag, RC)
    type(ESMF_FieldBundle),  intent(inout) :: Bundle
    type(ESMF_Field),  intent(in   ) :: Field
    logical, optional, intent(in   ) :: multiflag
    integer, optional, intent(  out) :: rc

    ! ErrLog vars
    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_FieldBundleAddField"
    integer                               :: STATUS

    ! Local var
    character(len=ESMF_MAXSTR), parameter   :: attrName = MAPL_BundleItemOrderList
    character(len=ESMF_MAXSTR)              :: name
    character(len=ESMF_MAXSTR), allocatable :: currList(:)
    character(len=ESMF_MAXSTR), allocatable :: thisList(:)
    integer                                 :: natt
    integer                                 :: na
    type(ESMF_Field)                        :: Fields(1)
    logical                                 :: haveAttr


    fields(1) = field
    call ESMF_FieldBundleAdd(Bundle, fields, multiflag=multiflag, __RC)

    ! check for attribute

    call ESMF_AttributeGet(Bundle, NAME=attrName, isPresent=haveAttr, __RC)
    if (haveAttr) then
       call ESMF_AttributeGet(Bundle, NAME=attrName, itemcount=natt, __RC)
    else
       natt = 0
    end if
    allocate(currList(natt), __STAT)

    if (natt > 0) then
       ! get the current list
       call ESMF_AttributeGet(Bundle, NAME=attrName, VALUELIST=currList, __RC)
       !ALT delete/destroy this attribute to prevent memory leaks
       call ESMF_AttributeRemove(bundle, NAME=attrName, __RC)
    end if

    na = natt+1
    allocate(thisList(na), __STAT)

    thisList(1:natt) = currList

    call ESMF_FieldGet(field, name=name, __RC)

    thisList(na) = name

    call ESMF_AttributeSet(bundle, NAME=attrName, itemcount=na, VALUELIST=thisList, __RC)

    deallocate(thisList)
    deallocate(currList)

    __RETURN(ESMF_SUCCESS)
  end subroutine MAPL_FieldBundleAddField

  module subroutine MAPL_FieldBundleGetByIndex(Bundle, fieldIndex, Field, RC)
    type(ESMF_FieldBundle),  intent(INout) :: Bundle
    integer,           intent(in   ) :: fieldIndex
    type(ESMF_Field),  intent(INout   ) :: Field
    integer, optional, intent(  out) :: rc

    ! ErrLog vars
    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_FieldBundleGetByIndex"
    integer                               :: STATUS

    ! Local var
    character(len=ESMF_MAXSTR), parameter   :: attrName = MAPL_BundleItemOrderList
    character(len=ESMF_MAXSTR)              :: name
    character(len=ESMF_MAXSTR), allocatable :: currList(:)
    integer                                 :: natt


    ! check for attribute

    call ESMF_AttributeGet(Bundle, NAME=attrName, itemcount=natt, __RC)
    allocate(currList(natt), __STAT)

    ! get the current list
    call ESMF_AttributeGet(Bundle, NAME=attrName, VALUELIST=currList, __RC)

    name = currList(fieldIndex)
    call ESMF_FieldBundleGet(Bundle, fieldName = name, field=field, __RC)

    deallocate(currList)

    __RETURN(ESMF_SUCCESS)
  end subroutine MAPL_FieldBundleGetByIndex

!-----------------------------------------------------------------------
!>
! `MAPL_GetHorzIJIndex` -- Get indexes on destributed ESMF grid for an arbitary lat and lon
!
! For a set of longitudes and latitudes in radians this routine will return the indexes for the domain
! Depending on how it is invoked these will be the local domain or the global indices.
! If the Lat/Lon pair is not in the domain -1 is returned.
! The routine works for both the gmao cube and lat/lon grids.
! Currently the lat/lon grid is asumed to go from -180 to 180
!
  module subroutine MAPL_GetHorzIJIndex(npts,II,JJ,lon,lat,lonR8,latR8,Grid, rc)
    implicit none
    !ARGUMENTS:
    integer,                      intent(in   ) :: npts        !! number of points in lat and lon arrays
    integer,                      intent(inout) :: II(npts)    !! array of the first index for each lat and lon
    integer,                      intent(inout) :: JJ(npts)    !! array of the second index for each lat and lon
    real, optional,               intent(in   ) :: lon(npts)   !! array of longitudes in radians
    real, optional,               intent(in   ) :: lat(npts)   !! array of latitudes in radians
    real(ESMF_KIND_R8), optional, intent(in   ) :: lonR8(npts) !! array of longitudes in radians
    real(ESMF_KIND_R8), optional, intent(in   ) :: latR8(npts) !! array of latitudes in radians
    type(ESMF_Grid),    optional, intent(inout) :: Grid        !! ESMF grid
    integer,            optional, intent(out  ) :: rc          !! return code

    integer                    :: status

    integer                         :: IM_World, JM_World, dims(3)
    integer                         :: IM, JM, counts(3)
    real(ESMF_KIND_R8), pointer     :: lons(:,:)
    real(ESMF_KIND_R8), pointer     :: lats(:,:)
    real(ESMF_KIND_R8), allocatable :: elons(:)
    real(ESMF_KIND_R8), allocatable :: elats(:)
    integer :: i,iiloc,jjloc, i1, i2, j1, j2
    real(ESMF_KIND_R4) :: lonloc,latloc
    logical            :: localSearch
    real(ESMF_KIND_R8), allocatable :: tmp_lons(:),tmp_lats(:)
    type(ESMF_CoordSys_Flag) :: coordSys
    character(len=ESMF_MAXSTR) :: grid_type

    ! if the grid is present then we can just get the prestored edges and the dimensions of the grid
    ! this also means we are running on a distributed grid
    ! if grid not present then the we just be running outside of ESMF and the user must
    ! pass in the the dimensions of the grid and we must compute them
    ! and assume search on the global domain
    if (present(Grid)) then
       call MAPL_GridGet(grid, localCellCountPerDim=counts,globalCellCountPerDim=dims,__RC)
       IM_World = dims(1)
       JM_World = dims(2)
       IM = counts(1)
       JM = counts(2)
       localSearch = .true.
    else
       localSearch = .false.
    end if

    allocate(tmp_lons(npts),tmp_lats(npts))
    if (present(lon) .and. present(lat)) then
       tmp_lons = lon
       tmp_lats = lat
    else if (present(lonR8) .and. present(latR8)) then
       tmp_lons = lonR8
       tmp_lats = latR8
    end if

    if (im_world*6==jm_world) then

      call MAPL_GetGlobalHorzIJIndex(npts, II, JJ, lon=lon, lat=lat, lonR8=lonR8, latR8=latR8, Grid=Grid, __RC)

      call MAPL_Grid_Interior(Grid,i1,i2,j1,j2)
      ! convert index to local, if it is not in domain, set it to -1 just as the legacy code
      where ( i1 <= II .and. II <=i2 .and. j1<=JJ .and. JJ<=j2)
        II = II - i1 + 1
        JJ = JJ - j1 + 1
      elsewhere
        II = -1
        JJ = -1
      end where

    else
       __ASSERT(localSearch,"Global Search for IJ for latlon not implemented")
       call ESMF_GridGetCoord(grid,coordDim=1, localDe=0, &
               staggerloc=ESMF_STAGGERLOC_CORNER, fArrayPtr = lons, __RC)
       call ESMF_GridGetCoord(grid,coordDim=2, localDe=0, &
               staggerloc=ESMF_STAGGERLOC_CORNER, fArrayPtr = lats, __RC)
       allocate(elons(im+1),__STAT)
       allocate(elats(jm+1),__STAT)
       call ESMF_GridGet(grid,coordSys=coordSys,__RC)
       elons = lons(:,1)
       elats = lats(1,:)
       if (coordSys==ESMF_COORDSYS_SPH_DEG) then
          elons=elons*MAPL_DEGREES_TO_RADIANS_R8
          elats=elats*MAPL_DEGREES_TO_RADIANS_R8
       else if (coordSys==ESMF_COORDSYS_CART) then
          __FAIL('Unsupported coordinate system:  ESMF_COORDSYS_CART')
       end if
       ! lat-lon grid goes from -180 to 180 shift if we must
       ! BMA this -180 to 180 might change at some point
       do i=1,npts
          lonloc = tmp_lons(i)
          latloc = tmp_lats(i)
          if (lonloc > MAPL_PI) lonloc = lonloc - 2.0*MAPL_PI
          IIloc = ijsearch(elons,im+1,lonloc,.false.)
          JJloc = ijsearch(elats,jm+1,latloc,.false.)
          II(i) = IIloc
          JJ(i) = JJloc
       end do
       deallocate(elons,elats)
    end if

    deallocate(tmp_lons, tmp_lats)
    __RETURN(ESMF_SUCCESS)

  contains

    integer function ijsearch(coords,idim,valueIn,periodic) ! fast bisection version
      implicit NONE
      integer, intent(in) :: idim
      real(ESMF_KIND_R8), intent(in) :: coords(:)
      real, intent(inout) :: valueIn
      logical, intent(in)  :: periodic
      integer i, i1, i2, k
      real :: value
      value = valueIn
      if ( periodic ) then
         if ( value>coords(idim) ) value = value - 360.
      endif

      ijsearch = -1
      i1 = 1
      i2 = idim
      if (coords(idim) > coords(1)) then
         do k = 1, idim  ! it should never take take long
            i = (i1 + i2) / 2
            if ( (value .ge. coords(i)) ) then
               if (value .lt. coords(i+1) ) then
                  ijsearch = i
                  exit
               else
                  i1 = i
               end if
            else
               i2 = i
            endif
         end do
      else
         do k = 1, idim  ! it should never take take long
            i = (i1 + i2) / 2
            if ( (value .lt. coords(i)) ) then
               if (value .ge. coords(i+1) ) then
                  ijsearch = i
                  exit
               else
                  i1 = i
               end if
            else
               i2 = i
            endif
         end do
      endif
    end function ijsearch

  end subroutine MAPL_GetHorzIJIndex

  module subroutine MAPL_GetGlobalHorzIJIndex(npts,II,JJ,lon,lat,lonR8,latR8,Grid, rc)
    implicit none
    !ARGUMENTS:
    integer,                      intent(in   ) :: npts ! number of points in lat and lon arrays
    integer,                      intent(inout) :: II(npts) ! array of the first index for each lat and lon
    integer,                      intent(inout) :: JJ(npts) ! array of the second index for each lat and lon
    real, optional,               intent(in   ) :: lon(npts) ! array of longitudes in radians
    real, optional,               intent(in   ) :: lat(npts) ! array of latitudes in radians
    real(ESMF_KIND_R8), optional, intent(in   ) :: lonR8(npts) ! array of longitudes in radians
    real(ESMF_KIND_R8), optional, intent(in   ) :: latR8(npts) ! array of latitudes in radians
    type(ESMF_Grid),    optional, intent(inout) :: Grid ! ESMF grid
    integer,            optional, intent(out  ) :: rc  ! return code

    integer :: status
    integer :: dims(3), IM_WORLD, JM_WORLD
    real(ESMF_KIND_R8), allocatable, dimension (:,:) :: xyz
    real(ESMF_KIND_R8), allocatable, dimension (:)   :: x,y,z
    real(ESMF_KIND_R8), allocatable :: max_abs(:)
    real(ESMF_KIND_R8)              :: dalpha, shift0
    real(ESMF_KIND_R8), allocatable :: lons(:), lats(:)

    ! sqrt(2.0d0), distance from center to the mid of an edge for a 2x2x2 cube
    real(ESMF_KIND_R8), parameter :: sqr2 = 1.41421356237310d0

    ! asin(1.d0/sqrt(3.d0)),  angle between the two lines(center to the mid and center to the end of an edge)
    real(ESMF_KIND_R8), parameter :: alpha= 0.615479708670387d0

    ! MAPL_PI_R8/18, Japan Fuji mountain shift
    real(ESMF_KIND_R8), parameter :: shift= 0.174532925199433d0

    logical :: good_grid, stretched

    if ( .not. present(grid)) then
      __FAIL("need a cubed-sphere grid")
    endif
    call MAPL_GridGet(grid, globalCellCountPerDim=dims,__RC)
    IM_World = dims(1)
    JM_World = dims(2)
    __ASSERT( IM_WORLD*6 == JM_WORLD, "It only works for cubed-sphere grid")

    allocate(lons(npts),lats(npts))

    call MAPL_Reverse_Schmidt(Grid, stretched, npts, lon=lon, lat=lat, lonR8=lonR8, latR8=latR8, lonRe=lons, latRe=lats, __RC)

    dalpha = 2.0d0*alpha/IM_WORLD

    ! make sure the grid can be used in this subroutine
    good_grid = grid_is_ok(grid)
    if ( .not. good_grid ) then
       __FAIL( "MAPL_GetGlobalHorzIJIndex cannot handle this grid")
    endif
    ! Return if no local points
    __RETURN_IF(npts==0)

    ! shift the grid away from Japan Fuji Mt.
    shift0 = shift
    if (stretched) shift0 = 0
    lons = lons + shift0

    ! get xyz from sphere surface
    allocate(xyz(3, npts), max_abs(npts))
    xyz(1,:) = cos(lats)*cos(lons)
    xyz(2,:) = cos(lats)*sin(lons)
    xyz(3,:) = sin(lats)

    ! project onto 2x2x2 cube
    max_abs = maxval(abs(xyz), dim=1)

    xyz(1,:) = xyz(1,:)/max_abs
    xyz(2,:) = xyz(2,:)/max_abs
    xyz(3,:) = xyz(3,:)/max_abs

    x = xyz(1,:)
    y = xyz(2,:)
    z = xyz(3,:)

    II = -1
    JJ = -1

    ! The edge points are assigned in the order of face 1,2,3,4,5,6
    call calculate(x,y,z,II,JJ)

    __RETURN(__SUCCESS)

contains

    elemental subroutine calculate(x, y, z, i, j)
       real(ESMF_KIND_R8), intent(in) :: x
       real(ESMF_KIND_R8), intent(in) :: y
       real(ESMF_KIND_R8), intent(in) :: z
       integer, intent(out) :: i, j
       real :: tolerance

       tolerance = epsilon(1.0d0)

       ! face = 1
       if   ( abs(x-1.0d0) <= tolerance) then
         call angle_to_index(y, z, i, j)
       ! face = 2
       elseif (abs(y-1.0d0) <= tolerance) then
         call angle_to_index(-x,  z, i, j)
         J = J + IM_WORLD
       ! face = 3
       elseif (abs(z-1.0d0) <= tolerance) then
         call angle_to_index(-x, -y, i, j)
         J = J + IM_WORLD*2
       ! face = 4
       elseif (abs(x+1.0d0) <= tolerance) then
         call angle_to_index(-z, -y, i, j)
         J = J + IM_WORLD*3
       ! face = 5
       elseif (abs(y+1.0d0) <= tolerance) then
         call angle_to_index(-z,  x, i, j)
         J = J + IM_WORLD*4
       ! face = 6
       elseif (abs(z+1.0d0) <= tolerance) then
         call angle_to_index( y,  x, i, j)
         J = J + IM_WORLD*5
       endif

       if (I == 0 ) I = 1
       if (I == IM_WORLD+1 ) I = IM_WORLD
    end subroutine calculate

    elemental subroutine angle_to_index(xval, yval, i, j)
       real(ESMF_KIND_R8), intent(in) :: xval
       real(ESMF_KIND_R8), intent(in) :: yval
       integer, intent(out) :: i, j
       I = ceiling((atan(xval/sqr2) + alpha)/dalpha)
       J = ceiling((atan(yval/sqr2) + alpha)/dalpha)
       if (j == 0 ) J = 1
       if (j == IM_WORLD+1) j = IM_WORLD
    end subroutine angle_to_index

    function grid_is_ok(grid) result(OK)
       type(ESMF_Grid), intent(inout) :: grid
       logical :: OK
       integer :: I1, I2, J1, J2, j
       real(ESMF_KIND_R8), allocatable :: corner_lons(:,:), corner_lats(:,:)
       real(ESMF_KIND_R8), allocatable :: lonRe(:), latRe(:)
       real(ESMF_KIND_R8), allocatable :: accurate_lat(:), accurate_lon(:)
       real(ESMF_KIND_R8) :: stretch_factor, target_lon, target_lat, shift0
       real :: tolerance
       integer :: local_dims(3)

       tolerance = epsilon(1.0)
       call MAPL_GridGetInterior(grid,I1,I2,J1,J2)
       call MAPL_GridGet(grid, localCellCountPerDim=local_dims, __RC)
       OK = .true.
       ! check the edge of face 1 along longitude
       !call ESMF_GridGetCoord(grid,localDE=0,coordDim=1,staggerloc=ESMF_STAGGERLOC_CORNER, &
       !     farrayPtr=corner_lons, __RC)
       !call ESMF_GridGetCoord(grid,localDE=0,coordDim=2,staggerloc=ESMF_STAGGERLOC_CORNER, &
       !     farrayPtr=corner_lats, __RC)
       allocate(corner_lons(local_dims(1)+1, local_dims(2)+1))
       allocate(corner_lats(local_dims(1)+1, local_dims(2)+1))
       call MAPL_GridGetCorners(grid, corner_lons, corner_lats, __RC)


       if ( I1 == 1 .and. J1 == 1 ) then
          allocate(lonRe(local_dims(2)),      latRe(local_dims(2)))
          call MAPL_Reverse_Schmidt(grid, stretched, local_dims(2), lonR8=corner_lons(1,1:local_dims(2)), &
                                   latR8=corner_lats(1,1:local_dims(2)), lonRe=lonRe, latRe=latRe, __RC)

          allocate(accurate_lon(local_dims(2)), accurate_lat(local_dims(2)))

          shift0 = shift
          if (stretched) shift0 = 0

          accurate_lon = 1.750d0*MAPL_PI_R8 - shift0
          accurate_lat = [(-alpha + (j-1)*dalpha, j = j1, j2)]

          if (any(abs(accurate_lon - lonRe) > 2.0* tolerance) .or. any(abs(accurate_lat - latRe) > 2.0*tolerance)) then
             print*, "Error: It could be "
             print*, "  1) grid may not have pi/18 Japan mountain shift"
             print*, "  2) grid is NOT gnomonic_ed;"
             print*, "  3) lats lons from MAPL_GridGetCorners are NOT accurate (single precision from ESMF)"
             print*, "  4) strtech grid rotates north pole"
             OK = .false.
             return
          endif
        endif
    end function
  end subroutine MAPL_GetGlobalHorzIJIndex

  module subroutine MAPL_GenGridName(im, jm, lon, lat, xyoffset, gridname, geos_style)
    integer :: im, jm
    character (len=*) :: gridname
    character(len=2)  :: dateline, pole
    real, optional    :: lon(:), lat(:)
    integer, optional :: xyoffset
    logical,  optional :: geos_style

    integer           :: I
    real, parameter   :: eps=1.0e-4
    character(len=16) :: imstr, jmstr
    real              :: dlat

    logical :: old_style
    if (present(geos_style)) then
       old_style = geos_style
    else
       old_style = .false.
    end if

    if (jm /= 6*im) then
       ! Lat-Lon
       dateline='UU' ! Undefined
       pole='UU'     ! Undefined
       if (present(LON) .and. present(LAT)) then

          dlat = LAT(4) - LAT(3)
          if ((abs(LAT(1) + 90.0) < eps).or.(abs(LAT(1) + 90.0 - 0.25*dLat) < eps)) then
             pole='PC'
          else if (abs(LAT(1) + 90.0 - 0.5*dLat) < eps) then
             pole='PE'
          end if
          do I=0,1
             if(abs(LON(1) + 180.0*I) < eps) then
                dateline='DC'
                exit
             else if (abs(LON(1) + 180.0*I - 0.5*(LON(2)-LON(1))) < eps) then
                dateline='DE'
                exit
             end if
          end do

       else if (present(xyoffset)) then
          ! xyoffset Optional Flag for Grid Staggering (0:DcPc, 1:DePc, 2:DcPe, 3:DePe)
          select case (xyoffset)
          case (0)
             dateline='DC'
             pole='PC'
          case (1)
             dateline='DE'
             pole='PC'
          case (2)
             dateline='DC'
             pole='PE'
          case (3)
             dateline='DE'
             pole='PE'
          end select
       endif

       if (old_style) then
          write(imstr,*) im
          write(jmstr,*) jm
          gridname =  pole // trim(adjustl(imstr))//'x'//&
               trim(adjustl(jmstr))//'-'//dateline
       else
          write(gridname,'(a,i4.4,a,a,i4.4)') dateline,im,'x',pole,jm
       end if
    else
       ! cubed-sphere
       dateline='CF'
       pole='6C'
       if (old_style) then
          pole='PE'
          write(imstr,*) im
          write(jmstr,*) jm
          gridname =  pole // trim(adjustl(imstr))//'x'//&
               trim(adjustl(jmstr))//'-CF'
       else
          write(gridname,'(a,i4.4,a,a)') dateline,im,'x',pole
       end if
    end if

  end subroutine MAPL_GenGridName

  module function MAPL_GenXYOffset(lon, lat) result(xy)
    real        :: lon(:), lat(:)
    integer     :: xy

    integer           :: I
    integer           :: p, d
    real, parameter   :: eps=1.0e-4

    p = 0 ! default
    d = 0 ! default

    if(abs(LAT(1) + 90.0) < eps) then
       p=0 ! 'PC'
    else if (abs(LAT(1) + 90.0 - 0.5*(LAT(2)-LAT(1))) < eps) then
       p=1 ! 'PE'
    end if
    do I=0,1
       if(abs(LON(1) + 180.0*I) < eps) then
          d=0 ! 'DC'
          exit
       else if (abs(LON(1) + 180.0*I - 0.5*(LON(2)-LON(1))) < eps) then
          d=1 ! 'DE'
          exit
       end if
    end do
    xy = 2*p + d
    return
  end function MAPL_GenXYOffset

  module subroutine MAPL_GeosNameNew(name)
    character(len=*) :: name

    integer :: im, jm
    integer :: nn
    character(len=MAPL_TileNameLength) :: gridname
    character(len=2) :: dateline, pole
    character(len=8) :: imsz
    character(len=8) :: jmsz

    ! Parse name for grid info
    !-------------------------

    Gridname = AdjustL(name)
    nn   = len_trim(Gridname)
    imsz = Gridname(3:index(Gridname,'x')-1)
    jmsz = Gridname(index(Gridname,'x')+1:nn-3)
    pole = Gridname(1:2)
    dateline = Gridname(nn-1:nn)

    read(IMSZ,*) IM
    read(JMSZ,*) JM

    if (jm /= 6*im) then
       ! Lat-Lon
       write(name,'(a,i4.4,a,a,i4.4)') dateline,im,'x',pole,jm
    else
       ! Cubed-sphere
       pole='6C'
       if (dateline=='CF') then
          write(name,'(a,i4.4,a,a)') dateline,im,'x',pole
       else
          name='UNKNOWN_ERROR'
       end if
    end if
  end subroutine MAPL_GeosNameNew

  ! From a grid and a list of fields create an allocated ESMF bundle with
  ! these fields. By Default variables will be 3D at the center location
  ! unless 2 optional arguements are passed in. Can also pass in a list
  ! of long names and units if desired
  module function MAPL_BundleCreate(name,grid,fieldNames,is2D,isEdge,long_names,units,rc) result(B)
    character(len=*),           intent(in   ) :: name
    type(ESMF_Grid),            intent(inout) :: grid
    character(len=*),           intent(in   ) :: fieldNames(:)
    logical, optional,          intent(in   ) :: is2D(:)
    logical, optional,          intent(in   ) :: isEdge(:)
    character(len=*), optional, intent(in   ) :: long_names(:)
    character(len=*), optional, intent(in   ) :: units(:)
    integer, optional, intent(out  ) :: rc
    type(ESMF_FieldBundle) :: B

    character(len=ESMF_MAXSTR), parameter :: IAm='MAPL_BundleCreate'
    integer :: status
    integer :: i
    logical, allocatable :: localIs2D(:)
    logical, allocatable :: localIsEdge(:)
    real, pointer :: PTR2(:,:) => null()
    real, pointer :: PTR3(:,:,:) => null()
    integer :: counts(5)
    integer :: dims(3)
    integer, allocatable :: gridToFieldMap(:)
    integer :: gridRank
    type(ESMF_Field) :: field

    allocate(localIs2D(size(fieldNames)),__STAT)
    if (present(is2D)) then
       __ASSERT(size(fieldNames) == size(is2D),'inconsistent size of is2D array')
       localIs2D = is2D
    else
       localIs2D = .false.
    end if
    allocate(localIsEdge(size(fieldNames)),__STAT)
    if (present(isEdge)) then
       __ASSERT(size(fieldNames) == size(isEdge), 'inconsistent size of isEdge array')
       localIsEdge = isEdge
    else
       localIsEdge = .false.
    end if
    if (present(long_names)) then
       __ASSERT(size(fieldNames) == size(long_names), 'inconsistent size of long_names array')
    end if
    if (present(units)) then
       __ASSERT(size(fieldNames) == size(units), 'inconsistent size of units array')
    end if

    B = ESMF_FieldBundleCreate ( name=name, __RC )
    call ESMF_FieldBundleSet ( B, grid=GRID, __RC )
    call MAPL_GridGet(GRID, globalCellCountPerDim=COUNTS, &
         localCellCountPerDim=DIMS, __RC)
    do i=1,size(fieldnames)
       if (localIs2D(i)) then

          allocate(PTR2(DIMS(1),DIMS(2)),__STAT)
          PTR2  = 0.0
          call ESMF_GridGet(GRID, dimCount=gridRank, __RC)
          allocate(gridToFieldMap(gridRank), __STAT)
          if(gridRank == 2) then
             gridToFieldMap(1) = 1
             gridToFieldMap(2) = 2
          else if (gridRank == 3) then
             gridToFieldMap(1) = 1
             gridToFieldMap(2) = 2
             gridToFieldMap(3) = 0
          else
             __RETURN(ESMF_FAILURE)
          end if
          FIELD = ESMF_FieldCreate(grid=GRID, &
               datacopyFlag = ESMF_DATACOPY_REFERENCE,   &
               farrayPtr=PTR2, gridToFieldMap=gridToFieldMap, &
               name=fieldNames(i), __RC)
          deallocate(gridToFieldMap)
          call ESMF_AttributeSet(FIELD, NAME='DIMS', VALUE=MAPL_DimsHorzOnly, __RC)
          call ESMF_AttributeSet(FIELD, NAME='VLOCATION', VALUE=MAPL_VLocationNone, __RC)

       else
          if (localIsEdge(i)) then
             allocate(PTR3(Dims(1),Dims(2),0:counts(3)),__STAT)
          else
             allocate(PTR3(Dims(1),Dims(2),counts(3)),__STAT)
          end if
          PTR3 = 0.0
          FIELD = ESMF_FieldCreate(grid=GRID, &
               datacopyFlag = ESMF_DATACOPY_REFERENCE,   &
               farrayPtr=PTR3, name=fieldNames(i), __RC)
          call ESMF_AttributeSet(FIELD, NAME='DIMS', VALUE=MAPL_DimsHorzVert, __RC)
          if (localIsEdge(i)) then
             call ESMF_AttributeSet(FIELD, NAME='VLOCATION', VALUE=MAPL_VLocationEdge, __RC)
          else
             call ESMF_AttributeSet(FIELD, NAME='VLOCATION', VALUE=MAPL_VLocationCenter, __RC)
          end if

       end if
       if (present(long_names)) then
          call ESMF_AttributeSet(FIELD, NAME='LONG_NAME', VALUE=long_names(i), __RC)
       else
          call ESMF_AttributeSet(FIELD, NAME='LONG_NAME', VALUE="UNKNOWN", __RC)
       end if
       if (present(units)) then
          call ESMF_AttributeSet(FIELD, NAME='LONG_NAME', VALUE=units(i), __RC)
       else
          call ESMF_AttributeSet(FIELD, NAME='LONG_NAME', VALUE="UNKNOWN", __RC)
       end if
       call MAPL_FieldBundleAdd(B, FIELD, __RC)
    enddo

    deallocate(localIs2D)
    deallocate(localIsEdge)
    __RETURN(ESMF_SUCCESS)

  end function MAPL_BundleCreate


  module function MAPL_TrimString(istring,rc) result(ostring)
    character(len=*), intent(in) :: istring
    integer, optional, intent(out) :: rc

    character(len=:), allocatable :: ostring
    integer :: strlen
    integer :: status

    strlen = len_trim(istring)
    if (istring(strlen:strlen)==char(0)) then
       allocate(ostring,source=istring(1:strlen-1),__STAT)
    else
       allocate(ostring,source=istring(1:strlen),__STAT)
    end if
    __RETURN(__SUCCESS)
  end function MAPL_TrimString

  module subroutine MAPL_FieldSplit(field, fields, aliasName, rc)
    type(ESMF_Field),          intent(IN   ) :: field
    type(ESMF_Field), pointer, intent(  out) :: fields(:)
    character(len=*), optional, intent(in  ) :: aliasName
    integer, optional,         intent(  out) :: rc

    ! local vars
    integer :: status
    integer :: k, n
    integer :: k1,k2
    logical                    :: has_ungrd
    integer                    :: ungrd_cnt
    integer                    :: fieldRank
    integer, allocatable       :: ungrd(:)
    integer, allocatable       :: localMinIndex(:), localMaxIndex(:)
    type(ESMF_Field)           :: f, fld
    character(len=ESMF_MAXSTR) :: name
    character(len=ESMF_MAXSTR) :: splitName
    character(len=ESMF_MAXSTR), allocatable :: splitNameArray(:)
    character(len=ESMF_MAXSTR) :: longName

    call ESMF_FieldGet(field, name=name, __RC)

    call ESMF_FieldGet(FIELD, dimCount=fieldRank, __RC)

    allocate(localMinIndex(fieldRank),localMaxIndex(fieldRank), __STAT)
    call ESMF_FieldGet(Field, &
         localMinIndex=localMinIndex, localMaxIndex=localMaxIndex, __RC)

    k1 = localMinIndex(fieldRank)
    k2 = localMaxIndex(fieldRank)
    deallocate(localMinIndex,localMaxIndex)

    n = k2 - k1 + 1

    allocate(fields(n), __STAT)

    call genAlias(name, n, splitNameArray, aliasName=aliasName,__RC)

    n = 0
    do k=k1,k2
       n = n+1
       splitName = splitNameArray(n)
       f = ESMF_FieldCreate(field, &
            datacopyflag=ESMF_DATACOPY_REFERENCE, &
            trailingUngridSlice=[k], name=splitName, __RC)

       ! copy attributes and adjust as necessary
       fld = field ! shallow copy to get around intent(in/out)
       call MAPL_FieldCopyAttributes(FIELD_IN=fld, FIELD_OUT=f, __RC)

       ! adjust ungridded dims attribute (if any)
       call ESMF_AttributeGet(FIELD, NAME='UNGRIDDED_DIMS', isPresent=has_ungrd, __RC)
       if (has_ungrd) then
          call ESMF_AttributeGet(F, NAME='UNGRIDDED_DIMS', itemcount=UNGRD_CNT, __RC)
          allocate(ungrd(UNGRD_CNT), __STAT)
          call ESMF_AttributeGet(F, NAME='UNGRIDDED_DIMS', valueList=UNGRD, __RC)
          call ESMF_AttributeRemove(F, NAME='UNGRIDDED_DIMS', __RC)
          if (ungrd_cnt > 1) then
             ungrd_cnt = ungrd_cnt - 1
             call ESMF_AttributeSet(F, NAME='UNGRIDDED_DIMS', &
                  valueList=UNGRD(1:ungrd_cnt), __RC)
          else
             has_ungrd = .false.
          end if
          deallocate(ungrd)
       end if

       fields(n) = f
    end do

    deallocate(splitNameArray)
    ! fields SHOULD be deallocated by the caller!!!

!ALT: check if we need to expand "%d" in the long name
!    Note that at this point the original, and each of the split fields
!    have the same long name. We check the original.

    call ESMF_AttributeGet(FIELD, NAME='LONG_NAME', VALUE=longName, __RC)
    if (index(longName, "%d") /= 0) then
       call expandBinNumber(fields, __RC)
    end if


    __RETURN(ESMF_SUCCESS)

  contains
    subroutine expandBinNumber(fields, rc)
      type(ESMF_Field) :: fields(:)
      integer, optional :: rc

      integer :: i, tlen, i1, i2
      character(len=ESMF_MAXSTR) :: longName
      character(len=3) :: tmp
      character(len=ESMF_MAXSTR) :: newLongName

      do i = 1, size(fields)
         call ESMF_AttributeGet(fields(i), NAME='LONG_NAME', VALUE=longName, __RC)
         i1 = index(longName, "%d")
         __ASSERT(i1>0, "Nothing to expand")
         i2 = i1 + 2 ! size of "%d"
         tlen = len_trim(longName)
         __ASSERT(tlen + 1 <= len(longName),'LONG_NAME would exceed MAX length after expansion')
         write(tmp,'(i3.3)') i
         newLongName = longName(1:i1-1)//tmp//trim(longName(i2:tlen))
         ! remove old attribute
         call ESMF_AttributeRemove(fields(i), NAME='LONG_NAME', __RC)
         ! save the new one
         call ESMF_AttributeSet(fields(i), NAME='LONG_NAME', VALUE=newLongName, __RC)
      end do
    __RETURN(ESMF_SUCCESS)
    end subroutine expandBinNumber

    subroutine genAlias(name, n, splitNameArray, aliasName, rc)
      integer :: n
      character(len=*) :: name
      character(len=*), allocatable :: splitNameArray(:)
      character(len=*), optional :: aliasName
      integer, optional :: rc

      integer :: i, k
      integer :: k1, k2, kk, count
      integer :: nn
      character(len=ESMF_MAXSTR), allocatable :: tmp(:)
      character(len=ESMF_MAXSTR) :: aliasName_

      if (present(aliasName)) then
         aliasName_ = aliasName
      else
         aliasName_ = name
      end if

      allocate(splitNameArray(n), __STAT)

      ! parse the aliasName
      ! count the separators (";") in aliasName
      count = 0
      k1 = 1
      kk = len_trim(aliasName_)
      do k=1,kk
         if (aliasName_(k:k) == ";") then
            count = count+1
         end if
      end do
      nn = count+1
      allocate(tmp(nn), __STAT__)

      count = 0
      do k=1,kk
         if (aliasName_(k:k) == ";") then
            count = count+1
            k2=k-1
            if (count > n) exit ! use atmost n of the aliases
            tmp(count) = aliasName_(k1:k2)
            k1 = k+1
         end if
      end do
      count = count+1
      k2 = kk
      tmp(count) = aliasName_(k1:k2)

      do i=1,min(nn,n)
         splitNameArray(i) = tmp(i)
      end do
      deallocate(tmp)
      ! if the user did no supply enough separated alias field names,
      ! append 00i to the original field name
      do i=nn+1,n
         write(splitNameArray(i),'(A,I3.3)') trim(name), i
      end do

      __RETURN(ESMF_SUCCESS)
    end subroutine GenAlias
  end subroutine MAPL_FieldSplit

  module function MAPL_GetCorrectedPhase(gc,rc) result(phase)
     type(ESMF_GridComp), intent(inout) :: gc
     integer, optional, intent(out) :: rc
     integer :: phase

     integer :: status

     call ESMF_GridCompGet(gc,currentPhase=phase,__RC)
     if (phase>10) phase=phase-10
     __RETURN(__SUCCESS)
  end function MAPL_GetCorrectedPhase

  module subroutine MAPL_Reverse_Schmidt(Grid, stretched, npts, lon, lat, lonR8, latR8, lonRe, latRe, rc)
     type(ESMF_Grid), intent(inout) :: Grid
     logical, intent(out)           :: stretched
     integer,                      intent(in   ) :: npts        ! number of points in lat and lon arrays
     real, optional,               intent(in   ) :: lon(npts)   ! array of longitudes in radians
     real, optional,               intent(in   ) :: lat(npts)   ! array of latitudes in radians
     real(ESMF_KIND_R8), optional, intent(in   ) :: lonR8(npts) ! array of longitudes in radians
     real(ESMF_KIND_R8), optional, intent(in   ) :: latR8(npts) !
     real(ESMF_KIND_R8), optional, intent(out  ) :: lonRe(npts) !
     real(ESMF_KIND_R8), optional, intent(out  ) :: latRe(npts) !
     integer, optional, intent(out) :: rc

     logical :: factorPresent, lonPresent, latPresent
     integer :: status
     real(ESMF_KIND_R8) :: c2p1, c2m1, half_pi, two_pi, stretch_factor, target_lon, target_lat
     real(ESMF_KIND_R8), dimension(npts) :: x,y,z, Xx, Yy, Zz
     logical, dimension(npts) :: n_s

     __RETURN_IF( npts == 0 )

     call ESMF_AttributeGet(grid, name='STRETCH_FACTOR', isPresent= factorPresent, __RC)
     call ESMF_AttributeGet(grid, name='TARGET_LON',     isPresent= lonPresent,    __RC)
     call ESMF_AttributeGet(grid, name='TARGET_LAT',     isPresent= latPresent,    __RC)

     if ( factorPresent .and. lonPresent .and. latPresent) then
        stretched = .true.
     else
        stretched = .false.
     endif

     if (present(lonRe) .and. present(latRe)) then
        if (present(lonR8) .and. present(latR8)) then
           lonRe = lonR8
           latRe = latR8
        else if (present(lon) .and. present(lat)) then
           lonRe = lon
           latRe = lat
        else
           __FAIL("Need input to get the output lonRe, latRe")
        endif
     else
        __RETURN(__SUCCESS)
     endif

     if (.not. stretched) then
        __RETURN(__SUCCESS)
     endif

     call ESMF_AttributeGet(grid, name='STRETCH_FACTOR', value=stretch_factor, __RC)
     call ESMF_AttributeGet(grid, name='TARGET_LON',     value=target_lon,     __RC)
     call ESMF_AttributeGet(grid, name='TARGET_LAT',     value=target_lat,     __RC)

     c2p1 = 1 + stretch_factor*stretch_factor
     c2m1 = 1 - stretch_factor*stretch_factor

     half_pi = MAPL_PI_R8/2
     two_pi  = MAPL_PI_R8*2

     target_lon = target_lon*MAPL_DEGREES_TO_RADIANS_R8
     target_lat = target_lat*MAPL_DEGREES_TO_RADIANS_R8

     x = cos(latRe)*cos(lonRe - target_lon)
     y = cos(latRe)*sin(lonRe - target_lon)
     z = sin(latRe)

     Xx =  sin(target_lat)*x - cos(target_lat)*z
     Yy = -y
     Zz = -cos(target_lat)*x - sin(target_lat)*z

     n_s = (1. - abs(Zz)) < 10**(-7)

     where(n_s)
       lonRe = 0.0d0
       latRe = half_pi*sign(1.0d0, Zz)
     elsewhere
       lonRe = atan2(Yy,Xx)
       latRe = asin(Zz)
     endwhere

     if (abs(c2m1) > 10**(-7)) then !# unstretch
        latRe  = asin( (c2m1-c2p1*sin(latRe))/(c2m1*sin(latRe)-c2p1))
     endif

     where ( lonRe < 0)
         lonRe = lonRe + two_pi
     elsewhere (lonRe >= two_pi)
         lonRe = lonRe - two_pi
     endwhere

      __RETURN(__SUCCESS)
  end subroutine

end submodule Base_Implementation
