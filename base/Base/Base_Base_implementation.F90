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
  use mapl_MaplGrid, only: MAPL_GridGet, MAPL_DistGridGet, MAPL_GetImsJms, MAPL_GridHasDE
  use MAPL_ExceptionHandling
  use MAPL_Profiler
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
    type (ESMF_Info) :: infoh

    call ESMF_FieldGet(field, status=fieldStatus, _RC)

    if (fieldStatus /= ESMF_FIELDSTATUS_COMPLETE) then

       !ALT: if the attributeGet calls fail, this would very likely indicate
       !     that the field was NOT created by MAPL (or something terrible happened)
       !     For now we just abort
       call ESMF_InfoGetFromHost(FIELD,infoh,_RC)
       call ESMF_InfoGet(infoh,'DIMS',DIMS,_RC)
       call ESMF_InfoGet(infoh,'VLOCATION',LOCATION,_RC)
       call ESMF_InfoGet(infoh,'HALOWIDTH',HW,_RC)
       call ESMF_InfoGet(infoh,'PRECISION',KND,_RC)
       call ESMF_InfoGet(infoh,'DEFAULT_PROVIDED',defaultProvided,_RC)
       if(defaultProvided) then
          call ESMF_InfoGet(infoh,'DEFAULT_VALUE',default_value,_RC)
       end if

       has_ungrd = ESMF_InfoIsPresent(infoh,'UNGRIDDED_DIMS',_RC)
       if (has_ungrd) then
          call ESMF_InfoGet(infoh,key='UNGRIDDED_DIMS',size=UNGRD_CNT,_RC)
          allocate(ungrd(UNGRD_CNT), _STAT)
          call ESMF_InfoGet(infoh,key='UNGRIDDED_DIMS',values=UNGRD,_RC)
          if (defaultProvided) then
             call MAPL_FieldAllocCommit(field, dims=dims, location=location, typekind=knd, &
                  hw=hw, ungrid=ungrd, default_value=default_value, _RC)
          else
             call MAPL_FieldAllocCommit(field, dims=dims, location=location, typekind=knd, &
                  hw=hw, ungrid=ungrd, _RC)
          end if
       else
          if (defaultProvided) then
             call MAPL_FieldAllocCommit(field, dims=dims, location=location, typekind=knd, &
                  hw=hw, default_value=default_value, _RC)
          else
             call MAPL_FieldAllocCommit(field, dims=dims, location=location, typekind=knd, &
                  hw=hw, _RC)
          end if
       end if

       if (has_ungrd) then
          deallocate(ungrd)
       end if

    end if

    _RETURN(ESMF_SUCCESS)
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
    type(ESMF_Info)                       :: infoh

! SSI
    type(ESMF_Pin_Flag) :: pinflag
    type(ESMF_VM)       :: vm
    logical             :: ssiSharedMemoryEnabled
! SSI

    call ESMF_FieldGet(field, grid=GRID, _RC)
    call MAPL_GridGet(GRID, localCellCountPerDim=COUNTS, _RC)
    call ESMF_GridGet(GRID, dimCount=gridRank, _RC)
    _ASSERT(gridRank <= 3,' MAPL restriction - only 2 and 3d are supported')
    allocate(gridToFieldMap(gridRank), _STAT)
    gridToFieldMap = 0
    do I = 1, gridRank
       gridToFieldMap(I) = I
    end do
    ! ALT: the next allocation should have been griddedDims,
    !      but this compilcates the code unnecessery
    allocate(haloWidth(gridRank), _STAT)
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
    call ESMF_VMGetCurrent(vm, _RC)

    call ESMF_VMGet(vm, ssiSharedMemoryEnabledFlag=ssiSharedMemoryEnabled, _RC)

    ! call pinflag getter
    pinflag = MAPL_PinFlagGet()

    if (any(pinflag == [ESMF_PIN_DE_TO_SSI,ESMF_PIN_DE_TO_SSI_CONTIG])) then
       _ASSERT(ssiSharedMemoryEnabled, 'SSI shared memory is NOT supported')
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
             allocate(VAR_1D(UNGRID(1)), _STAT)
             VAR_1D = INIT_VALUE
             call ESMF_FieldEmptyComplete(FIELD, farray=VAR_1D,    &
                  indexflag=ESMF_INDEX_DELOCAL, &
                  datacopyFlag = ESMF_DATACOPY_REFERENCE,             &
                  gridToFieldMap=gridToFieldMap,                      &
                  ungriddedLBound=[1],&
                  ungriddedUBound=[ungrid(1)], &
                  _RC)
          case default
             _FAIL( 'unsupported rank > 1')
          end select

       else
          select case (rank)
          case (1)
             allocate(VR8_1D(UNGRID(1)), _STAT)
             VR8_1D = INIT_VALUE
             call ESMF_FieldEmptyComplete(FIELD, farray=VR8_1D,    &
                  indexflag=ESMF_INDEX_DELOCAL, &
                  datacopyFlag = ESMF_DATACOPY_REFERENCE,             &
                  gridToFieldMap=gridToFieldMap,                      &
                  ungriddedLBound=[1],&
                  ungriddedUBound=[ungrid(1)], &
                  _RC)
          case default
             _FAIL( 'unsupported rank > 1')
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
          _RETURN(ESMF_FAILURE)
       end select

       if (typekind == ESMF_KIND_R4) then
          allocate(VAR_1D(lb1:ub1), _STAT)
          VAR_1D = INIT_VALUE

          call ESMF_FieldEmptyComplete(FIELD, farray=var_1d,  &
               indexflag=ESMF_INDEX_DELOCAL, &
               datacopyFlag = ESMF_DATACOPY_REFERENCE,             &
               gridToFieldMap=gridToFieldMap,                           &
               ungriddedLBound=[lb1],&
               ungriddedUBound=[ub1], &
               _RC)
       else
          allocate(VR8_1D(lb1:ub1), _STAT)
          VR8_1D = INIT_VALUE

          call ESMF_FieldEmptyComplete(FIELD, farray=vr8_1d,  &
               indexflag=ESMF_INDEX_DELOCAL, &
               datacopyFlag = ESMF_DATACOPY_REFERENCE,             &
               gridToFieldMap=gridToFieldMap,                           &
               ungriddedLBound=[lb1],&
               ungriddedUBound=[ub1], &
               _RC)
       end if

       ! Horizontal only
       ! ---------------

    case(MAPL_DimsHorzOnly)
       rank = 2 + szungrd
       _ASSERT(rank <= 4, 'unsupported rank > 4 (UNGRD not fully implemented)')
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
                  pinflag=pinflag, _RC)
             call ESMF_FieldGet(FIELD, farrayPtr=VAR_2D, _RC)
             VAR_2D = INIT_VALUE
          case (3)
             call ESMF_FieldEmptyComplete(FIELD, typekind=ESMF_TYPEKIND_R4, &
                  gridToFieldMap=gridToFieldMap,              &
                  totalLWidth=haloWidth(1:griddedDims),     &
                  totalUWidth=haloWidth(1:griddedDims),     &
                  ungriddedLBound=(/1/), ungriddedUBound=(/UNGRID(1)/),  &
                  pinflag=pinflag,_RC)
             call ESMF_FieldGet(FIELD, farrayPtr=VAR_3D, _RC)
             VAR_3D = INIT_VALUE
          case (4)
             call ESMF_FieldEmptyComplete(FIELD, typekind=ESMF_TYPEKIND_R4, &
                  gridToFieldMap=gridToFieldMap,              &
                  totalLWidth=haloWidth(1:griddedDims),     &
                  totalUWidth=haloWidth(1:griddedDims),     &
                  ungriddedLBound=(/1,1/), ungriddedUBound=(/UNGRID(1),UNGRID(2)/), &
                pinflag=pinflag, _RC)
             call ESMF_FieldGet(FIELD, farrayPtr=VAR_4D, _RC)
             VAR_4D = INIT_VALUE
          case default
             _FAIL('only up to 4D are supported')
          end select RankCase2d
       else
          select case (rank)
          case (2)
          call ESMF_FieldEmptyComplete(FIELD, typekind=ESMF_TYPEKIND_R8, &
               gridToFieldMap=gridToFieldMap,                   &
               totalLWidth=haloWidth(1:griddedDims),            &
               totalUWidth=haloWidth(1:griddedDims),            &
                pinflag=pinflag, _RC)
          call ESMF_FieldGet(FIELD, farrayPtr=VR8_2D, _RC)
          VR8_2D = INIT_VALUE
          case (3)
          call ESMF_FieldEmptyComplete(FIELD, typekind=ESMF_TYPEKIND_R8, &
               gridToFieldMap=gridToFieldMap,                   &
               totalLWidth=haloWidth(1:griddedDims),            &
               totalUWidth=haloWidth(1:griddedDims),            &
               ungriddedLBound=(/1/), ungriddedUBound=(/UNGRID(1)/),  &
               pinflag=pinflag, _RC)
          call ESMF_FieldGet(FIELD, farrayPtr=VR8_3D, _RC)
          VR8_3D = INIT_VALUE
          case (4)
          call ESMF_FieldEmptyComplete(FIELD, typekind=ESMF_TYPEKIND_R8, &
               gridToFieldMap=gridToFieldMap,              &
               totalLWidth=haloWidth(1:griddedDims),     &
               totalUWidth=haloWidth(1:griddedDims),     &
               ungriddedLBound=(/1,1/), ungriddedUBound=(/UNGRID(1),UNGRID(2)/), &
               pinflag=pinflag, _RC)
          call ESMF_FieldGet(FIELD, farrayPtr=VR8_4D, _RC)
          VR8_4D = INIT_VALUE
          case default
             _FAIL('only up to 4D are supported')
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
          _RETURN(ESMF_FAILURE)
       end select

       RankCase3d: select case(rank)
       case (3)
          if (typekind == ESMF_KIND_R4) then
             call ESMF_FieldEmptyComplete(FIELD, typekind=ESMF_TYPEKIND_R4, &
                  gridToFieldMap=gridToFieldMap,              &
                  totalLWidth=haloWidth(1:griddedDims),     &
                  totalUWidth=haloWidth(1:griddedDims),     &
                  ungriddedLBound=(/lb3/), ungriddedUBound=(/ub3/),  &
                  pinflag=pinflag, _RC)
             call ESMF_FieldGet(FIELD, farrayPtr=VAR_3D, _RC)
             VAR_3D = INIT_VALUE
          else
             call ESMF_FieldEmptyComplete(FIELD, typekind=ESMF_TYPEKIND_R8, &
                  gridToFieldMap=gridToFieldMap,              &
                  totalLWidth=haloWidth(1:griddedDims),     &
                  totalUWidth=haloWidth(1:griddedDims),     &
                  ungriddedLBound=(/lb3/), ungriddedUBound=(/ub3/),  &
                  pinflag=pinflag, _RC)
             call ESMF_FieldGet(FIELD, farrayPtr=VR8_3D, _RC)
             VR8_3D = INIT_VALUE
          endif

       case (4)
          if (typekind == ESMF_KIND_R4) then
             call ESMF_FieldEmptyComplete(FIELD, typekind=ESMF_TYPEKIND_R4, &
                  gridToFieldMap=gridToFieldMap,              &
                  totalLWidth=haloWidth(1:griddedDims),     &
                  totalUWidth=haloWidth(1:griddedDims),     &
                  ungriddedLBound=(/lb3,1/), ungriddedUBound=(/ub3,ungrid(1)/),  &
                   pinflag=pinflag, _RC)
             call ESMF_FieldGet(FIELD, farrayPtr=VAR_4D, _RC)
             VAR_4D = INIT_VALUE
          else
             call ESMF_FieldEmptyComplete(FIELD, typekind=ESMF_TYPEKIND_R8, &
                  gridToFieldMap=gridToFieldMap,              &
                  totalLWidth=haloWidth(1:griddedDims),     &
                  totalUWidth=haloWidth(1:griddedDims),     &
                  ungriddedLBound=(/lb3,1/), ungriddedUBound=(/ub3,ungrid(1)/),  &
                   pinflag=pinflag, _RC)
             call ESMF_FieldGet(FIELD, farrayPtr=VR8_4D, _RC)
             VR8_4D = INIT_VALUE
          endif

       case default
          _RETURN(ESMF_FAILURE)
       end select RankCase3d

       ! Tiles
       ! -----
    case(MAPL_DimsTileOnly)
       rank = 1 + szungrd
       _ASSERT(gridRank == 1, 'gridRank /= 1')

       if (typekind == ESMF_KIND_R4) then
          select case (rank)
          case (1)
             allocate(VAR_1D(COUNTS(1)), _STAT)
             VAR_1D = INIT_VALUE
             call ESMF_FieldEmptyComplete(FIELD, farray=VAR_1D,    &
                  indexflag=ESMF_INDEX_DELOCAL, &
                  datacopyFlag = ESMF_DATACOPY_REFERENCE,         &
                  _RC)
          case (2)
             allocate(VAR_2D(COUNTS(1),UNGRID(1)), _STAT)
             VAR_2D = INIT_VALUE
             call ESMF_FieldEmptyComplete(FIELD, farray=VAR_2D,    &
                  indexflag=ESMF_INDEX_DELOCAL, &
                  gridToFieldMap=gridToFieldMap,                      &
                  datacopyFlag = ESMF_DATACOPY_REFERENCE,             &
                  _RC)
          case (3)
             allocate(VAR_3D(COUNTS(1), UNGRID(1), UNGRID(2)), _STAT)
             VAR_3D = INIT_VALUE
             call ESMF_FieldEmptyComplete(FIELD, farray=VAR_3D,    &
                  indexflag=ESMF_INDEX_DELOCAL, &
                  gridToFieldMap=gridToFieldMap,                      &
                  datacopyFlag = ESMF_DATACOPY_REFERENCE,             &
                  _RC)
          case default
             _FAIL( 'only 2D and 3D are supported')
          end select

       else
          select case (rank)
          case (1)
             allocate(VR8_1D(COUNTS(1)), _STAT)
             VR8_1D = INIT_VALUE
             call ESMF_FieldEmptyComplete(FIELD, farray=VR8_1D,    &
                  indexflag=ESMF_INDEX_DELOCAL, &
                  datacopyFlag = ESMF_DATACOPY_REFERENCE,         &
                  _RC)
          case (2)
             allocate(VR8_2D(COUNTS(1),UNGRID(1)), _STAT)
             VR8_2D = INIT_VALUE
             call ESMF_FieldEmptyComplete(FIELD, farray=VR8_2D,    &
                  indexflag=ESMF_INDEX_DELOCAL, &
                  gridToFieldMap=gridToFieldMap,                      &
                  datacopyFlag = ESMF_DATACOPY_REFERENCE,             &
                  _RC)
          case (3)
             allocate(VR8_3D(COUNTS(1), UNGRID(1), UNGRID(2)), _STAT)
             VR8_3D = INIT_VALUE
             call ESMF_FieldEmptyComplete(FIELD, farray=VR8_3D,    &
                  indexflag=ESMF_INDEX_DELOCAL, &
                  gridToFieldMap=gridToFieldMap,                      &
                  datacopyFlag = ESMF_DATACOPY_REFERENCE,             &
                  _RC)
          case default
             _FAIL( 'only 2D and 3D are supported')
          end select

       endif

    case(MAPL_DimsTileTile)
       rank=2
       _ASSERT(gridRank == 1, 'gridRank /= 1')

       if (typekind == ESMF_KIND_R4) then
          allocate(VAR_2D(COUNTS(1), COUNTS(2)), _STAT)
          VAR_2D = INIT_VALUE
          call ESMF_FieldEmptyComplete(FIELD, farray=VAR_2D,    &
               indexflag=ESMF_INDEX_DELOCAL, &
               datacopyFlag = ESMF_DATACOPY_REFERENCE,             &
                                !                  ungriddedLBound = (/1/),                      &
                                !                  ungriddedUBound = (/counts(2)/),              &
               _RC)
       else
          allocate(VR8_2D(COUNTS(1), COUNTS(2)), _STAT)
          VR8_2D = INIT_VALUE
          call ESMF_FieldEmptyComplete(FIELD, farray=VR8_2D,    &
               indexflag=ESMF_INDEX_DELOCAL, &
               datacopyFlag = ESMF_DATACOPY_REFERENCE,             &
                                !                  ungriddedLBound = (/1/),                      &
                                !                  ungriddedUBound = (/counts(2)/),              &
               _RC)
       endif

       ! Invalid dimensionality
       ! ----------------------

    case default
       _RETURN(ESMF_FAILURE)

    end select Dimensionality

    if (present(default_value)) then
       block
         type(ESMF_Info) :: attr_infoh
         type(ESMF_Array) :: attr_array
         type(ESMF_FieldStatus_Flag) :: attr_fieldStatus
         call ESMF_InfoGetFromHost(field, attr_infoh, _RC)
         call ESMF_InfoSet(attr_infoh, "MAPL_InitStatus", int(MAPL_InitialDefault), _RC)
         call ESMF_FieldGet(field, status=attr_fieldStatus, _RC)
         if (attr_fieldStatus == ESMF_FIELDSTATUS_COMPLETE) then
            call ESMF_FieldGet(field, Array=attr_array, _RC)
            call ESMF_InfoGetFromHost(attr_array, attr_infoh, _RC)
            call ESMF_InfoSet(attr_infoh, "MAPL_InitStatus", int(MAPL_InitialDefault), _RC)
         end if
       end block
    end if

    ! Clean up
    deallocate(haloWidth)
    deallocate(gridToFieldMap)


    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_FieldAllocCommit




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
    type(ESMF_Info)                        :: infoh

    call ESMF_InfoGetFromHost(FIELD,infoh,_RC)
    isPresent = ESMF_InfoIsPresent(infoh,'TimeStamp',_RC)
    if(.not. isPresent) then
       call ESMF_TimeSet          (TIME,      YY=0,                _RC)
    else
       call ESMF_InfoGet(infoh,'TimeStamp',TIMESTAMP,_RC)

       call MAPL_TimeStringGet    (TIMESTAMP, YY=YEAR, MM=MONTH,  DD=DAY,   &
            H =HOUR, M =MINUTE, S =SCND   )
       call ESMF_TimeSet          (TIME,      YY=YEAR, MM=MONTH,  DD=DAY,   &
            H =HOUR, M =MINUTE, S =SCND,  &
            _RC)
    end if

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_GetFieldTimeFromField

  ! ------------------------------------------------------------------------------

  module subroutine  MAPL_SetFieldTimeFromField (FIELD, TIME, RC )
    type(ESMF_FIELD),        intent(INOUT) :: FIELD
    type(ESMF_TIME),         intent(INOUT) :: TIME !ALT: IN
    integer, optional,       intent(  OUT) :: RC

    character(len=ESMF_MAXSTR),parameter   :: IAm=" MAPL_SetFieldTimeFromField"
    integer                                :: STATUS

    character(len=ESMF_MAXSTR)             :: TIMESTAMP
    type(ESMF_Info)                        :: infoh

    call ESMF_TimeGet(TIME,timeString=TIMESTAMP,_RC)
    call ESMF_InfoGetFromHost(FIELD,infoh,_RC)
    call ESMF_InfoSet(infoh,'TimeStamp',TIMESTAMP,_RC)

    _RETURN(ESMF_SUCCESS)
  end subroutine  MAPL_SetFieldTimeFromField


  module subroutine  MAPL_GetFieldTimeFromState ( STATE, Fieldname, TIME, RC )
    type(ESMF_STATE),        intent(IN   ) :: STATE
    character(len=*),        intent(IN   ) :: Fieldname
    type(ESMF_Time),         intent(  OUT) :: TIME
    integer, optional,       intent(  OUT) :: RC

    character(len=ESMF_MAXSTR),parameter   :: IAm=" MAPL_GetFieldTimeFromState"
    integer                                :: STATUS

    type(ESMF_FIELD)                       :: FIELD

    call ESMF_StateGet (STATE, FIELDNAME, FIELD, _RC )
    call MAPL_FieldGetTime  (FIELD, TIME,             _RC)

    _RETURN(ESMF_SUCCESS)
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

    call ESMF_StateGet (STATE, FIELDNAME, FIELD, _RC)
    call MAPL_FieldSetTime  (FIELD, TIME,             _RC)

    _RETURN(ESMF_SUCCESS)
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

    f = ESMF_FieldCreate(field, datacopyflag=datacopy, name=NAME, _RC)

    call MAPL_FieldCopyAttributes(FIELD_IN=field, FIELD_OUT=f, _RC)

    _RETURN(ESMF_SUCCESS)
  end function MAPL_FieldCreateRename

  module function MAPL_FieldCreateNewgrid(FIELD, GRID, LM, NEWNAME, RC) RESULT(F)
    type (ESMF_Field), intent(INOUT) :: FIELD !ALT: intent(IN)
    type (ESMF_Grid),  intent(INout) :: GRID
    integer, optional, intent(IN   ) :: lm
    character(len=*), optional, intent(IN) :: newName
    integer, optional, intent(  OUT) :: RC
    type (ESMF_Field)                :: F
    type (ESMF_Info)                 :: infoh

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
    logical :: has_de

    call ESMF_FieldGet(FIELD, grid=fgrid, _RC)

    call ESMF_GridGet(fGRID, dimCount=fgridRank, _RC)
    allocate(gridToFieldMap(fgridRank), _STAT)
    call ESMF_FieldGet(FIELD, Array=Array, name=name, &
         gridToFieldMap=gridToFieldMap, _RC)
    griddedDims = fgridRank - count(gridToFieldMap == 0)

    call ESMF_GridGet(GRID, dimCount=gridRank, _RC)

    call ESMF_ArrayGet(array, rank=rank, _RC)
    ungriddedDims = rank - griddedDims

    call MAPL_GridGet(GRID, localCellCountPerDim=COUNTS, _RC)

    call ESMF_ArrayGet(array, localDeCount=localDeCount, _RC)
    _ASSERT(localDeCount == 1, 'MAPL supports only 1 local array')
    call ESMF_ArrayGet(array, localarrayList=larrayList, _RC)
    larray => lArrayList(1) ! alias

    call ESMF_LocalArrayGet(larray, totalLBound=lbnds, totalUBound=ubnds, _RC)

    newRank = rank
    if (griddedDims == 1 .and. gridRank > 1) then
       deallocate(gridToFieldMap)
       allocate(gridToFieldMap(gridRank), _STAT)
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
            name=newName_, gridToFieldMap=gridToFieldMap, _RC )
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
            ungriddedLBound=[lb],ungriddedUBound=[ub],_RC )
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
            ungriddedUBound=[ubnds(griddedDims+1),ubnds(griddedDims+2)],_RC )
       if (ungriddedDims > 0) then
          DIMS = MAPL_DimsHorzOnly
       else
          DIMS = MAPL_DimsHorzVert
       end if
    else
       _FAIL( 'rank > 4 not supported')
    end if

    deallocate(gridToFieldMap)

    call MAPL_FieldCopyAttributes(FIELD_IN=field, FIELD_OUT=f, _RC)
    ! we are saving DIMS attribute in case the FIELD did not contain one
    ! otherwise we will overwrite it
    call ESMF_InfoGetFromHost(F,infoh,_RC)
    call ESMF_InfoSet(infoh,'DIMS',DIMS,_RC)

    has_de = MAPL_GridHasDe(grid, _RC)
    if (has_de) then
       call assign_fptr(f, ptr1d, _RC)
       ptr1d = 0.0
    end if

    _RETURN(ESMF_SUCCESS)
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
         name=fieldName, typekind=tk, _RC)
    _ASSERT(tk == ESMF_TypeKind_R8, 'tk /= ESMF_TypeKind_R8')
    call ESMF_GridGet(GRID, dimCount=gridRank, _RC)
    allocate(gridToFieldMap(gridRank), _STAT)
    call ESMF_FieldGet(FIELD, gridToFieldMap=gridToFieldMap, _RC)

    datacopy = ESMF_DATACOPY_REFERENCE

    select case (fieldRank)
    case (1)
       call ESMF_FieldGet(field, farrayPtr=vr8_1d, _RC)
       allocate(var_1d(lbound(vr8_1d,1):ubound(vr8_1d,1)), _STAT)
       var_1d=vr8_1d
       f = MAPL_FieldCreateEmpty(name=fieldNAME, grid=grid, _RC)
       call ESMF_FieldEmptyComplete(F, farrayPtr=VAR_1D,    &
            gridToFieldMap=gridToFieldMap,                      &
            datacopyFlag = datacopy,             &
            _RC)
    case (2)
       call ESMF_FieldGet(field, farrayPtr=vr8_2d, _RC)
       allocate(var_2d(lbound(vr8_2d,1):ubound(vr8_2d,1), &
            lbound(vr8_2d,2):ubound(vr8_2d,2)), &
            _STAT)
       var_2d=vr8_2d
       f = MAPL_FieldCreateEmpty(name=fieldNAME, grid=grid, _RC)
       call ESMF_FieldEmptyComplete(F, farrayPtr=VAR_2D,    &
            gridToFieldMap=gridToFieldMap,                      &
            datacopyFlag = datacopy,             &
            _RC)
    case (3)
       call ESMF_FieldGet(field, farrayPtr=vr8_3d, _RC)
       allocate(var_3d(lbound(vr8_3d,1):ubound(vr8_3d,1), &
            lbound(vr8_3d,2):ubound(vr8_3d,2), &
            lbound(vr8_3d,3):ubound(vr8_3d,3)), &
            _STAT)
       var_3d=vr8_3d
       f = MAPL_FieldCreateEmpty(name=fieldNAME, grid=grid, _RC)
       call ESMF_FieldEmptyComplete(F, farrayPtr=VAR_3D,    &
            gridToFieldMap=gridToFieldMap,                      &
            datacopyFlag = datacopy,             &
            _RC)
    case default
       _FAIL( 'only 2D and 3D are supported')
    end select

    deallocate(gridToFieldMap)

    call MAPL_FieldCopyAttributes(FIELD_IN=field, FIELD_OUT=f, _RC)

    _RETURN(ESMF_SUCCESS)
  end function MAPL_FieldCreateR4

  module function MAPL_FieldCreateEmpty(NAME, GRID, RC) RESULT(FIELD)
    character(len=*),  intent(IN   ) :: NAME
    type (ESMF_Grid),  intent(INout) :: GRID
    integer, optional, intent(  OUT) :: RC
    type (ESMF_Field)                :: FIELD

    character(len=ESMF_MAXSTR),parameter   :: IAm=" MAPL_FieldCreateEmpty"
    integer                                :: STATUS

    FIELD = ESMF_FieldEmptyCreate(name=name, _RC)

    call ESMF_FieldEmptySet(FIELD, &
         grid=GRID, &
         staggerloc = ESMF_STAGGERLOC_CENTER,        &
         _RC)

    _RETURN(ESMF_SUCCESS)

  end function MAPL_FieldCreateEmpty

  module subroutine MAPL_FieldCopyAttributes(FIELD_IN, FIELD_OUT, RC)
    type (ESMF_Field), intent(INOUT) :: FIELD_IN !ALT: intent(in)
    type (ESMF_Field), intent(INOUT) :: FIELD_OUT
    integer, optional, intent(  OUT) :: RC
    integer                          :: status

    type(ESMF_Info) :: info_in, info_out

    call ESMF_InfoGetFromHost(field_in, info_in,_RC)

    call ESMF_InfoGetFromHost(field_out, info_out, _RC)

    call ESMF_InfoSet(info_out, key="", value=info_in, _RC)

    _RETURN(ESMF_SUCCESS)
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
         typekind=tk, _RC)
    _ASSERT(tk == ESMF_TypeKind_R8, 'inconsistent typekind (should be ESMF_TypeKind_R8)')

    select case (fieldRank)
    case (1)
       call ESMF_FieldGet(from, farrayPtr=vr8_1d, _RC)
       call ESMF_FieldGet(to, dimCount=fieldRank, typekind=tk, _RC)
       _ASSERT(tk == ESMF_TypeKind_R4, 'inconsistent typekind (should be ESMF_TypeKind_R4)')
       _ASSERT(fieldRank==1, 'inconsistent fieldrank (should be 1)')
       call ESMF_FieldGet(to, farrayPtr=var_1d, _RC)
       var_1d = vr8_1d
    case (2)
       call ESMF_FieldGet(from, farrayPtr=vr8_2d, _RC)
       call ESMF_FieldGet(to, dimCount=fieldRank, typekind=tk, _RC)
       _ASSERT(tk == ESMF_TypeKind_R4, 'inconsistent typekind (should be ESMF_TypeKind_R4)')
       _ASSERT(fieldRank==2, 'inconsistent fieldRank (should be 2)')
       call ESMF_FieldGet(to, farrayPtr=var_2d, _RC)
       var_2d = vr8_2d
    case (3)
       call ESMF_FieldGet(from, farrayPtr=vr8_3d, _RC)
       call ESMF_FieldGet(to, dimCount=fieldRank, typekind=tk, _RC)
       _ASSERT(tk == ESMF_TypeKind_R4, 'inconsistent typekind (should be ESMF_TypeKind_R4)')
       _ASSERT(fieldRank==3,'inconsistent fieldRank (should be 3)')
       call ESMF_FieldGet(to, farrayPtr=var_3d, _RC)
       var_3d = vr8_3d
    case default
       _FAIL( 'unsupported fieldRank (> 3)')
    end select

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_FieldCopy



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
    type(ESMF_Info) :: infoh

    i1=-1
    j1=-1
    in=-1
    jn=-1

    call ESMF_InfoGetFromHost(grid,infoh,_RC)
    isPresent = ESMF_InfoIsPresent(infoh,'GLOBAL_GRID_INFO',_RC)
    if (isPresent) then
      call ESMF_InfoGetAlloc(infoh, key="GLOBAL_GRID_INFO", values=global_grid_info, _RC)
      I1 = global_grid_info(7)
      IN = global_grid_info(8)
      j1 = global_grid_info(9)
      JN = global_grid_info(10)
      deallocate(global_grid_info, _STAT)
      _RETURN(_SUCCESS)
    end if

    call ESMF_GridGet    (GRID, dimCount=gridRank, distGrid=distGrid, _RC)
    call ESMF_DistGridGet(distGRID, delayout=layout, _RC)
    call ESMF_DELayoutGet(layout, deCount = nDEs, localDeCount=localDeCount,_RC)
    if (localDeCount > 0) then
       allocate(localDeToDeMap(localDeCount),_STAT)
       call ESMF_DELayoutGet(layout, localDEtoDeMap=localDeToDeMap,_RC)
       deId=localDeToDeMap(1)

       allocate (AL(gridRank,0:nDEs-1),  _STAT)
       allocate (AU(gridRank,0:nDEs-1),  _STAT)

       call MAPl_DistGridGet(distgrid, &
            minIndex=AL, maxIndex=AU, _RC)

       I1 = AL(1, deId)
       IN = AU(1, deId)
       !    _ASSERT(gridRank > 1, 'tilegrid is 1d (without RC this only for info')
       J1 = 1
       JN = 1
       if (gridRank > 1) then
         J1 = AL(2, deId)
         JN = AU(2, deId)
       endif
       deallocate(AU, AL, localDeToDeMap)
    end if

  end subroutine MAPL_GRID_INTERIOR


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
    type(ESMF_Info) :: infoh

    call MAPL_GridGet(grid,localCellCountPerDim=counts,_RC)
    im=counts(1)
    jm=counts(2)
    ! check if we have corners
    call ESMF_InfoGetFromHost(grid,infoh,_RC)
    hasLons = ESMF_InfoIsPresent(infoh,'GridCornerLons',_RC)
    hasLats = ESMF_InfoIsPresent(infoh,'GridCornerLats',_RC)
    if (hasLons .and. hasLats) then
       call ESMF_InfoGet(infoh,key='GridCornerLons',size=lsz,_RC)
       _ASSERT(size(gridCornerLons,1)*size(gridCornerLons,2)==lsz,"stored corner sizes to not match grid")
       call ESMF_InfoGet(infoh,key='GridCornerLats',size=lsz,_RC)
       _ASSERT(size(gridCornerLats,1)*size(gridCornerLats,2)==lsz,"stored corner sizes to not match grid")
       allocate(r8ptr(lsz),_STAT)

       call ESMF_InfoGet(infoh,key='GridCornerLons',values=r8ptr,_RC)

       idx = 0
       do j = 1, size(gridCornerLons,2)
          do i = 1, size(gridCornerLons,1)
             idx = idx+1
             gridCornerLons(i,j) = r8ptr(idx)
          end do
       end do

       call ESMF_InfoGet(infoh,key='GridCornerLats',values=r8ptr,_RC)

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
            farrayPtr=corner, _RC)
       imc=size(corner,1)
       jmc=size(corner,2)
       allocate(ptr(0:imc+1,0:jmc+1),source=0.0d0,_STAT)
       field = ESMF_FieldCreate(grid,ptr,staggerLoc=ESMF_STAGGERLOC_CORNER,totalLWidth=[1,1],totalUWidth=[1,1],_RC)
       call ESMF_FieldHaloStore(field,rh,_RC)

       ptr(1:imc,1:jmc)=corner
       call ESMF_FieldHalo(field,rh,_RC)
       gridCornerLons=ptr(1:im+1,1:jm+1)

       call ESMF_GridGetCoord(grid,localDE=0,coordDim=2,staggerloc=ESMF_STAGGERLOC_CORNER, &
            farrayPtr=corner, _RC)
       ptr(1:imc,1:jmc)=corner
       call ESMF_FieldHalo(field,rh,_RC)
       gridCornerLats=ptr(1:im+1,1:jm+1)

       deallocate(ptr)
       call ESMF_FieldDestroy(field,_RC)
       call ESMF_FieldHaloRelease(rh,_RC)

       call ESMF_GridGet(grid,coordSys=coordSys,_RC)
       if (coordSys==ESMF_COORDSYS_SPH_DEG) then
          gridCornerLons=gridCornerLons*MAPL_DEGREES_TO_RADIANS_R8
          gridCornerLats=gridCornerLats*MAPL_DEGREES_TO_RADIANS_R8
       else if (coordSys==ESMF_COORDSYS_CART) then
          _FAIL('Unsupported coordinate system:  ESMF_COORDSYS_CART')
       end if
       allocate(lons1d(size(gridCornerLons,1)*size(gridCornerLons,2)),_STAT)
       allocate(lats1d(size(gridCornerLons,1)*size(gridCornerLons,2)),_STAT)
       idx = 0
       do j=1,size(gridCornerLons,2)
          do i=1,size(gridCornerLons,1)
             idx=idx+1
             lons1d(idx)=gridCornerLons(i,j)
             lats1d(idx)=gridCornerLats(i,j)
          enddo
       enddo
       call ESMF_InfoGetFromHost(grid,infoh,_RC)
       call ESMF_InfoSet(infoh,key='GridCornerLons:',values=lons1d,_RC)
       call ESMF_InfoSet(infoh,key='GridCornerLats:',values=lats1d,_RC)
       deallocate(lons1d,lats1d)
    end if

    _RETURN(ESMF_SUCCESS)

  end subroutine MAPL_GridGetCorners

  !.......................................................................




  module subroutine MAPL_FieldBundleDestroy(Bundle,NoGarbage,RC)
    type(ESMF_FieldBundle),    intent(INOUT) :: Bundle
    logical, optional,         intent(IN   ) :: NoGarbage
    integer, optional,         intent(OUT  ) :: RC

    integer                               :: I
    integer                               :: FieldCount
    type(ESMF_Field)                      :: Field
    logical                               :: isCreated

    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_FieldBundleDestroy"
    integer                               :: STATUS


    isCreated = ESMF_FieldBundleIsCreated(bundle,_RC)
    if(isCreated) then
       call ESMF_FieldBundleGet(BUNDLE, FieldCount=FIELDCOUNT, _RC)

       do I = 1, FIELDCOUNT
          call ESMF_FieldBundleGet(BUNDLE, I, FIELD, _RC)
          call MAPL_FieldDestroy(FIELD, _RC)
       end do
       call ESMF_FieldBundleDestroy(bundle, NoGarbage=NoGarbage, _RC)
    end if

    _RETURN(ESMF_SUCCESS)

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
    type(ESMF_Info)                         :: infoh

    fields(1) = field
    call ESMF_StateAdd(state, fields, _RC)
    !=================
!!!ALT Example to add one field at the time (not used anymore)
!!!      call ESMF_StateAdd(STATE, FIELD, proxyflag=.false., &
!!!           addflag=.true., replaceflag=.false., _RC )
    !=================

    ! check for attribute

    call ESMF_InfoGetFromHost(state,infoh,_RC)
    haveAttr = ESMF_InfoIsPresent(infoh,attrName,_RC)
    if (haveAttr) then
       call ESMF_InfoGet(infoh,key=attrName,size=natt,_RC)
    else
       natt = 0
    end if
    allocate(currList(natt), _STAT)

    if (natt > 0) then
       ! get the current list
       call ESMF_InfoGet(infoh,key=attrName,values=currList,_RC)
       !ALT delete/destroy this attribute to prevent memory leaks
       call ESMF_InfoRemove(infoh,attrName,_RC)
    end if

    na = natt+1
    allocate(thisList(na), _STAT)

    thisList(1:natt) = currList

    call ESMF_FieldGet(field, name=name, _RC)

    thisList(na) = name

    call ESMF_InfoSet(infoh,key=attrName,values=thisList,_RC)

    deallocate(thisList)
    deallocate(currList)

    _RETURN(ESMF_SUCCESS)
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
    type(ESMF_Info)                         :: infoh

    bundles(1) = bundle
    call ESMF_StateAdd(state, Bundles, _RC)

    ! check for attribute

    call ESMF_InfoGetFromHost(state,infoh,_RC)
    haveAttr = ESMF_InfoIsPresent(infoh,attrName,_RC)
    if (haveAttr) then
       call ESMF_InfoGet(infoh,key=attrName,size=natt,_RC)
    else
       natt = 0
    end if
    allocate(currList(natt), _STAT)

    if (natt > 0) then
       ! get the current list
       call ESMF_InfoGet(infoh,key=attrName,values=currList,_RC)
       !ALT delete/destroy this attribute to prevent memory leaks
       call ESMF_InfoRemove(infoh,attrName,_RC)
    end if

    na = natt+1
    allocate(thisList(na), _STAT)

    thisList(1:natt) = currList

    call ESMF_FieldBundleGet(bundle, name=name, _RC)

    thisList(na) = name

    call ESMF_InfoSet(infoh,key=attrName,values=thisList,_RC)

    deallocate(thisList)
    deallocate(currList)

    _RETURN(ESMF_SUCCESS)
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
    type(ESMF_Info)                         :: infoh


    fields(1) = field
    call ESMF_FieldBundleAdd(Bundle, fields, multiflag=multiflag, _RC)

    ! check for attribute
    call ESMF_InfoGetFromHost(Bundle,infoh,_RC)
    haveAttr = ESMF_InfoIsPresent(infoh,attrName,_RC)
    if (haveAttr) then
       call ESMF_InfoGet(infoh,key=attrName,size=natt,_RC)
    else
       natt = 0
    end if
    allocate(currList(natt), _STAT)

    if (natt > 0) then
       ! get the current list
       call ESMF_InfoGet(infoh,key=attrName,values=currList,_RC)
       !ALT delete/destroy this attribute to prevent memory leaks
       call ESMF_InfoRemove(infoh,attrName,_RC)
    end if

    na = natt+1
    allocate(thisList(na), _STAT)

    thisList(1:natt) = currList

    call ESMF_FieldGet(field, name=name, _RC)

    thisList(na) = name

    call ESMF_InfoSet(infoh,key=attrName,values=thisList,_RC)

    deallocate(thisList)
    deallocate(currList)

    _RETURN(ESMF_SUCCESS)
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
    type(ESMF_Info)                         :: infoh


    ! check for attribute

    call ESMF_InfoGetFromHost(Bundle,infoh,_RC)
    call ESMF_InfoGet(infoh,key=attrName,size=natt,_RC)
    allocate(currList(natt), stat=status)

    ! get the current list
    call ESMF_InfoGet(infoh,key=attrName,values=currList,_RC)

    name = currList(fieldIndex)
    call ESMF_FieldBundleGet(Bundle, fieldName = name, field=field, _RC)

    deallocate(currList)

    _RETURN(ESMF_SUCCESS)
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
    type(ESMF_Info) :: infoh

    ! if the grid is present then we can just get the prestored edges and the dimensions of the grid
    ! this also means we are running on a distributed grid
    ! if grid not present then the we just be running outside of ESMF and the user must
    ! pass in the the dimensions of the grid and we must compute them
    ! and assume search on the global domain
    if (present(Grid)) then
       call MAPL_GridGet(grid, localCellCountPerDim=counts,globalCellCountPerDim=dims,_RC)
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

!    call ESMF_InfoGetFromHost(grid,infoh,_RC)
!    call ESMF_InfoGet(infoh, key='GridType', value=grid_type, _RC)
!    if(trim(grid_type) == "Cubed-Sphere") then
    if (im_world*6==jm_world) then

      call MAPL_GetGlobalHorzIJIndex(npts, II, JJ, lon=lon, lat=lat, lonR8=lonR8, latR8=latR8, Grid=Grid, _RC)

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
       _ASSERT(localSearch,"Global Search for IJ for latlon not implemented")
       call ESMF_GridGetCoord(grid,coordDim=1, localDe=0, &
               staggerloc=ESMF_STAGGERLOC_CORNER, fArrayPtr = lons, _RC)
       call ESMF_GridGetCoord(grid,coordDim=2, localDe=0, &
               staggerloc=ESMF_STAGGERLOC_CORNER, fArrayPtr = lats, _RC)
       allocate(elons(im+1),_STAT)
       allocate(elats(jm+1),_STAT)
       call ESMF_GridGet(grid,coordSys=coordSys,_RC)
       elons = lons(:,1)
       elats = lats(1,:)
       if (coordSys==ESMF_COORDSYS_SPH_DEG) then
          elons=elons*MAPL_DEGREES_TO_RADIANS_R8
          elats=elats*MAPL_DEGREES_TO_RADIANS_R8
       else if (coordSys==ESMF_COORDSYS_CART) then
          _FAIL('Unsupported coordinate system:  ESMF_COORDSYS_CART')
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
    _RETURN(ESMF_SUCCESS)

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
      _FAIL("need a cubed-sphere grid")
    endif
    call MAPL_GridGet(grid, globalCellCountPerDim=dims,_RC)
    IM_World = dims(1)
    JM_World = dims(2)
    _ASSERT( IM_WORLD*6 == JM_WORLD, "It only works for cubed-sphere grid")

    allocate(lons(npts),lats(npts))

    call MAPL_Reverse_Schmidt(Grid, stretched, npts, lon=lon, lat=lat, lonR8=lonR8, latR8=latR8, lonRe=lons, latRe=lats, _RC)

    dalpha = 2.0d0*alpha/IM_WORLD

    ! make sure the grid can be used in this subroutine
    good_grid = grid_is_ok(grid)
    if ( .not. good_grid ) then
       _FAIL( "MAPL_GetGlobalHorzIJIndex cannot handle this grid")
    endif
    ! Return if no local points
    _RETURN_IF(npts==0)

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

    _RETURN(_SUCCESS)

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
       real(ESMF_KIND_R8) :: shift0
       real :: tolerance
       integer :: local_dims(3)

       tolerance = epsilon(1.0)
       call MAPL_GridGetInterior(grid,I1,I2,J1,J2)
       call MAPL_GridGet(grid, localCellCountPerDim=local_dims, _RC)
       OK = .true.
       ! check the edge of face 1 along longitude
       !call ESMF_GridGetCoord(grid,localDE=0,coordDim=1,staggerloc=ESMF_STAGGERLOC_CORNER, &
       !     farrayPtr=corner_lons, _RC)
       !call ESMF_GridGetCoord(grid,localDE=0,coordDim=2,staggerloc=ESMF_STAGGERLOC_CORNER, &
       !     farrayPtr=corner_lats, _RC)
       allocate(corner_lons(local_dims(1)+1, local_dims(2)+1))
       allocate(corner_lats(local_dims(1)+1, local_dims(2)+1))
       call MAPL_GridGetCorners(grid, corner_lons, corner_lats, _RC)


       if ( I1 == 1 .and. J1 == 1 ) then
          allocate(lonRe(local_dims(2)),      latRe(local_dims(2)))
          call MAPL_Reverse_Schmidt(grid, stretched, local_dims(2), lonR8=corner_lons(1,1:local_dims(2)), &
                                   latR8=corner_lats(1,1:local_dims(2)), lonRe=lonRe, latRe=latRe, _RC)

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
    type(ESMF_Info)            :: infoh1,infoh2,infoh

    call ESMF_FieldGet(field, name=name, _RC)

    call ESMF_FieldGet(FIELD, dimCount=fieldRank, _RC)

    allocate(localMinIndex(fieldRank),localMaxIndex(fieldRank), _STAT)
    call ESMF_FieldGet(Field, &
         localMinIndex=localMinIndex, localMaxIndex=localMaxIndex, _RC)

    k1 = localMinIndex(fieldRank)
    k2 = localMaxIndex(fieldRank)
    deallocate(localMinIndex,localMaxIndex)

    n = k2 - k1 + 1

    allocate(fields(n), _STAT)

    call genAlias(name, n, splitNameArray, aliasName=aliasName,_RC)

    n = 0
    do k=k1,k2
       n = n+1
       splitName = splitNameArray(n)
       f = ESMF_FieldCreate(field, &
            datacopyflag=ESMF_DATACOPY_REFERENCE, &
            trailingUngridSlice=[k], name=splitName, _RC)

       ! copy attributes and adjust as necessary
       fld = field ! shallow copy to get around intent(in/out)
       call MAPL_FieldCopyAttributes(FIELD_IN=fld, FIELD_OUT=f, _RC)

       ! adjust ungridded dims attribute (if any)
       call ESMF_InfoGetFromHost(FIELD,infoh1,_RC)
       call ESMF_InfoGetFromHost(F,infoh2,_RC)
       has_ungrd = ESMF_InfoIsPresent(infoh1,'UNGRIDDED_DIMS',_RC)
       if (has_ungrd) then
          call ESMF_InfoGet(infoh2,key='UNGRIDDED_DIMS',size=UNGRD_CNT,_RC)
          allocate(ungrd(UNGRD_CNT), _STAT)
          call ESMF_InfoGet(infoh2,key='UNGRIDDED_DIMS',values=UNGRD,_RC)
          call ESMF_InfoRemove(infoh2,'UNGRIDDED_DIMS',_RC)
          if (ungrd_cnt > 1) then
             ungrd_cnt = ungrd_cnt - 1
             call ESMF_InfoSet(infoh2,key='UNGRIDDED_DIMS', &
                  values=UNGRD(1:ungrd_cnt),_RC)
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

    call ESMF_InfoGetFromHost(FIELD,infoh,_RC)
    call ESMF_InfoGet(infoh, 'LONG_NAME', longName, _RC)
    if (index(longName, "%d") /= 0) then
       call expandBinNumber(fields, _RC)
    end if


    _RETURN(ESMF_SUCCESS)

  contains
    subroutine expandBinNumber(fields, rc)
      type(ESMF_Field) :: fields(:)
      integer, optional :: rc

      integer :: i, tlen, i1, i2
      character(len=ESMF_MAXSTR) :: longName
      character(len=3) :: tmp
      character(len=ESMF_MAXSTR) :: newLongName
      type(ESMF_Info) :: infoh

      do i = 1, size(fields)
         call ESMF_InfoGetFromHost(fields(i),infoh,_RC)

         call ESMF_InfoGet(infoh, key='LONG_NAME', value=longName, _RC)
         i1 = index(longName, "%d")
         _ASSERT(i1>0, "Nothing to expand")
         i2 = i1 + 2 ! size of "%d"
         tlen = len_trim(longName)
         _ASSERT(tlen + 1 <= len(longName),'LONG_NAME would exceed MAX length after expansion')
         write(tmp,'(i3.3)') i
         newLongName = longName(1:i1-1)//tmp//trim(longName(i2:tlen))
         ! remove old attribute
         call ESMF_InfoRemove(infoh, 'LONG_NAME', _RC)
         ! save the new one
         call ESMF_InfoSet(infoh, key='LONG_NAME', value=newLongName, _RC)
      end do
    _RETURN(ESMF_SUCCESS)
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

      allocate(splitNameArray(n), _STAT)

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

      _RETURN(ESMF_SUCCESS)
    end subroutine GenAlias
  end subroutine MAPL_FieldSplit


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
     real(ESMF_KIND_R8) :: c2p1, c2m1, half_pi, two_pi, stretch_factor, target_lon, target_lat, target_lon_degrees, target_lat_degrees
     real(ESMF_KIND_R8), dimension(npts) :: x,y,z, Xx, Yy, Zz
     logical, dimension(npts) :: n_s
     type(ESMF_Info) :: infoh

     _RETURN_IF( npts == 0 )

     call ESMF_InfoGetFromHost(grid, infoh, _RC)
     factorPresent = ESMF_InfoIsPresent(infoh, 'STRETCH_FACTOR', _RC)
     lonPresent    = ESMF_InfoIsPresent(infoh, 'TARGET_LON',     _RC)
     latPresent    = ESMF_InfoIsPresent(infoh, 'TARGET_LAT',     _RC)

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
           _FAIL("Need input to get the output lonRe, latRe")
        endif
     else
        _RETURN(_SUCCESS)
     endif

     if (.not. stretched) then
        _RETURN(_SUCCESS)
     endif

     call ESMF_InfoGet(infoh, 'STRETCH_FACTOR', value=stretch_factor, _RC)
     call ESMF_InfoGet(infoh, 'TARGET_LON',     value=target_lon_degrees,     _RC)
     call ESMF_InfoGet(infoh, 'TARGET_LAT',     value=target_lat_degrees,     _RC)

     c2p1 = 1 + stretch_factor*stretch_factor
     c2m1 = 1 - stretch_factor*stretch_factor

     half_pi = MAPL_PI_R8/2
     two_pi  = MAPL_PI_R8*2

     target_lon = target_lon_degrees*MAPL_DEGREES_TO_RADIANS_R8
     target_lat = target_lat_degrees*MAPL_DEGREES_TO_RADIANS_R8

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

      _RETURN(_SUCCESS)
  end subroutine

end submodule Base_Implementation
