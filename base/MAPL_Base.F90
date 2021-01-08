
#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module MAPL_BaseMod

!BOP
!
! !MODULE: MAPL_BaseMod --- A Collection of Assorted MAPL Utilities

! !USES:
!
use ESMF
use MAPL_ConstantsMod, only: MAPL_PI, MAPL_PI_R8
use MAPL_RangeMod
use, intrinsic :: iso_fortran_env, only: REAL32
use, intrinsic :: iso_fortran_env, only: REAL64
use MAPL_ExceptionHandling
implicit NONE
private

! !PUBLIC MEMBER FUNCTIONS:
!
public MAPL_AllocateCoupling    ! Atanas: please provide 1-line for each
public MAPL_FieldAllocCommit
!public MAPL_FieldF90Deallocate
public MAPL_ClimInterpFac
!public MAPL_ConnectCoupling
public MAPL_DecomposeDim
public MAPL_MakeDecomposition
public MAPL_FieldCreate
public MAPL_FieldCreateEmpty
public MAPL_FieldGetTime
public MAPL_FieldSetTime
public MAPL_GridGet
public MAPL_GRID_INTERIOR
public MAPL_GridHasDE
public MAPL_IncYMD
public MAPL_Interp_Fac
public MAPL_LatLonGridCreate   ! Creates regular Lat/Lon ESMF Grids
public MAPL_Nhmsf
public MAPL_NSECF
public MAPL_Nsecf2
public MAPL_PackTime
public MAPL_PackDateTime
public MAPL_RemapBounds
public MAPL_Tick
public MAPL_TimeStringGet
public MAPL_UnpackTime
public MAPL_UnpackDateTime
public MAPL_RmQualifier
public MAPL_GetImsJms
public MAPL_AttributeSet
public MAPL_SetPointer
public MAPL_FieldCopyAttributes
public MAPL_StateAdd
public MAPL_FieldBundleAdd
public MAPL_FieldBundleGet
public MAPL_FieldDestroy
public MAPL_FieldBundleDestroy
public MAPL_GetHorzIJIndex
public MAPL_GenGridName
public MAPL_GenXYOffset
public MAPL_GeosNameNew
public MAPL_Communicator
public MAPL_Communicators
public MAPL_BundleCreate
public MAPL_FieldCopy
public MAPL_Leap
public MAPL_Range
public MAPL_DistGridGet
public MAPL_GridGetCorners
public MAPL_GridGetInterior
public MAPL_TrimString
public MAPL_FieldSplit

! !PUBLIC PARAMETERS
!
integer, public, parameter :: MAPL_CplUNKNOWN        = 0
integer, public, parameter :: MAPL_CplSATISFIED      = 1
integer, public, parameter :: MAPL_CplNEEDED         = 2
integer, public, parameter :: MAPL_CplNOTNEEDED      = 4
integer, public, parameter :: MAPL_FriendlyVariable  = 8
integer, public, parameter :: MAPL_FieldItem         = 8
integer, public, parameter :: MAPL_BundleItem        = 16
integer, public, parameter :: MAPL_StateItem         = 32
integer, public, parameter :: MAPL_NoRestart         = 64

integer, public, parameter :: MAPL_Write2Disk        = 0
integer, public, parameter :: MAPL_Write2RAM         = 1

integer, public, parameter :: MAPL_VLocationNone   = 0
integer, public, parameter :: MAPL_VLocationEdge   = 1
integer, public, parameter :: MAPL_VLocationCenter = 2

integer, public, parameter :: MAPL_DimsUnknown     = 0
integer, public, parameter :: MAPL_DimsVertOnly    = 1
integer, public, parameter :: MAPL_DimsHorzOnly    = 2
integer, public, parameter :: MAPL_DimsHorzVert    = 3
integer, public, parameter :: MAPL_DimsTileOnly    = 4
integer, public, parameter :: MAPL_DimsTileTile    = 5
integer, public, parameter :: MAPL_DimsNone        = 6

integer, public, parameter :: MAPL_ScalarField     = 1
integer, public, parameter :: MAPL_VectorField     = 2


integer, public, parameter :: MAPL_CplAverage      = 0
integer, public, parameter :: MAPL_CplMin          = 1
integer, public, parameter :: MAPL_CplMax          = 2
integer, public, parameter :: MAPL_CplAccumulate   = 3
integer, public, parameter :: MAPL_MinMaxUnknown   = MAPL_CplAverage

integer, public, parameter :: MAPL_AttrGrid        = 1
integer, public, parameter :: MAPL_AttrTile        = 2

integer, public, parameter :: MAPL_UnInitialized  = 0
integer, public, parameter :: MAPL_InitialDefault  = 1
integer, public, parameter :: MAPL_InitialRestart  = 2

integer, public, parameter :: MAPL_DuplicateEntry  = -99
integer, public, parameter :: MAPL_Self = 0 
integer, public, parameter :: MAPL_Import = 1
integer, public, parameter :: MAPL_Export = 2
integer, public, parameter :: MAPL_ConnUnknown = -1
integer, public, parameter :: MAPL_FirstPhase   = 1
integer, public, parameter :: MAPL_SecondPhase  = MAPL_FirstPhase+1
integer, public, parameter :: MAPL_ThirdPhase   = MAPL_FirstPhase+2
integer, public, parameter :: MAPL_FourthPhase  = MAPL_FirstPhase+3
integer, public, parameter :: MAPL_FifthPhase   = MAPL_FirstPhase+4

real,    public, parameter :: MAPL_UNDEF              = 1.0e15  

integer, public, parameter :: MAPL_Ocean              = 0
integer, public, parameter :: MAPL_Lake               = 19
integer, public, parameter :: MAPL_LandIce            = 20
integer, public, parameter :: MAPL_Land               = 100
integer, public, parameter :: MAPL_Vegetated          = 101

integer, public, parameter :: MAPL_NumVegTypes        = 6

integer, public, parameter :: MAPL_AGrid = 0
integer, public, parameter :: MAPL_CGrid = 1
integer, public, parameter :: MAPL_DGrid = 2

integer, public, parameter :: MAPL_RotateLL = 0
integer, public, parameter :: MAPL_RotateCube = 1


integer, public, parameter :: MAPL_HorzTransOrderBinning  = 0
integer, public, parameter :: MAPL_HorzTransOrderBilinear = 1
integer, public, parameter :: MAPL_HorzTransOrderFraction = 98
integer, public, parameter :: MAPL_HorzTransOrderSample   = 99

integer, public, parameter :: MAPL_RestartOptional = 0
integer, public, parameter :: MAPL_RestartSkip = 1
integer, public, parameter :: MAPL_RestartRequired = 2
integer, public, parameter :: MAPL_RestartBootstrap = 3
integer, public, parameter :: MAPL_RestartSkipInitial = 4

integer, public, parameter :: MAPL_TileNameLength = 128

character(len=ESMF_MAXSTR), public, parameter :: MAPL_StateItemOrderList = 'MAPL_StateItemOrderList'
character(len=ESMF_MAXSTR), public, parameter :: MAPL_BundleItemOrderList = 'MAPL_BundleItemOrderList'

type MAPL_Communicator
   integer :: comm
   integer :: rank
   integer :: root
   integer :: size
   !logical :: am_i_root
end type MAPL_Communicator

type MAPL_Communicators
   type(MAPL_Communicator) :: esmf
   type(MAPL_Communicator) :: io
   type(MAPL_Communicator) :: global
   type(MAPL_Communicator) :: mapl

   integer :: maxMem
end type MAPL_Communicators

#ifdef __PROTEX__

 !DESCRIPTION:

   The module {\tt MAPL\_Base} provides a collection assorted
utilities and constants used throughout the MAPL Library.


#endif

!EOP
!----------------------------------------------------------------------

interface MAPL_FieldCreate
   module procedure MAPL_FieldCreateRename
   module procedure MAPL_FieldCreateNewgrid
   module procedure MAPL_FieldCreateR4
end interface

interface MAPL_FieldGetTime
   module procedure MAPL_GetFieldTimeFromField
   module procedure MAPL_GetFieldTimeFromState
end interface

interface MAPL_FieldSetTime
   module procedure MAPL_SetFieldTimeFromField
   module procedure MAPL_SetFieldTimeFromState
end interface

interface MAPL_RemapBounds
   module procedure MAPL_RemapBoundsFull_3dr4
   module procedure MAPL_RemapBounds_3dr4
   module procedure MAPL_RemapBounds_3dr8
end interface

interface MAPL_AttributeSet
   module procedure MAPL_StateAttSetI4
   module procedure MAPL_BundleAttSetI4
   module procedure MAPL_FieldAttSetI4
end interface

interface MAPL_SetPointer
  module procedure MAPL_SetPointer2DR4
  module procedure MAPL_SetPointer3DR4
end interface

interface MAPL_StateAdd
  module procedure MAPL_StateAddField
  module procedure MAPL_StateAddBundle
end interface

interface MAPL_FieldBundleAdd
  module procedure MAPL_FieldBundleAddField
end interface

interface MAPL_FieldBundleGet
  module procedure MAPL_FieldBundleGetByIndex
end interface

interface MAPL_GetHorzIJIndex
  module procedure MAPL_GetHorzIJIndex
end interface

#define R8  8

contains

  subroutine MAPL_AllocateCoupling(field, rc)

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

    call ESMF_FieldGet(field, status=fieldStatus, rc=status)
    _VERIFY(STATUS)

    if (fieldStatus /= ESMF_FIELDSTATUS_COMPLETE) then

!ALT: if the attributeGet calls fail, this would very likely indicate
!     that the field was NOT created by MAPL (or something terrible happened)
!     For now we just abort
       call ESMF_AttributeGet(FIELD, NAME='DIMS', VALUE=DIMS, RC=STATUS)
       _VERIFY(STATUS)
       call ESMF_AttributeGet(FIELD, NAME='VLOCATION', VALUE=LOCATION, RC=STATUS)
       _VERIFY(STATUS)
       call ESMF_AttributeGet(FIELD, NAME='HALOWIDTH', VALUE=HW, RC=STATUS)
       _VERIFY(STATUS)
       call ESMF_AttributeGet(FIELD, NAME='PRECISION', VALUE=KND, RC=STATUS)
       _VERIFY(STATUS)
       call ESMF_AttributeGet(FIELD, NAME='DEFAULT_PROVIDED', value=defaultProvided, RC=STATUS)
       _VERIFY(STATUS)
       if(defaultProvided) then
          call ESMF_AttributeGet(FIELD, NAME='DEFAULT_VALUE', value=default_value, RC=STATUS)
          _VERIFY(STATUS)
       end if

       call ESMF_AttributeGet(FIELD, NAME="UNGRIDDED_DIMS", isPresent=has_ungrd, RC=STATUS)
       _VERIFY(STATUS)
       if (has_ungrd) then
          call ESMF_AttributeGet(FIELD, NAME='UNGRIDDED_DIMS', itemcount=UNGRD_CNT, RC=STATUS)
          _VERIFY(STATUS)
          allocate(ungrd(UNGRD_CNT), stat=status)
          _VERIFY(STATUS)
          call ESMF_AttributeGet(FIELD, NAME='UNGRIDDED_DIMS', valueList=UNGRD, RC=STATUS)
          _VERIFY(STATUS)
          if (defaultProvided) then
             call MAPL_FieldAllocCommit(field, dims=dims, location=location, typekind=knd, &
                  hw=hw, ungrid=ungrd, default_value=default_value, rc=status)
             _VERIFY(STATUS)
          else
             call MAPL_FieldAllocCommit(field, dims=dims, location=location, typekind=knd, &
                  hw=hw, ungrid=ungrd, rc=status)
             _VERIFY(STATUS)
          end if
       else
          if (defaultProvided) then
             call MAPL_FieldAllocCommit(field, dims=dims, location=location, typekind=knd, &
                  hw=hw, default_value=default_value, rc=status)
             _VERIFY(STATUS)
          else
             call MAPL_FieldAllocCommit(field, dims=dims, location=location, typekind=knd, &
                  hw=hw, rc=status)
             _VERIFY(STATUS)
          end if
       end if

       if (has_ungrd) then
          deallocate(ungrd)
       end if

    end if

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_AllocateCoupling

  subroutine MAPL_FieldAllocCommit(field, dims, location, typekind, &
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

    call ESMF_FieldGet(field, grid=GRID, RC=STATUS)
    _VERIFY(STATUS)
    call MAPL_GridGet(GRID, localCellCountPerDim=COUNTS, RC=STATUS)
    _VERIFY(STATUS)
    call ESMF_GridGet(GRID, dimCount=gridRank, rc=status)
    _VERIFY(STATUS)
    _ASSERT(gridRank <= 3,' MAPL restriction - only 2 and 3d are supported')
    allocate(gridToFieldMap(gridRank), stat=status)
    _VERIFY(STATUS)
    gridToFieldMap = 0
    do I = 1, gridRank
       gridToFieldMap(I) = I
    end do
! ALT: the next allocation should have been griddedDims,
!      but this compilcates the code unnecessery
    allocate(haloWidth(gridRank), stat=status) 
    _VERIFY(STATUS)
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
             allocate(VAR_1D(UNGRID(1)), STAT=STATUS)
             _VERIFY(STATUS)
             VAR_1D = INIT_VALUE
             call ESMF_FieldEmptyComplete(FIELD, farrayPtr=VAR_1D,    &
                  datacopyFlag = ESMF_DATACOPY_REFERENCE,             &
                  gridToFieldMap=gridToFieldMap,                      &
                  rc = status)
          case default
             _ASSERT(.false., 'unsupported rank > 1')
          end select
          
       else
          select case (rank)
          case (1)
             allocate(VR8_1D(UNGRID(1)), STAT=STATUS)
             _VERIFY(STATUS)
             VR8_1D = INIT_VALUE
             call ESMF_FieldEmptyComplete(FIELD, farrayPtr=VR8_1D,    &
                  datacopyFlag = ESMF_DATACOPY_REFERENCE,             &
                  gridToFieldMap=gridToFieldMap,                      &
                  rc = status)
          case default
             _ASSERT(.false., 'unsupported rank > 1')
          end select
          
       endif
       _VERIFY(STATUS)
       
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
          allocate(VAR_1D(lb1:ub1), STAT=STATUS)
          _VERIFY(STATUS)
          VAR_1D = INIT_VALUE
             
          call ESMF_FieldEmptyComplete(FIELD, farrayPtr=var_1d,  &
               gridToFieldMap=gridToFieldMap,                           &
               RC=status)
          _VERIFY(STATUS)
       else
          allocate(VR8_1D(lb1:ub1), STAT=STATUS)
          _VERIFY(STATUS)
          VR8_1D = INIT_VALUE
             
          call ESMF_FieldEmptyComplete(FIELD, farrayPtr=vr8_1d,  &
               gridToFieldMap=gridToFieldMap,                           &
               RC=status)
          _VERIFY(STATUS)
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
          allocate(VAR_2D(lb1:ub1, lb2:ub2), STAT=STATUS)
          _VERIFY(STATUS)
          VAR_2D = INIT_VALUE
          call ESMF_FieldEmptyComplete(FIELD, farrayPtr=VAR_2D, &
               datacopyFlag = ESMF_DATACOPY_REFERENCE,         &
               gridToFieldMap=gridToFieldMap,              &
               totalLWidth=haloWidth(1:griddedDims),     &
               totalUWidth=haloWidth(1:griddedDims),     &
               rc = status)
          case (3)
             allocate(VAR_3D(lb1:ub1, lb2:ub2, UNGRID(1)), STAT=STATUS)
             _VERIFY(STATUS)
             VAR_3D = INIT_VALUE
             call ESMF_FieldEmptyComplete(FIELD, farrayPtr=VAR_3D, &
                  datacopyFlag = ESMF_DATACOPY_REFERENCE,         &
                  gridToFieldMap=gridToFieldMap,              &
                  totalLWidth=haloWidth(1:griddedDims),     &
                  totalUWidth=haloWidth(1:griddedDims),     &
                  rc = status)
          case (4)
             allocate(VAR_4D(lb1:ub1, lb2:ub2, UNGRID(1), UNGRID(2)), STAT=STATUS)
             _VERIFY(STATUS)
             VAR_4D = INIT_VALUE
             call ESMF_FieldEmptyComplete(FIELD, farrayPtr=VAR_4D, &
                  datacopyFlag = ESMF_DATACOPY_REFERENCE,         &
                  gridToFieldMap=gridToFieldMap,              &
                  totalLWidth=haloWidth(1:griddedDims),     &
                  totalUWidth=haloWidth(1:griddedDims),     &
               rc = status)
          case default
             _ASSERT(.false., 'only up to 4D are supported')
          end select RankCase2d
       else
          select case (rank)
          case (2)
          allocate(VR8_2D(lb1:ub1, lb2:ub2), STAT=STATUS)
          _VERIFY(STATUS)
          VR8_2D = INIT_VALUE
          call ESMF_FieldEmptyComplete(FIELD, farrayPtr=VR8_2D, &
               datacopyFlag = ESMF_DATACOPY_REFERENCE,          &
               gridToFieldMap=gridToFieldMap,                   &
               totalLWidth=haloWidth(1:griddedDims),            &
               totalUWidth=haloWidth(1:griddedDims),            &
               rc = status)
          case (3)
          allocate(VR8_3D(lb1:ub1, lb2:ub2, UNGRID(1)), STAT=STATUS)
          _VERIFY(STATUS)
          VR8_3D = INIT_VALUE
          call ESMF_FieldEmptyComplete(FIELD, farrayPtr=VR8_3D, &
               datacopyFlag = ESMF_DATACOPY_REFERENCE,          &
               gridToFieldMap=gridToFieldMap,                   &
               totalLWidth=haloWidth(1:griddedDims),            &
               totalUWidth=haloWidth(1:griddedDims),            &
               rc = status)
          case (4)
          allocate(VR8_4D(lb1:ub1, lb2:ub2, UNGRID(1), UNGRID(2)), STAT=STATUS)
          _VERIFY(STATUS)
          VR8_4D = INIT_VALUE
          call ESMF_FieldEmptyComplete(FIELD, farrayPtr=VR8_4D, &
               datacopyFlag = ESMF_DATACOPY_REFERENCE,         &
               gridToFieldMap=gridToFieldMap,              &
               totalLWidth=haloWidth(1:griddedDims),     &
               totalUWidth=haloWidth(1:griddedDims),     &
               rc = status)
          case default
             _ASSERT(.false., 'only up to 4D are supported')
          end select
       end if
       _VERIFY(STATUS)

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
             NULLIFY(VAR_3D)
             allocate(VAR_3D(lb1:ub1, lb2:ub2, lb3:ub3), STAT=status)
             _VERIFY(STATUS)
             VAR_3D = INIT_VALUE
             call ESMF_FieldEmptyComplete(FIELD, farrayPtr=VAR_3D, &
                  datacopyFlag = ESMF_DATACOPY_REFERENCE,         &
                  gridToFieldMap=gridToFieldMap,              &
                  totalLWidth=haloWidth(1:griddedDims),     &
                  totalUWidth=haloWidth(1:griddedDims),     &
                  rc = status)
          else
             NULLIFY(VR8_3D)
             allocate(VR8_3D(lb1:ub1, lb2:ub2, lb3:ub3), STAT=status)
             _VERIFY(STATUS)
             VR8_3D = INIT_VALUE
             call ESMF_FieldEmptyComplete(FIELD, farrayPtr=VR8_3D, &
                  datacopyFlag = ESMF_DATACOPY_REFERENCE,         &
                  gridToFieldMap=gridToFieldMap,              &
                  totalLWidth=haloWidth(1:griddedDims),     &
                  totalUWidth=haloWidth(1:griddedDims),     &
                  rc = status)
          endif
          _VERIFY(STATUS)

       case (4)
          if (typekind == ESMF_KIND_R4) then
             NULLIFY(VAR_4D)
             allocate(VAR_4D(lb1:ub1, lb2:ub2, lb3:ub3, ungrid(1)), STAT=status)
             _VERIFY(STATUS)
             VAR_4D = INIT_VALUE
             call ESMF_FieldEmptyComplete(FIELD, farrayPtr=VAR_4D, &
                  datacopyFlag = ESMF_DATACOPY_REFERENCE,         &
                  gridToFieldMap=gridToFieldMap,              &
                  totalLWidth=haloWidth(1:griddedDims),     &
                  totalUWidth=haloWidth(1:griddedDims),     &
                  rc = status)
          else
             NULLIFY(VR8_4D)
             allocate(VR8_4D(lb1:ub1, lb2:ub2, lb3:ub3, ungrid(1)), STAT=status)
             _VERIFY(STATUS)
             VR8_4D = INIT_VALUE
             call ESMF_FieldEmptyComplete(FIELD, farrayPtr=VR8_4D, &
                  datacopyFlag = ESMF_DATACOPY_REFERENCE,         &
                  gridToFieldMap=gridToFieldMap,              &
                  totalLWidth=haloWidth(1:griddedDims),     &
                  totalUWidth=haloWidth(1:griddedDims),     &
                  rc = status)
          endif
          _VERIFY(STATUS)

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
             allocate(VAR_1D(COUNTS(1)), STAT=STATUS)
             _VERIFY(STATUS)
             VAR_1D = INIT_VALUE
             call ESMF_FieldEmptyComplete(FIELD, farrayPtr=VAR_1D,    &
                  datacopyFlag = ESMF_DATACOPY_REFERENCE,         &
                  rc = status)
          case (2)
             allocate(VAR_2D(COUNTS(1),UNGRID(1)), STAT=STATUS)
             _VERIFY(STATUS)
             VAR_2D = INIT_VALUE
             call ESMF_FieldEmptyComplete(FIELD, farrayPtr=VAR_2D,    &
                  gridToFieldMap=gridToFieldMap,                      &
                  datacopyFlag = ESMF_DATACOPY_REFERENCE,             &
                  rc = status)
          case (3)
             allocate(VAR_3D(COUNTS(1), UNGRID(1), UNGRID(2)), &
                  STAT=STATUS)
             _VERIFY(STATUS)
             VAR_3D = INIT_VALUE
             call ESMF_FieldEmptyComplete(FIELD, farrayPtr=VAR_3D,    &
                  gridToFieldMap=gridToFieldMap,                      &
                  datacopyFlag = ESMF_DATACOPY_REFERENCE,             &
                  rc = status)
          case default
             _ASSERT(.false., 'only 2D and 3D are supported')
          end select
          
       else
          select case (rank)
          case (1)
             allocate(VR8_1D(COUNTS(1)), STAT=STATUS)
             _VERIFY(STATUS)
             VR8_1D = INIT_VALUE
             call ESMF_FieldEmptyComplete(FIELD, farrayPtr=VR8_1D,    &
                  datacopyFlag = ESMF_DATACOPY_REFERENCE,             &
                  rc = status)
          case (2)
             allocate(VR8_2D(COUNTS(1),UNGRID(1)), STAT=STATUS)
             _VERIFY(STATUS)
             VR8_2D = INIT_VALUE
             call ESMF_FieldEmptyComplete(FIELD, farrayPtr=VR8_2D,    &
                  gridToFieldMap=gridToFieldMap,                      &
                  datacopyFlag = ESMF_DATACOPY_REFERENCE,             &
                  rc = status)
          case (3)
             allocate(VR8_3D(COUNTS(1), UNGRID(1), UNGRID(2)), &
                  STAT=STATUS)
             _VERIFY(STATUS)
             VR8_3D = INIT_VALUE
             call ESMF_FieldEmptyComplete(FIELD, farrayPtr=VR8_3D,    &
                  gridToFieldMap=gridToFieldMap,                      &
                  datacopyFlag = ESMF_DATACOPY_REFERENCE,             &
                  rc = status)
          case default
             _ASSERT(.false., 'only 2D and 3D are supported')
          end select
          
       endif
       _VERIFY(STATUS)
       
    case(MAPL_DimsTileTile)
       rank=2
       _ASSERT(gridRank == 1, 'gridRank /= 1')
       
       if (typekind == ESMF_KIND_R4) then
          allocate(VAR_2D(COUNTS(1), COUNTS(2)), STAT=STATUS)
          _VERIFY(STATUS)
          VAR_2D = INIT_VALUE
          call ESMF_FieldEmptyComplete(FIELD, farrayPtr=VAR_2D,    &
               datacopyFlag = ESMF_DATACOPY_REFERENCE,             &
               !                  ungriddedLBound = (/1/),                      &
               !                  ungriddedUBound = (/counts(2)/),              &
               rc = status)
       else
          allocate(VR8_2D(COUNTS(1), COUNTS(2)), STAT=STATUS)
          _VERIFY(STATUS)
          VR8_2D = INIT_VALUE
          call ESMF_FieldEmptyComplete(FIELD, farrayPtr=VR8_2D,    &
               datacopyFlag = ESMF_DATACOPY_REFERENCE,             &
               !                  ungriddedLBound = (/1/),                      &
               !                  ungriddedUBound = (/counts(2)/),              &
               rc = status)
       endif
       _VERIFY(STATUS)
            
! Invalid dimensionality
! ----------------------

    case default 
       _RETURN(ESMF_FAILURE)
       
    end select Dimensionality
    _VERIFY(STATUS)
         
    if (present(default_value)) then
       call MAPL_AttributeSet(field, NAME="MAPL_InitStatus", &
                              VALUE=MAPL_InitialDefault, RC=STATUS)
       _VERIFY(STATUS)      
    end if

! Clean up
    deallocate(haloWidth)
    deallocate(gridToFieldMap)


    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_FieldAllocCommit

  subroutine MAPL_FieldF90Deallocate(field, rc)
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

    call ESMF_FieldGet(field, status=fieldStatus, rc=status)
    _VERIFY(STATUS)

    if (fieldStatus == ESMF_FIELDSTATUS_COMPLETE) then
       call ESMF_FieldGet(field, Array=array, rc=status)
       _VERIFY(STATUS)

       call ESMF_ArrayGet(array, localDeCount=localDeCount, rc=status)
       _VERIFY(STATUS)
       _ASSERT(localDeCount == 1, 'currently MAPL supports only 1 local array')
       call ESMF_ArrayGet(array, localarrayList=larrayList, rc=status)
       _VERIFY(STATUS)
       larray => lArrayList(1) ! alias

       call ESMF_LocalArrayGet(larray, rank=rank, typekind=tk, &
            rc=status)
       _VERIFY(STATUS)

       call ESMF_LocalArrayF90Deallocate(larray, typekind=tk, rank=rank, rc=status)
       _VERIFY(STATUS)
    end if

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_FieldF90Deallocate

  subroutine MAPL_SetPointer2DR4(state, ptr, name, rc)
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

    _ASSERT(associated(ptr), 'unassociated pointer')

! Get Field from state

    loc = index(name,';;')

    if(loc/=0) then
       call ESMF_StateGet(state, name(:loc-1), Bundle, rc=status)
       _VERIFY(STATUS)
       call ESMF_StateGet(state, name(loc+2:), Field, rc=status)
       _VERIFY(STATUS)
    else
       call ESMF_StateGet(state, name, Field, rc=status)
       _VERIFY(STATUS)
    end if

    call ESMF_FieldGet(field, status=fieldStatus, rc=status)
    _VERIFY(STATUS)
    _ASSERT(fieldStatus /= ESMF_FIELDSTATUS_COMPLETE, 'fieldStatus == ESMF_FIELDSTATUS_COMPLETE')

    call ESMF_FieldGet(field, grid=GRID, RC=STATUS)
    _VERIFY(STATUS)
    call MAPL_GridGet(GRID, localCellCountPerDim=COUNTS, RC=STATUS)
    _VERIFY(STATUS)

    _ASSERT(size(ptr,1) == COUNTS(1), 'shape mismatch dim=1')
    _ASSERT(size(ptr,2) == COUNTS(2), 'shape mismatch dim=2')
    call ESMF_GridGet(GRID, dimCount=gridRank, rc=status)
    _VERIFY(STATUS)
    ! MAPL restriction (actually only the first 2 dims are distributted) 
    _ASSERT(gridRank <= 3, 'gridRank > 3 not supported')
    allocate(gridToFieldMap(gridRank), stat=status)
    _VERIFY(STATUS)
    do I = 1, gridRank
       gridToFieldMap(I) = I
    end do

! this is 2d case
    if (gridRank == 3) gridToFieldMap(3) = 0

    call ESMF_FieldEmptyComplete(FIELD, farrayPtr=ptr, &
         datacopyFlag = ESMF_DATACOPY_REFERENCE,       &
         gridToFieldMap=gridToFieldMap,                &
         rc = status)
    _VERIFY(STATUS)

! Clean up
    deallocate(gridToFieldMap)


    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_SetPointer2DR4

  subroutine MAPL_SetPointer3DR4(state, ptr, name, rc)
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

    _ASSERT(associated(ptr), 'unassociated pointer')

! Get Field from state

    loc = index(name,';;')

    if(loc/=0) then
       call ESMF_StateGet(state, name(:loc-1), Bundle, rc=status)
       _VERIFY(STATUS)
       call ESMF_StateGet(state, name(loc+2:), Field, rc=status)
       _VERIFY(STATUS)
    else
       call ESMF_StateGet(state, name, Field, rc=status)
       _VERIFY(STATUS)
    end if

    call ESMF_FieldGet(field, status=fieldStatus, rc=status)
    _VERIFY(STATUS)
    _ASSERT(fieldStatus /= ESMF_FIELDSTATUS_COMPLETE, 'fieldStatus == ESMF_FIELDSTATUS_COMPLETE')

    call ESMF_FieldGet(field, grid=GRID, RC=STATUS)
    _VERIFY(STATUS)
    call MAPL_GridGet(GRID, localCellCountPerDim=COUNTS, RC=STATUS)
    _VERIFY(STATUS)

    _ASSERT(size(ptr,1) == COUNTS(1), 'shape mismatch dim=1')
    _ASSERT(size(ptr,2) == COUNTS(2), 'shape mismatch dim=2')
    call ESMF_GridGet(GRID, dimCount=gridRank, rc=status)
    _VERIFY(STATUS)
    ! MAPL restriction (actually only the first 2 dims are distributted) 
    _ASSERT(gridRank <= 3, 'gridRank > 3 not supported') 
    allocate(gridToFieldMap(gridRank), stat=status)
    _VERIFY(STATUS)
    do I = 1, gridRank
       gridToFieldMap(I) = I
    end do

    call ESMF_FieldEmptyComplete(FIELD, farrayPtr=ptr, &
         datacopyFlag = ESMF_DATACOPY_REFERENCE,       &
         gridToFieldMap=gridToFieldMap,                &
         rc = status)
    _VERIFY(STATUS)

! Clean up
    deallocate(gridToFieldMap)


    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_SetPointer3DR4

  subroutine MAPL_DecomposeDim ( dim_world,dim,NDEs, unusable, symmetric, min_DE_extent )
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

      _UNUSED_DUMMY(unusable)

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
            !                 ie = is + CEILING( float(ieg-is+1)/(ndivs-ndiv) ) - 1

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

   subroutine MAPL_MakeDecomposition(nx, ny, unusable, reduceFactor, rc)
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

      _UNUSED_DUMMY(unusable)

      call ESMF_VMGetCurrent(vm, rc=status)
      _VERIFY(status)
      call ESMF_VMGet(vm, petCount=pet_count, rc=status)
      _VERIFY(status)
      if (present(reduceFactor)) pet_count=pet_count/reduceFactor

      ! count down from sqrt(n)
      ! Note: inal iteration (nx=1) is guaranteed to succeed.
      do nx = floor(sqrt(real(2*pet_count))), 1, -1
         if (mod(pet_count, nx) == 0) then ! found a decomposition
            ny = pet_count / nx
            exit
         end if
      end do

      _RETURN(ESMF_SUCCESS)

   end subroutine MAPL_MakeDecomposition

  subroutine MAPL_Interp_Fac (TIME0, TIME1, TIME2, FAC1, FAC2, RC)

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

  subroutine MAPL_ClimInterpFac (CLOCK,I1,I2,FAC, RC)

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

    call ESMF_ClockGet       ( CLOCK,    CurrTime=CurrTime, calendar=cal, rc=STATUS )
    _VERIFY(STATUS)
    call ESMF_TimeGet        ( CurrTime, midMonth=midMonth,               rc=STATUS )
    _VERIFY(STATUS)
    call ESMF_TimeIntervalSet( oneMonth, MM = 1, calendar=cal,            rc=status )
    _VERIFY(STATUS)

    if( CURRTIME < midMonth ) then
       AFTER    = midMonth
       midMonth = midMonth - oneMonth
       call ESMF_TimeGet (midMonth, midMonth=BEFORE, rc=STATUS )
       _VERIFY(STATUS)
    else
       BEFORE   = midMonth
       midMonth = midMonth + oneMonth
       call ESMF_TimeGet (midMonth, midMonth=AFTER , rc=STATUS )
       _VERIFY(STATUS)
    endif

    call MAPL_Interp_Fac( CURRTIME, BEFORE, AFTER, FAC, RC=STATUS)
    _VERIFY(STATUS)

    call ESMF_TimeGet (BEFORE, MM=I1, rc=STATUS )
    _VERIFY(STATUS)
    call ESMF_TimeGet (AFTER , MM=I2, rc=STATUS )
    _VERIFY(STATUS)
 

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_ClimInterpFac


subroutine MAPL_TimeStringGet(TIMESTRING,YY,MM,DD,H,M,S)
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


subroutine MAPL_UnpackTime(TIME,IYY,IMM,IDD)
  integer, intent (IN ) :: TIME
  integer, intent (OUT) :: IYY
  integer, intent (OUT) :: IMM
  integer, intent (OUT) :: IDD
  IYY = TIME/10000
  IMM = mod(TIME/100,100)
  IDD = mod(TIME,100)
end subroutine MAPL_UnpackTime


subroutine MAPL_PackTime(TIME,IYY,IMM,IDD)
  integer, intent (OUT) :: TIME
  integer, intent (IN ) :: IYY
  integer, intent (IN ) :: IMM
  integer, intent (IN ) :: IDD
  TIME=IYY*10000+IMM*100+IDD
end subroutine MAPL_PackTime


subroutine MAPL_PackDateTime(date_time, yy, mm, dd, h, m, s)
  integer, intent(in) :: yy, mm, dd, h, m, s
  integer, intent(out) :: date_time(:)

  date_time(1) = (10000 * yy) + (100 * mm) + dd
  date_time(2) = (10000 * h) + (100 * m) + s
end subroutine MAPL_PackDateTime


subroutine MAPL_UnpackDateTime(date_time, yy, mm, dd, h, m, s)
  integer, intent(in) :: date_time(:)
  integer, intent(out) :: yy, mm, dd, h, m, s

  yy =     date_time(1) / 10000
  mm = mod(date_time(1), 10000) / 100
  dd = mod(date_time(1), 100)
  h  =     date_time(2) / 10000
  m  = mod(date_time(2), 10000) / 100
  s  = mod(date_time(2), 100)
end subroutine MAPL_UnpackDateTime


integer function MAPL_nsecf(nhms)
   integer, intent(in) :: nhms
   MAPL_nsecf = nhms/10000*3600 + mod(nhms,10000)/100*60 + mod(nhms,100)
end function MAPL_nsecf
   
subroutine MAPL_tick (nymd,nhms,ndt)
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

integer function MAPL_nsecf2 (nhhmmss,nmmdd,nymd)
      integer nhhmmss,nmmdd,nymd,nday,month
      integer nsday, ncycle,iday,iday2
      integer i,nsegm,nsegd
      PARAMETER ( NSDAY  = 86400 )
      PARAMETER ( NCYCLE = 1461*24*3600 )
      INTEGER YEAR, DAY, SEC
      integer    MNDY(12,4), mnd48(48)
      DATA MND48/0,31,60,91,121,152,182,213,244,274,305,335,366,397,34*0 /
!     DATA MNDY /0,31,60,91,121,152,182,213,244,274,305,335,366,397,34*0 /
      equivalence ( mndy(1,1), mnd48(1) )
      MAPL_nsecf2 = MAPL_nsecf( nhhmmss )
      if( nmmdd.eq.0 ) return
      DO 100 I=15,48
!     MNDY(I,1) = MNDY(I-12,1) + 365
      MND48(I) = MND48(I-12) + 365
100   CONTINUE
      nsegm =     nmmdd/100
      nsegd = mod(nmmdd,100)
      YEAR   = NYMD / 10000
      MONTH  = MOD(NYMD,10000) / 100
      DAY    = MOD(NYMD,100)
      SEC    = MAPL_NSECF(nhhmmss)
      IDAY   = MNDY( MONTH ,MOD(YEAR ,4)+1 )
      month = month + nsegm
      If( month.gt.12 ) then
      month = month - 12
      year = year + 1
      endif
      IDAY2  = MNDY( MONTH ,MOD(YEAR ,4)+1 )
                    nday = iday2-iday
      if(nday.lt.0) nday = nday + 1461
                    nday = nday + nsegd
      MAPL_nsecf2 = MAPL_nsecf2 + nday*nsday
end function MAPL_nsecf2

integer function MAPL_nhmsf (nsec)
        implicit none
        integer  nsec
        MAPL_nhmsf =  nsec/3600*10000 + mod(nsec,3600)/60*100 + mod(nsec,60)
end function MAPL_nhmsf

! A year is a leap year if
! 1) it is divible by 4, and 
! 2) it is not divisible by 100, unless
! 3) it is also divisible by 400.
logical function MAPL_LEAP(NY)
   integer, intent(in) :: NY

   MAPL_LEAP = mod(NY,4)==0 .and. (mod(NY,100)/=0 .or. mod(NY,400)==0)
   
end function MAPL_LEAP


integer function MAPL_incymd (NYMD,M)                                                  
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
   20 CONTINUE                                                                  
      MAPL_INCYMD = NY*10000 + NM*100 + ND                                           
      RETURN                                                                    
end function MAPL_incymd


subroutine MAPL_PICKEM(II,JJ,IM,JM,COUNT)
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




    subroutine MAPL_GetFieldTimeFromField ( FIELD, TIME, RC )
      type(ESMF_Field),        intent(INOUT) :: FIELD ! ALT: IN
      type(ESMF_Time),         intent(  OUT) :: TIME
      integer, optional,       intent(  OUT) :: RC

      character(len=ESMF_MAXSTR),parameter   :: IAm=" MAPL_GetFieldTimeFromField"
      integer                                :: STATUS

      integer                                :: YEAR, MONTH, DAY
      integer                                :: HOUR, MINUTE, SCND
      character(len=ESMF_MAXSTR)             :: TIMESTAMP
      logical                                :: isPresent

      call ESMF_AttributeGet(FIELD, NAME="TimeStamp", isPresent=isPresent, RC=STATUS)
      _VERIFY(STATUS)
      if(.not. isPresent) then
         call ESMF_TimeSet          (TIME,      YY=0,                RC=STATUS)
      else
         call ESMF_AttributeGet(FIELD, NAME="TimeStamp", VALUE=TIMESTAMP, RC=STATUS)
         _VERIFY(STATUS)

         call MAPL_TimeStringGet    (TIMESTAMP, YY=YEAR, MM=MONTH,  DD=DAY,   & 
                                                H =HOUR, M =MINUTE, S =SCND   )
         _VERIFY(STATUS)
         call ESMF_TimeSet          (TIME,      YY=YEAR, MM=MONTH,  DD=DAY,   &
                                                H =HOUR, M =MINUTE, S =SCND,  &
                                                                     RC=STATUS)
         _VERIFY(STATUS)
      end if

      _RETURN(ESMF_SUCCESS)
    end subroutine MAPL_GetFieldTimeFromField

! ------------------------------------------------------------------------------

    subroutine  MAPL_SetFieldTimeFromField (FIELD, TIME, RC )
      type(ESMF_FIELD),        intent(INOUT) :: FIELD
      type(ESMF_TIME),         intent(INOUT) :: TIME !ALT: IN
      integer, optional,       intent(  OUT) :: RC

      character(len=ESMF_MAXSTR),parameter   :: IAm=" MAPL_SetFieldTimeFromField"
      integer                                :: STATUS

      character(len=ESMF_MAXSTR)             :: TIMESTAMP

      call ESMF_TimeGet          (TIME,  timeString=TIMESTAMP,             RC=STATUS)
      _VERIFY(STATUS)
      call ESMF_AttributeSet(FIELD, NAME="TimeStamp", VALUE=TIMESTAMP, RC=STATUS)
      _VERIFY(STATUS)

      _RETURN(ESMF_SUCCESS)
    end subroutine  MAPL_SetFieldTimeFromField


    subroutine  MAPL_GetFieldTimeFromState ( STATE, Fieldname, TIME, RC )
      type(ESMF_STATE),        intent(IN   ) :: STATE
      character(len=*),        intent(IN   ) :: Fieldname
      type(ESMF_Time),         intent(  OUT) :: TIME
      integer, optional,       intent(  OUT) :: RC

      character(len=ESMF_MAXSTR),parameter   :: IAm=" MAPL_GetFieldTimeFromState"
      integer                                :: STATUS

      type(ESMF_FIELD)                       :: FIELD

      call ESMF_StateGet (STATE, FIELDNAME, FIELD, RC=STATUS )
      _VERIFY(STATUS)
      call MAPL_FieldGetTime  (FIELD, TIME,             RC=STATUS)
      _VERIFY(STATUS)

      _RETURN(ESMF_SUCCESS)
    end subroutine  MAPL_GetFieldTimeFromState

! ------------------------------------------------------------------------------

    subroutine  MAPL_SetFieldTimeFromState ( STATE, Fieldname, TIME, RC )
      type(ESMF_STATE),        intent(INOUT) :: STATE
      character(len=*),        intent(IN   ) :: Fieldname
      type(ESMF_Time),         intent(INOUT) :: TIME !ALT: IN
      integer, optional,       intent(  OUT) :: RC

      character(len=ESMF_MAXSTR),parameter   :: IAm=" MAPL_SetFieldTimeFromState"
      integer                                :: STATUS

      type(ESMF_FIELD)                       :: FIELD

      call ESMF_StateGet (STATE, FIELDNAME, FIELD, RC=STATUS)
      _VERIFY(STATUS)
      call MAPL_FieldSetTime  (FIELD, TIME,             RC=STATUS)
      _VERIFY(STATUS)

      _RETURN(ESMF_SUCCESS)
    end subroutine  MAPL_SetFieldTimeFromState


    function MAPL_FieldCreateRename(FIELD, NAME, DoCopy, RC) RESULT(F)
      type (ESMF_Field), intent(INOUT) :: FIELD !ALT: IN
      character(len=*),  intent(IN   ) :: NAME
      logical, optional, intent(IN   ) :: DoCopy
      integer, optional, intent(  OUT) :: RC
      type (ESMF_Field)                :: F

!   we are creating new field so that we can change the name of the field;
!   the important thing is that the data (ESMF_Array) and the grid (ESMF_Grid) 
!   are the SAME as the one in the original Field, if DoCopy flag is present
!   and set to true we create a new array and copy the data, not just reference it

      type(ESMF_Grid)         :: grid
      character(len=ESMF_MAXSTR)       :: fieldName
      integer, allocatable    :: gridToFieldMap(:)
      integer                 :: gridRank
      integer                 :: fieldRank
      integer                 :: status
      integer                 :: unGridDims
      character(len=ESMF_MAXSTR), parameter :: Iam='MAPL_FieldCreateRename'
      logical                 :: hasUngridDims
      integer                 :: notGridded
      logical                 :: DoCopy_
      type(ESMF_DataCopy_Flag):: datacopy
      real, pointer           :: var_1d(:)
      real, pointer           :: var_2d(:,:)
      real, pointer           :: var_3d(:,:,:)
      real, pointer           :: var_4d(:,:,:,:)
      real(kind=REAL64), pointer           :: vr8_1d(:)
      real(kind=REAL64), pointer           :: vr8_2d(:,:)
      real(kind=REAL64), pointer           :: vr8_3d(:,:,:)
      real(kind=REAL64), pointer           :: vr8_4d(:,:,:,:)
      type(ESMF_TypeKind_Flag)  :: tk

      DoCopy_ = .false.
      if (present(DoCopy) ) then
         DoCopy_ = DoCopy
      end if

      call ESMF_FieldGet(FIELD, grid=GRID, dimCount=fieldRank, &
           name=fieldName, RC=STATUS)
      _VERIFY(STATUS)
      call ESMF_GridGet(GRID, dimCount=gridRank, rc=status)
      _VERIFY(STATUS)
      allocate(gridToFieldMap(gridRank), stat=status)
      _VERIFY(STATUS)
      call ESMF_FieldGet(FIELD, gridToFieldMap=gridToFieldMap, typekind=tk, RC=STATUS)
      _VERIFY(STATUS)

      hasUngridDims = .false.
      notGridded = count(gridToFieldMap==0)
      unGridDims = fieldRank - gridRank + notGridded

      if (unGridDims > 0) then
         hasUngridDims = .true.
      endif

      if (doCopy_) then 
         datacopy = ESMF_DATACOPY_VALUE
      else
         datacopy = ESMF_DATACOPY_REFERENCE
      end if

      f = MAPL_FieldCreateEmpty(name=NAME, grid=grid, rc=status)
      _VERIFY(STATUS)

      if (tk == ESMF_TypeKind_R4) then
         select case (fieldRank)
            case (1)
               call ESMF_FieldGet(field, farrayPtr=var_1d, rc=status)
               _VERIFY(STATUS)
               call ESMF_FieldEmptyComplete(F, farrayPtr=VAR_1D,    &
                    gridToFieldMap=gridToFieldMap,                      &
                    datacopyFlag = datacopy,             &
                    rc = status)
               _VERIFY(STATUS)
            case (2)
               call ESMF_FieldGet(field, farrayPtr=var_2d, rc=status)
               _VERIFY(STATUS)
               call ESMF_FieldEmptyComplete(F, farrayPtr=VAR_2D,    &
                    gridToFieldMap=gridToFieldMap,                      &
                    datacopyFlag = datacopy,             &
                    rc = status)
               _VERIFY(STATUS)
            case (3)
               call ESMF_FieldGet(field, farrayPtr=var_3d, rc=status)
               _VERIFY(STATUS)
               call ESMF_FieldEmptyComplete(F, farrayPtr=VAR_3D,    &
                    gridToFieldMap=gridToFieldMap,                      &
                    datacopyFlag = datacopy,             &
                    rc = status)
               _VERIFY(STATUS)
            case (4)
               call ESMF_FieldGet(field, farrayPtr=var_4d, rc=status)
               _VERIFY(STATUS)
               call ESMF_FieldEmptyComplete(F, farrayPtr=VAR_4D,    &
                    gridToFieldMap=gridToFieldMap,                      &
                    datacopyFlag = datacopy,             &
                    rc = status)
               _VERIFY(STATUS)
            case default
               _ASSERT(.false., 'only upto 4D are supported')
         end select
      else if (tk == ESMF_TypeKind_R8) then
         select case (fieldRank)
            case (1)
               call ESMF_FieldGet(field, farrayPtr=vr8_1d, rc=status)
               _VERIFY(STATUS)
               call ESMF_FieldEmptyComplete(F, farrayPtr=VR8_1D,    &
                    gridToFieldMap=gridToFieldMap,                      &
                    datacopyFlag = datacopy,             &
                    rc = status)
               _VERIFY(STATUS)
            case (2)
               call ESMF_FieldGet(field, farrayPtr=vr8_2d, rc=status)
               _VERIFY(STATUS)
               call ESMF_FieldEmptyComplete(F, farrayPtr=VR8_2D,    &
                    gridToFieldMap=gridToFieldMap,                      &
                    datacopyFlag = datacopy,             &
                    rc = status)
               _VERIFY(STATUS)
            case (3)
               call ESMF_FieldGet(field, farrayPtr=vr8_3d, rc=status)
               _VERIFY(STATUS)
               call ESMF_FieldEmptyComplete(F, farrayPtr=VR8_3D,    &
                    gridToFieldMap=gridToFieldMap,                      &
                    datacopyFlag = datacopy,             &
                    rc = status)
               _VERIFY(STATUS)
            case (4)
               call ESMF_FieldGet(field, farrayPtr=vr8_4d, rc=status)
               _VERIFY(STATUS)
               call ESMF_FieldEmptyComplete(F, farrayPtr=VR8_4D,    &
                    gridToFieldMap=gridToFieldMap,                      &
                    datacopyFlag = datacopy,             &
                    rc = status)
               _VERIFY(STATUS)
            case default
               _ASSERT(.false., 'only 2D and 3D are supported')
         end select
      else
         _ASSERT(.false., 'unsupported typekind')
      endif

      deallocate(gridToFieldMap)

      call MAPL_FieldCopyAttributes(FIELD_IN=field, FIELD_OUT=f, RC=status)
      _VERIFY(STATUS)

      _RETURN(ESMF_SUCCESS)
    end function MAPL_FieldCreateRename

    function MAPL_FieldCreateNewgrid(FIELD, GRID, LM, NEWNAME, RC) RESULT(F)
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

      call ESMF_FieldGet(FIELD, grid=fgrid, RC=STATUS)
      _VERIFY(STATUS)

      call ESMF_GridGet(fGRID, dimCount=fgridRank, rc=status)
      _VERIFY(STATUS)
      allocate(gridToFieldMap(fgridRank), stat=status)
      _VERIFY(STATUS)
      call ESMF_FieldGet(FIELD, Array=Array, name=name, &
           gridToFieldMap=gridToFieldMap, RC=STATUS)
      _VERIFY(STATUS)
      griddedDims = fgridRank - count(gridToFieldMap == 0)

      call ESMF_GridGet(GRID, dimCount=gridRank, rc=status)
      _VERIFY(STATUS)

      call ESMF_ArrayGet(array, rank=rank, rc=status)
      _VERIFY(STATUS)
      ungriddedDims = rank - griddedDims

      call MAPL_GridGet(GRID, localCellCountPerDim=COUNTS, RC=STATUS)
      _VERIFY(STATUS)

      call ESMF_ArrayGet(array, localDeCount=localDeCount, rc=status)
      _VERIFY(STATUS)
      _ASSERT(localDeCount == 1, 'MAPL supports only 1 local array')
      call ESMF_ArrayGet(array, localarrayList=larrayList, rc=status)
      _VERIFY(STATUS)
      larray => lArrayList(1) ! alias

      call ESMF_LocalArrayGet(larray, totalLBound=lbnds, totalUBound=ubnds, rc=status)
      _VERIFY(STATUS)

      newRank = rank
      if (griddedDims == 1 .and. gridRank > 1) then
         deallocate(gridToFieldMap)
         allocate(gridToFieldMap(gridRank), stat=status)
         _VERIFY(STATUS)
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
              name=newName_, gridToFieldMap=gridToFieldMap, RC=STATUS )
         _VERIFY(STATUS)
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
              ungriddedLBound=[lb],ungriddedUBound=[ub],RC=STATUS )
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
              ungriddedUBound=[ubnds(griddedDims+1),ubnds(griddedDims+2)],RC=STATUS )
         if (ungriddedDims > 0) then
            DIMS = MAPL_DimsHorzOnly
         else
            DIMS = MAPL_DimsHorzVert
         end if
      else
         _ASSERT(.false., 'rank > 4 not supported')
      end if

      deallocate(gridToFieldMap)

      call MAPL_FieldCopyAttributes(FIELD_IN=field, FIELD_OUT=f, RC=status)
      _VERIFY(STATUS)
! we are saving DIMS attribute in case the FIELD did not contain one
! otherwise we will overwrite it
      call ESMF_AttributeSet(F, NAME='DIMS', VALUE=DIMS, RC=STATUS)
      _VERIFY(STATUS)

      _RETURN(ESMF_SUCCESS)
    end function MAPL_FieldCreateNewgrid

    function MAPL_FieldCreateR4(FIELD, RC) RESULT(F)
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
      real*8, pointer           :: vr8_1d(:)
      real*8, pointer           :: vr8_2d(:,:)
      real*8, pointer           :: vr8_3d(:,:,:)
      type(ESMF_TypeKind_Flag)  :: tk

      call ESMF_FieldGet(FIELD, grid=GRID, dimCount=fieldRank, &
           name=fieldName, typekind=tk, RC=STATUS)
      _VERIFY(STATUS)
      _ASSERT(tk == ESMF_TypeKind_R8, 'tk /= ESMF_TypeKind_R8')
      call ESMF_GridGet(GRID, dimCount=gridRank, rc=status)
      _VERIFY(STATUS)
      allocate(gridToFieldMap(gridRank), stat=status)
      _VERIFY(STATUS)
      call ESMF_FieldGet(FIELD, gridToFieldMap=gridToFieldMap, RC=STATUS)
      _VERIFY(STATUS)

      datacopy = ESMF_DATACOPY_REFERENCE

      select case (fieldRank)
      case (1)
         call ESMF_FieldGet(field, farrayPtr=vr8_1d, rc=status)
         _VERIFY(STATUS)
         allocate(var_1d(lbound(vr8_1d,1):ubound(vr8_1d,1)), stat=status)
         _VERIFY(STATUS)
         var_1d=vr8_1d
         f = MAPL_FieldCreateEmpty(name=fieldNAME, grid=grid, rc=status)
         _VERIFY(STATUS)
         call ESMF_FieldEmptyComplete(F, farrayPtr=VAR_1D,    &
              gridToFieldMap=gridToFieldMap,                      &
              datacopyFlag = datacopy,             &
              rc = status)
         _VERIFY(STATUS)
      case (2)
         call ESMF_FieldGet(field, farrayPtr=vr8_2d, rc=status)
         _VERIFY(STATUS)
         allocate(var_2d(lbound(vr8_2d,1):ubound(vr8_2d,1), &
                         lbound(vr8_2d,2):ubound(vr8_2d,2)), &
                         stat=status)
         _VERIFY(STATUS)
         var_2d=vr8_2d
         f = MAPL_FieldCreateEmpty(name=fieldNAME, grid=grid, rc=status)
         _VERIFY(STATUS)
         call ESMF_FieldEmptyComplete(F, farrayPtr=VAR_2D,    &
              gridToFieldMap=gridToFieldMap,                      &
              datacopyFlag = datacopy,             &
              rc = status)
         _VERIFY(STATUS)
      case (3)
         call ESMF_FieldGet(field, farrayPtr=vr8_3d, rc=status)
         _VERIFY(STATUS)
         allocate(var_3d(lbound(vr8_3d,1):ubound(vr8_3d,1), &
                         lbound(vr8_3d,2):ubound(vr8_3d,2), &
                         lbound(vr8_3d,3):ubound(vr8_3d,3)), &
                         stat=status)
         _VERIFY(STATUS)
         var_3d=vr8_3d
         f = MAPL_FieldCreateEmpty(name=fieldNAME, grid=grid, rc=status)
         _VERIFY(STATUS)
         call ESMF_FieldEmptyComplete(F, farrayPtr=VAR_3D,    &
              gridToFieldMap=gridToFieldMap,                      &
              datacopyFlag = datacopy,             &
              rc = status)
         _VERIFY(STATUS)
      case default
         _ASSERT(.false., 'only 2D and 3D are supported')
      end select

      deallocate(gridToFieldMap)

      call MAPL_FieldCopyAttributes(FIELD_IN=field, FIELD_OUT=f, RC=status)
      _VERIFY(STATUS)

      _RETURN(ESMF_SUCCESS)
    end function MAPL_FieldCreateR4

    function MAPL_FieldCreateEmpty(NAME, GRID, RC) RESULT(FIELD)
      character(len=*),  intent(IN   ) :: NAME
      type (ESMF_Grid),  intent(INout) :: GRID
      integer, optional, intent(  OUT) :: RC
      type (ESMF_Field)                :: FIELD

      character(len=ESMF_MAXSTR),parameter   :: IAm=" MAPL_FieldCreateEmpty"
      integer                                :: STATUS

      FIELD = ESMF_FieldEmptyCreate(name=name, rc=status)
      _VERIFY(STATUS)

      call ESMF_FieldEmptySet(FIELD, &
           grid=GRID, &
           staggerloc = ESMF_STAGGERLOC_CENTER,        &
           rc = status)
      _VERIFY(STATUS)

      _RETURN(ESMF_SUCCESS)

    end function MAPL_FieldCreateEmpty

    subroutine MAPL_FieldCopyAttributes(FIELD_IN, FIELD_OUT, RC)
      type (ESMF_Field), intent(INOUT) :: FIELD_IN !ALT: intent(in)
      type (ESMF_Field), intent(INOUT) :: FIELD_OUT
      integer, optional, intent(  OUT) :: RC

      type (ESMF_TypeKind_Flag)        :: tk
      integer                          :: status
      character(len=ESMF_MAXSTR), parameter :: Iam='MAPL_FieldCopyAttributes'
      integer                          :: i, n, count
      character(len=ESMF_MAXSTR)       :: attname
      character(len=ESMF_MAXSTR)       :: att
      integer, pointer                 :: iptr(:)
      logical, pointer                 :: lptr(:)
      real,    pointer                 :: rptr(:)

      call ESMF_AttributeGet(field_in, count=n, rc=status)
      _VERIFY(STATUS)

      do i = 1, n
         call  ESMF_AttributeGet(field_in, attributeIndex=i, name=attname, &
                                          typekind=tk, itemcount=count, rc=status)
         _VERIFY(STATUS)

         if (tk == ESMF_TypeKind_I4) then
            allocate(iptr(count), stat=status)
            _VERIFY(STATUS)
            call ESMF_AttributeGet(field_in,  NAME=attname, itemcount=count, VALUELIST=iptr, RC=STATUS)
            _VERIFY(STATUS)
            call ESMF_AttributeSet(field_out, NAME=attname, itemcount=count, VALUELIST=iptr, RC=STATUS)
            _VERIFY(STATUS)
            deallocate(iptr)

         else if (tk == ESMF_TypeKind_Logical) then
            allocate(lptr(count), stat=status)
            _VERIFY(STATUS)
            call ESMF_AttributeGet(field_in,  NAME=attname, itemcount=count, VALUELIST=lptr, RC=STATUS)
            _VERIFY(STATUS)
            call ESMF_AttributeSet(field_out, NAME=attname, itemcount=count, VALUELIST=lptr, RC=STATUS)
            _VERIFY(STATUS)
            deallocate(lptr)

         else if (tk == ESMF_TypeKind_R4) then
            allocate(rptr(count), stat=status)
            _VERIFY(STATUS)
            call ESMF_AttributeGet(field_in,  NAME=attname, itemcount=count, VALUELIST=rptr, RC=STATUS)
            _VERIFY(STATUS)
            call ESMF_AttributeSet(field_out, NAME=attname, itemcount=count, VALUELIST=rptr, RC=STATUS)
            _VERIFY(STATUS)
            deallocate(rptr)

         else if (tk == ESMF_TypeKind_Character) then
            call ESMF_AttributeGet(field_in,  NAME=attname, VALUE=att, RC=STATUS)
            _VERIFY(STATUS)
            call ESMF_AttributeSet(field_out, NAME=attname, VALUE=att, RC=STATUS)
            _VERIFY(STATUS)

         else
            _RETURN(ESMF_FAILURE)
         end if
      end do
      _RETURN(ESMF_SUCCESS)
    end subroutine MAPL_FieldCopyAttributes

    subroutine MAPL_FieldCopy(from, to, RC)
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
      real*8, pointer           :: vr8_1d(:)
      real*8, pointer           :: vr8_2d(:,:)
      real*8, pointer           :: vr8_3d(:,:,:)
      type(ESMF_TypeKind_Flag)  :: tk

      call ESMF_FieldGet(from, dimCount=fieldRank, &
           typekind=tk, RC=STATUS)
      _VERIFY(STATUS)
      _ASSERT(tk == ESMF_TypeKind_R8, 'inconsistent typekind (should be ESMF_TypeKind_R8)')

      select case (fieldRank)
      case (1)
         call ESMF_FieldGet(from, farrayPtr=vr8_1d, rc=status)
         _VERIFY(STATUS)
         call ESMF_FieldGet(to, dimCount=fieldRank, typekind=tk, RC=STATUS)
         _VERIFY(STATUS)
         _ASSERT(tk == ESMF_TypeKind_R4, 'inconsistent typekind (should be ESMF_TypeKind_R4)')
         _ASSERT(fieldRank==1, 'inconsistent fieldrank (should be 1)')
         call ESMF_FieldGet(to, farrayPtr=var_1d, rc=status)
         _VERIFY(STATUS)
         var_1d = vr8_1d
      case (2)
         call ESMF_FieldGet(from, farrayPtr=vr8_2d, rc=status)
         _VERIFY(STATUS)
         call ESMF_FieldGet(to, dimCount=fieldRank, typekind=tk, RC=STATUS)
         _VERIFY(STATUS)
         _ASSERT(tk == ESMF_TypeKind_R4, 'inconsistent typekind (should be ESMF_TypeKind_R4)')
         _ASSERT(fieldRank==2, 'inconsistent fieldRank (should be 2)')
         call ESMF_FieldGet(to, farrayPtr=var_2d, rc=status)
         _VERIFY(STATUS)
         var_2d = vr8_2d
      case (3)
         call ESMF_FieldGet(from, farrayPtr=vr8_3d, rc=status)
         _VERIFY(STATUS)
         call ESMF_FieldGet(to, dimCount=fieldRank, typekind=tk, RC=STATUS)
         _VERIFY(STATUS)
         _ASSERT(tk == ESMF_TypeKind_R4, 'inconsistent typekind (should be ESMF_TypeKind_R4)')
         _ASSERT(fieldRank==3,'inconsistent fieldRank (should be 3)')
         call ESMF_FieldGet(to, farrayPtr=var_3d, rc=status)
         _VERIFY(STATUS)
         var_3d = vr8_3d
      case default
         _ASSERT(.false., 'unsupported fieldRank (> 3)')
      end select

      _RETURN(ESMF_SUCCESS)
    end subroutine MAPL_FieldCopy

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      function MAPL_RemapBoundsFull_3dr4(A,I1,IM,J1,JM,L1,LM)
        integer,      intent(IN) :: I1,IM,J1,JM,L1,LM
        real, target, intent(IN) :: A(I1:IM,J1:JM,L1:LM)
        real, pointer            :: MAPL_RemapBoundsFull_3dr4(:,:,:)

        MAPL_RemapBoundsFull_3dr4 => A
      end function MAPL_RemapBoundsFull_3dr4


!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      function MAPL_RemapBounds_3dr4(A, LB1, LB2, LB3) result(ptr)
        integer,      intent(IN) :: LB1, LB2, LB3
        real, target, intent(IN) :: A(LB1:,LB2:,LB3:)
        real, pointer            :: ptr(:,:,:)

        ptr => A
      end function MAPL_RemapBounds_3dr4

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      function MAPL_RemapBounds_3dr8(A, LB1, LB2, LB3) result(ptr)
        integer,      intent(IN) :: LB1, LB2, LB3
        real(kind=REAL64), target, intent(IN) :: A(LB1:,LB2:,LB3:)
        real(kind=REAL64), pointer            :: ptr(:,:,:)

        ptr => A
      end function MAPL_RemapBounds_3dr8

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!BOP

! !IROUTINE: MAPL_LatLonGridCreate --- Create regular Lat/Lon Grid
!
! !INTERFACE:

  function MAPL_LatLonGridCreate (Name, vm,                 &
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

#ifdef ___PROTEX___

!DESCRIPTION: 

This routine creates a distributed ESMF grid where the horizontal
coordinates are regular longitudes and latitudes. The grid is 
created on the user specified {\bf VM}, or on the current VM if the user 
does not specify one. The layout and the coordinate information can
be provided with a {\tt ESMF\_Config attribute}, a resource file name
or specified through the argument list.

 \subsubsection*{Using resource files}

The {\bf resource file} {\tt ConfigFile} has a syntax similar to a GrADS
control file.  Here is an example defining a typical GEOS-5 1x1.25
grid with 72 layers:
%
\begin{verbatim}
GDEF: LatLon 
IDEF: 32  
JDEF: 16  
LDEF:  1  
XDEF: 288 LINEAR -180. 1.25
YDEF: 181 LINEAR -90. 1.
ZDEF:  72 LINEAR 1 1
\end{verbatim}
%
More generally, 
\begin{verbatim}
GDEF: LatLon 
IDEF: Nx 
JDEF: Ny
LDEF: Nz
XDEF: IM_World XCoordType BegLon, DelLon
YDEF: JM_World YCoordType BegLat, DelLat
ZDEF: LM_World ZCoordType 1        1
\end{verbatim}
The attribute {\bf GDEF} must always be {\tt LatLon} for  Lat/Lon grids. 
The remaining parameters are:
\bd
\item[Nx] is the number of processors used to decompose the X dimension
\item[Ny] is the number of processors used to decompose the Y dimension
\item[Nz] is the number of processors used to decompose the Z dimension;
          must be 1 for now.          
\item[IM\_World] is the number of longitudinal grid points; 
         if {\tt IM\_World=0} then the grid has no zonal dimension.
\item[XCoordType] must be set to LINEAR
\item[BegLon] is the longitude (in degrees) of the {\em center} of the first 
              gridbox
\item[DelLon] is the constant mesh size (in degrees); if {\tt DelLon<1} then a
            global grid is assumed.
%
\item[JM\_World] is the number of meridional grid points
         if {\tt JM\_World=0} then the grid has no meridional dimension.
\item[YCoordType] must be set to LINEAR
\item[BegLat] is the latitude (in degrees) of the {\em center} of the first 
              gridbox
\item[DelLat] is the constant mesh size (in degrees); if {\tt DelLat<1} then a
              global grid is assumed.
%
\item[LM\_World] is the number of vertical grid points;
              if {\tt LM\_World=0} then the grid has no vertical dimension.
\ed
As of this writing, only the size of the vertical grid ({\tt LM\_World})
needs to be specified.

 \subsubsection*{Passing an ESMF Config}

The {\bf ESMF\_Config} object {\tt Config}, when specified, must
contain the same information as the resource file above.

subsubsection*{Providing parameters explicitly through the argument list}

Alternatively, one can specify coordinate information in the argument
list; their units and meaning is as in the resource file above. In
this case you must specify at least {\tt Nx, Ny, IM\_World, JM\_World,} and 
{\tt LM\_World}. The other parameters have default values
\bd
\item[BegLon] defaults to -180. (the date line)
\item[DelLon] defaults to -1. (meaning a global grid)
\item[BegLat] defaults to -90. (the south pole)
\item[DelLat] deaults to -1. (meaning a global grid)
\ed

  \subsubsection*{Restrictions}

The current implementation imposes the following 
restrictions:
\begin{enumerate}
\item Only uniform longitude/latitude grids are supported (no Gaussian grids).
\item Only 2D Lon-Lat or 3D Lon-Lat-Lev grids are currently supported 
      (no Lat-Lev or Lon-Lev grids supprted yet).
\item No vertical decomposition yet ({\tt Nz=1}).
\end{enumerate}

 \subsubsection*{Future enhancements}

The {\tt IDEF/JDEF/LDEF} records in the resource file should be
extended as to allow specification of a more general distribution.
For consistency with the {\tt XDEF/YDEF/ZDEF} records a similar 
syntax could be adopted. For example,
%
\begin{verbatim}
IDEF 4   LEVELS  22 50 50 22 
XDEF 144 LINEAR -180 2.5 
\end{verbatim}
would indicate that longitudes would be decomposed in 4 PETs,
with the first PET having 22 grid points, the second 50 gridpoints,
and so on. 

#endif

!
!EOP
!                                 ------


!   Internal version of the input arguments
!   ---------------------------------------
    type(ESMF_Config), pointer :: Config_
    integer           :: IM_World_      
    real*8            :: BegLon_
    real*8            :: DelLon_ 
    integer           :: JM_World_      
    real*8            :: BegLat_
    real*8            :: DelLat_
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

    real*8, parameter                 :: D2R = MAPL_PI_R8 / 180
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
      allocate(vm_, stat=STATUS)
      _VERIFY(STATUS)
      call ESMF_VMGetCurrent(vm_, rc=STATUS)
      _VERIFY(STATUS)
   end if

! Grid info via resources
! -----------------------
  if ( present(Config) .or. present(ConfigFile) ) then

!    Either use supplied Config or load resource file
!    ------------------------------------------------
     if ( present(ConfigFile) ) then
          allocate(Config_,stat=STATUS)
          _VERIFY(STATUS)
          Config_ = ESMF_ConfigCreate (rc=STATUS )
          _VERIFY(STATUS)
          call ESMF_ConfigLoadFile (Config_, ConfigFile, rc=STATUS )
          _VERIFY(STATUS)
     else if ( present(Config) ) then
          Config_ => Config
     else
        STATUS = 100
        _VERIFY(STATUS)
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
     _VERIFY(STATUS)

   end if
  
!  Global grids
!  ------------
   if ( IM_World_ < 1 .OR. JM_World_ < 1 ) then
        STATUS = 400
        _VERIFY(STATUS)
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
   allocate( IMs(0:Nx_-1), JMs(0:Ny_-1), LMs(0:Nz_-1), stat=STATUS)
   _VERIFY(STATUS) 
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
               rc=STATUS)
          _VERIFY(STATUS)
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
               rc=status)
          _VERIFY(STATUS)

          call ESMF_AttributeSet(grid, name='GRID_LM', value=LM_World, rc=status)
          _VERIFY(STATUS)

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
               rc=STATUS)
          _VERIFY(STATUS)

!  Other possibilities not implemented yet
!  --------------------------------------- 
   else
 
          STATUS = 300
          _VERIFY(STATUS)

   endif

!  -------------------------------------------------------------------
!  NOTE: In the remaining part of this routine it is assumed that the 
!        1st and 2nd axes correspond to lat/lon; revise this for other 
!        arrangements (say, YZ grids)
!  -------------------------------------------------------------------

!  Allocate coords at default stagger location
!  -------------------------------------------
   call ESMF_GridAddCoord(Grid, rc=status)
   _VERIFY(STATUS)

!  Compute the coordinates (the corner/center is for backward compatibility)
!  -------------------------------------------------------------------------
   deltaX      = D2R * DelLon_
   deltaY      = D2R * DelLat_
   minCoord(1) = D2R * BegLon_ - deltaX/2
   minCoord(2) = D2R * BegLat_ - deltaY/2 

   allocate(cornerX(IM_World_+1),cornerY(JM_World_+1), stat=STATUS)
   _VERIFY(STATUS)

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
                           farrayPtr=centerX, rc=status)
   _VERIFY(STATUS)
   
   call ESMF_GridGetCoord (Grid, coordDim=2, localDE=0, &
                           staggerloc=ESMF_STAGGERLOC_CENTER, &
                           farrayPtr=centerY, rc=status)
   _VERIFY(STATUS)

   FirstOut(1)=BegLon_
   FirstOut(2)=-90.
   LastOut(1)=360.+BegLon_ - 360./im_world_
   LastOut(2)=90. 

   block
     use MAPL_ConstantsMod, only: MAPL_DEGREES_TO_RADIANS
     real(kind=REAL64), allocatable :: lons(:)
     real(kind=REAL64), allocatable :: lats(:)
     
     lons = MAPL_Range(FirstOut(1), LastOut(1), im_world_, &
          & conversion_factor=MAPL_DEGREES_TO_RADIANS)
     lats = MAPL_Range(FirstOut(2), LastOut(2), JM_WORLD, &
          & conversion_factor=MAPL_DEGREES_TO_RADIANS)
     
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
   call ESMF_GridValidate(Grid,rc=status)
   _VERIFY(STATUS)

!  Clean up
!  --------   
   deallocate(cornerY,cornerX)
   deallocate(IMs,JMs,LMs)
   if ( present(ConfigFile) ) deallocate(Config_)
   if ( .not. present(vm) )   deallocate(vm_)

!  All Done
!  --------
   _RETURN(STATUS)

   Contains

     subroutine parseConfig_()
!
!    Internal routine to parse the ESMF_Config.
!
       STATUS = 200     ! not implemented yet
       _VERIFY(STATUS)

    end Subroutine parseConfig_

 end function MAPL_LatLonGridCreate

!............................................................................

 subroutine MAPL_GRID_INTERIOR(GRID,I1,IN,J1,JN)
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

    i1=-1
    j1=-1
    in=-1
    jn=-1
    call ESMF_GridGet    (GRID, dimCount=gridRank, distGrid=distGrid, rc=STATUS)
    call ESMF_DistGridGet(distGRID, delayout=layout, rc=STATUS)
    call ESMF_DELayoutGet(layout, deCount = nDEs, localDeCount=localDeCount,rc=status)
    if (localDeCount > 0) then
       allocate(localDeToDeMap(localDeCount),stat=status)
       call ESMF_DELayoutGet(layout, localDEtoDeMap=localDeToDeMap,rc=status)
       deId=localDeToDeMap(1)

       allocate (AL(gridRank,0:nDEs-1),  stat=status)
       allocate (AU(gridRank,0:nDEs-1),  stat=status)

       call MAPl_DistGridGet(distgrid, &
            minIndex=AL, maxIndex=AU, rc=status)

       I1 = AL(1, deId)
       IN = AU(1, deId)
       !    _ASSERT(gridRank > 1, 'tilegrid is 1d (without RC this only for info')
       J1 = AL(2, deId)
       JN = AU(2, deId)
       deallocate(AU, AL, localDeToDeMap)
    end if

 end subroutine MAPL_GRID_INTERIOR

 subroutine MAPL_GridGetCorners(grid,gridCornerLons, gridCornerLats, RC)
      type (ESMF_Grid), intent(INOUT) :: GRID
      real(ESMF_KIND_R8), intent(INOUT) :: gridCornerLons(:,:)
      real(ESMF_KIND_R8), intent(INOUT) :: gridCornerLats(:,:)
      integer, optional, intent(  OUT) :: RC
      integer :: status

      type(ESMF_RouteHandle) :: rh
      type(ESMF_Field) :: field
      integer :: counts(3)
      real(ESMF_KIND_R8), pointer :: ptr(:,:)
      real(ESMF_KIND_R8), pointer :: corner(:,:)
      integer :: im,jm,imc,jmc

      call MAPL_GridGet(grid,localCellCountPerDim=counts,rc=status)
      _VERIFY(status)
      im=counts(1)
      jm=counts(2)

      call ESMF_GridGetCoord(grid,localDE=0,coordDim=1,staggerloc=ESMF_STAGGERLOC_CORNER, &
                              farrayPtr=corner, rc=status)
      imc=size(corner,1)
      jmc=size(corner,2)
      allocate(ptr(0:imc+1,0:jmc+1),source=0.0d0,stat=status)
      _VERIFY(status)
      field = ESMF_FieldCreate(grid,ptr,staggerLoc=ESMF_STAGGERLOC_CORNER,totalLWidth=[1,1],totalUWidth=[1,1],rc=status)
      _VERIFY(status)
      call ESMF_FieldHaloStore(field,rh,rc=status)
      _VERIFY(status)

      ptr(1:imc,1:jmc)=corner
      call ESMF_FieldHalo(field,rh,rc=status)
      _VERIFY(status)
      gridCornerLons=ptr(1:im+1,1:jm+1)

      !lats
      call ESMF_GridGetCoord(grid,localDE=0,coordDim=2,staggerloc=ESMF_STAGGERLOC_CORNER, &
                              farrayPtr=corner, rc=status)
      _VERIFY(status)
      ptr(1:imc,1:jmc)=corner
      call ESMF_FieldHalo(field,rh,rc=status)
      _VERIFY(status)
      gridCornerLats=ptr(1:im+1,1:jm+1)
      
      deallocate(ptr)
      call ESMF_FieldDestroy(field,rc=status) 
      _VERIFY(status)
      call ESMF_FieldHaloRelease(rh,rc=status)
      _VERIFY(status)

      _RETURN(ESMF_SUCCESS)

   end subroutine MAPL_GridGetCorners

!............................................................................

  subroutine MAPL_GridGet(GRID, globalCellCountPerDim, localCellCountPerDim, RC)
      type (ESMF_Grid), intent(IN)     :: GRID
      integer, optional, intent(INout) :: globalCellCountPerDim(:)
      integer, optional, intent(INout) :: localCellCountPerDim(:)
      integer, optional, intent(  OUT) :: RC

! local vars
      integer :: status

      integer :: mincounts(ESMF_MAXDIM)
      integer :: maxcounts(ESMF_MAXDIM)
      integer :: gridRank
      integer :: UNGRID
      integer :: sz, tileCount
      logical :: plocal, pglobal, lxtradim
      logical :: isPresent,hasDE

      pglobal = present(globalCellCountPerDim)
      plocal  = present(localCellCountPerDim)

      if (pglobal .or. plocal) then
         call ESMF_GridGet(grid, dimCount=gridRank, rc=status)
         _VERIFY(STATUS)

!ALT kludge
         lxtradim = .false.
         if (gridRank == 1) then
            call ESMF_AttributeGet(grid, name='GRID_EXTRADIM', isPresent=isPresent, rc=status)
            _VERIFY(STATUS)
            if (isPresent) then
               call ESMF_AttributeGet(grid, name='GRID_EXTRADIM', value=UNGRID, rc=status)
               _VERIFY(STATUS)
               lxtradim = .true.
            end if
         else if (gridRank == 2) then
            call ESMF_AttributeGet(grid, name='GRID_LM', isPresent=isPresent, rc=status)
            _VERIFY(STATUS)
            if (isPresent) then
               call ESMF_AttributeGet(grid, name='GRID_LM', value=UNGRID, rc=status)
               _VERIFY(STATUS)
               lxtradim = .true.
            end if
         end if
      end if

      if (pglobal) then

         globalCellCountPerDim = 1
         call ESMF_GridGet(grid, tileCount=tileCount,rc=status)
         _VERIFY(status)

         call ESMF_GridGet(grid, tile=1, staggerLoc=ESMF_STAGGERLOC_CENTER, &
              minIndex=mincounts, &
              maxIndex=maxcounts, &
              rc = status)
         _VERIFY(STATUS)

         sz = min(gridRank, ESMF_MAXDIM, size(globalCellCountPerDim)) 
         globalCellCountPerDim(1:sz) = maxcounts(1:sz)-mincounts(1:sz)+1

         ! kludge for new cube sphere from ESMF
         if (tileCount == 6) then
            if (globalCellCountPerDim(1) /= 1) then ! kludge-on-the-kludge for Single-Column case
               globalCellCountPerDim(2)=globalCellCountPerDim(2)*6
            end if
         end if

         if (lxtradim ) then
            globalCellCountPerDim(gridRank+1) = UNGRID
         end if
      end if

      if (plocal) then
         localCellCountPerDim = 1

         HasDE = MAPL_GridHasDE(grid,rc=status)
         _VERIFY(status)
         if (HasDE) then
            call ESMF_GridGet(GRID, localDE=0, &
                 staggerloc=ESMF_STAGGERLOC_CENTER, &
                 exclusiveCount=localCellCountPerDim, RC=STATUS)
            _VERIFY(STATUS)
         end if

         if (lxtradim ) then
            localCellCountPerDim(gridRank+1) = UNGRID
         end if
      end if

      _RETURN(ESMF_SUCCESS)

    end subroutine MAPL_GridGet

!
! Note: The routine below came from ESMFL; it has been moved here to
!       avoid circular dependencies (Arlindo).
!
    subroutine MAPL_GridGetInterior(GRID,I1,IN,J1,JN)
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

    call ESMF_GridGet    (GRID, dimCount=gridRank, distGrid=distGrid, rc=STATUS)
    call ESMF_DistGridGet(distGRID, delayout=layout, rc=STATUS)
    call ESMF_DELayoutGet(layout, vm=vm, rc=status)
    call ESMF_VmGet(vm, localPet=deId, petCount=nDEs, rc=status)

    allocate (AL(gridRank,0:nDEs-1),  stat=status)
    allocate (AU(gridRank,0:nDEs-1),  stat=status)

    call MAPL_DistGridGet(distgrid, &
         minIndex=AL, maxIndex=AU, rc=status)

    I1 = AL(1, deId)
    IN = AU(1, deId)
!    _ASSERT(gridRank > 1, 'tilegrid is 1d (without RC this only for info')
    J1 = AL(2, deId)
    JN = AU(2, deId)
    deallocate(AU, AL)

  end subroutine MAPL_GridGetInterior

  function MAPL_GridHasDE(grid,rc) result(hasDE)
     type(ESMF_Grid), intent(in) :: grid
     integer, intent(out), optional :: rc

     integer :: status
     type(ESMF_DistGrid) :: distGrid
     type(ESMF_DeLayout) :: layout
     integer :: localDECount
     logical :: hasDE

     call ESMF_GridGet    (GRID, distGrid=distGrid, rc=STATUS)
     call ESMF_DistGridGet(distGRID, delayout=layout, rc=STATUS)
     call ESMF_DELayoutGet(layout, localDeCount=localDeCount,rc=status)
     hasDe = (localDECount /=0)
     _RETURN(_SUCCESS)

  end function MAPL_GridHasDE


!.......................................................................

     function MAPL_RmQualifier(str, del) result(new)

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



   function MAPL_StrUpCase(str) result(new)
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

   function MAPL_StrDnCase(str) result(new)
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

  subroutine MAPL_GetImsJms(Imins,Imaxs,Jmins,Jmaxs,Ims,Jms,rc)

!  Given lists of the min and max Is and Js in each processor
!  in a rectangular layout, it computes the number of elements
!  in each of the I columns and the number in each of the j
!  rows of the layout. Pointers Ims and Jms are allocated with the i and j
!  sizes of the layout (nx and ny) and filled with the IMs and JMs.

!  The four input lists must be in the same order, but the order
!  is arbitrary. Nx and Ny can be obtained from the sizes of
!  Ims and Jms, respectively


    use MAPL_SortMod

    integer, dimension(:), intent(IN   ) :: Imins,Imaxs,Jmins,Jmaxs
    integer, pointer                     :: Ims(:),Jms(:)
    integer, optional,   intent(out) :: rc
    
    integer              :: nx, ny, nx0, ny0, nde, k
    integer, allocatable :: Im0(:), Jm0(:)
    integer              :: minI,minJ ! in case the starting index is zero    
    integer              :: status
    
    _ASSERT(.not.associated(Ims), 'Ims is associated and should not be.')
    _ASSERT(.not.associated(Jms), 'Jms is associated and should not be.')
    
!   The original minI and minJ are assumed to be 1
!   The index of EASE grid  is starting from 0
    minI = minval(Imins,DIM=1)
    minJ = minval(Jmins,DIM=1)
    nx = count(Jmins==minJ)
    ny = count(Imins==minI)
    
    allocate(Ims(nx),Jms(ny), stat=STATUS)
    _VERIFY(STATUS)
    allocate(Im0(nx),Jm0(ny), stat=STATUS)
    _VERIFY(STATUS)
    
    nde = size(Imins)
    
    nx0 = 1
    ny0 = 1
    
    do k=1,nde
       if(Imins(k)==minI) then
          Jms(ny0) = Jmaxs(k)-Jmins(k) + 1
          Jm0(ny0) = Jmins(k)
          if(ny0==ny) then
             exit
          else
             ny0 = ny0 + 1
          end if
       end if
    end do
    
    do k=1,nde
       if(Jmins(k)==minJ) then
          Ims(nx0) = Imaxs(k)-Imins(k) + 1
          Im0(nx0) = Imins(k)
          if(nx0==nx) then
             exit
          else
             nx0 = nx0 + 1
          end if
       end if
    end do
    
    call MAPL_Sort(Im0,Ims)
    call MAPL_Sort(Jm0,Jms)
    
    deallocate(Im0,Jm0,stat=status)
    _VERIFY(STATUS)

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_GetImsJms

! ========================================
  recursive subroutine MAPL_StateAttSetI4(STATE, NAME, VALUE, RC)
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

    call ESMF_AttributeSet(STATE, NAME, VALUE, RC=status)
    _VERIFY(STATUS)

    call ESMF_StateGet(STATE,ITEMCOUNT=ITEMCOUNT,RC=STATUS)
    _VERIFY(STATUS)

    IF (ITEMCOUNT>0) then
       allocate(ITEMNAMES(ITEMCOUNT),STAT=STATUS)
       _VERIFY(STATUS)
       allocate(ITEMTYPES(ITEMCOUNT),STAT=STATUS)
       _VERIFY(STATUS)
       call ESMF_StateGet(STATE, ITEMNAMELIST=ITEMNAMES, &
            ITEMTYPELIST=ITEMTYPES, RC=STATUS)
       _VERIFY(STATUS)

       do I = 1, ITEMCOUNT
          if(itemtypes(I)==ESMF_StateItem_State) then
             call ESMF_StateGet(STATE, itemNames(I), nestedState, RC=STATUS)
             _VERIFY(STATUS)
             call MAPL_AttributeSet(nestedState, NAME, VALUE, RC=status)
             _VERIFY(STATUS)
          else if(itemtypes(I)==ESMF_StateItem_FieldBundle) then
             call ESMF_StateGet(STATE, itemNames(I), BUNDLE, RC=STATUS)
             _VERIFY(STATUS)
             call MAPL_AttributeSet(BUNDLE, NAME, VALUE, RC=status)
             _VERIFY(STATUS)
          else if(itemtypes(I)==ESMF_StateItem_Field) then
             call ESMF_StateGet(STATE, itemNames(I), FIELD, RC=STATUS)
             _VERIFY(STATUS)
             call MAPL_AttributeSet(FIELD, NAME, VALUE, RC=status)
             _VERIFY(STATUS)
          end if
       end do

       deallocate(ITEMNAMES)
       deallocate(ITEMTYPES)
    end if

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_StateAttSetI4

! ========================================
  subroutine MAPL_BundleAttSetI4(BUNDLE, NAME, VALUE, RC)
    type(ESMF_FieldBundle),           intent(INOUT) :: BUNDLE
    character(len=*),                 intent(IN   ) :: NAME
    integer,                          intent(IN   ) :: VALUE
    integer, optional,                intent(  OUT) :: RC

    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_BundleAttSet"
    integer                               :: STATUS

    type(ESMF_Field)                      :: FIELD
    integer                               :: FIELDCOUNT
    integer                               :: I

    call ESMF_AttributeSet(BUNDLE, NAME, VALUE, RC=status)
    _VERIFY(STATUS)

    call ESMF_FieldBundleGet(BUNDLE, FieldCount=FIELDCOUNT, RC=STATUS)
    _VERIFY(STATUS)

    do I = 1, FIELDCOUNT
       call ESMF_FieldBundleGet(BUNDLE, I, FIELD, RC=STATUS)
       _VERIFY(STATUS)
       call ESMF_AttributeSet(FIELD, NAME, VALUE, RC=status)
       _VERIFY(STATUS)
    end do

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_BundleAttSetI4

! ========================================
  subroutine MAPL_FieldAttSetI4(FIELD, NAME, VALUE, RC)
    type(ESMF_Field),                 intent(INOUT) :: FIELD
    character(len=*),                 intent(IN   ) :: NAME
    integer,                          intent(IN   ) :: VALUE
    integer, optional,                intent(  OUT) :: RC

    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_FieldAttSet"
    integer                               :: STATUS

    type(ESMF_Array)                        :: array
    type(ESMF_FieldStatus_Flag)             :: fieldStatus


    call ESMF_AttributeSet(FIELD, NAME, VALUE, RC=status)
    _VERIFY(STATUS)

    call ESMF_FieldGet(field, status=fieldStatus, rc=status)
    _VERIFY(STATUS)

    if(fieldStatus == ESMF_FIELDSTATUS_COMPLETE) then
       call ESMF_FieldGet(field, Array=array, rc=status)
       _VERIFY(STATUS)
       call ESMF_AttributeSet(array, NAME, VALUE, RC=status)
       _VERIFY(STATUS)
    end if

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_FieldAttSetI4
! ========================================

  subroutine MAPL_FieldDestroy(Field,RC)
    type(ESMF_Field),          intent(INOUT) :: Field
    integer, optional,         intent(OUT  ) :: RC

    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_FieldDestroy"
    integer                               :: STATUS

    real(kind=ESMF_KIND_R4), pointer      :: VAR_1D(:), VAR_2D(:,:), VAR_3D(:,:,:)
    real(kind=ESMF_KIND_R8), pointer      :: VR8_1D(:), VR8_2D(:,:), VR8_3D(:,:,:) 
    integer                      :: rank
    type(ESMF_TypeKind_Flag)     :: tk

    call ESMF_FieldGet(Field,typekind=tk,dimCount=rank,rc=status)
    _VERIFY(STATUS)
    if (tk == ESMF_TYPEKIND_R4 .and. rank == 1) then
       call ESMF_FieldGet(Field,0,VAR_1d,rc=status)
       _VERIFY(STATUS)
       deallocate(Var_1d,stat=status)
       _VERIFY(STATUS)
     else if (tk == ESMF_TYPEKIND_R8 .and. rank == 1) then
       call ESMF_FieldGet(Field,0,VR8_1d,rc=status)
       _VERIFY(STATUS)
       deallocate(VR8_1d,stat=status)
       _VERIFY(STATUS)
     else if (tk == ESMF_TYPEKIND_R4 .and. rank == 2) then
       call ESMF_FieldGet(Field,0,VAR_2d,rc=status)
       _VERIFY(STATUS)
       deallocate(Var_2d,stat=status)
       _VERIFY(STATUS)
     else if (tk == ESMF_TYPEKIND_R8 .and. rank == 2) then
       call ESMF_FieldGet(Field,0,VR8_2d,rc=status)
       _VERIFY(STATUS)
       deallocate(VR8_2d,stat=status)
       _VERIFY(STATUS)
     else if (tk == ESMF_TYPEKIND_R4 .and. rank == 3) then
       call ESMF_FieldGet(Field,0,VAR_3D,rc=status)
       _VERIFY(STATUS)
       deallocate(Var_3d,stat=status)
       _VERIFY(STATUS)
     else if (tk == ESMF_TYPEKIND_R8 .and. rank == 3) then
       call ESMF_FieldGet(Field,0,VR8_3D,rc=status)
       _VERIFY(STATUS)
       deallocate(VR8_3d,stat=status)
       _VERIFY(STATUS)
     else
       _ASSERT(.false., 'unsupported typekind+rank')
     end if
     call ESMF_FieldDestroy(Field,rc=status)
     _VERIFY(STATUS)
 
     _RETURN(ESMF_SUCCESS)

  end subroutine MAPL_FieldDestroy
         
  subroutine MAPL_FieldBundleDestroy(Bundle,RC)
    type(ESMF_FieldBundle),    intent(INOUT) :: Bundle
    integer, optional,         intent(OUT  ) :: RC

    integer                               :: I
    integer                               :: FieldCount
    type(ESMF_Field)                      :: Field
    logical                               :: isCreated

    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_FieldBundleDestroy"
    integer                               :: STATUS

 
    isCreated = ESMF_FieldBundleIsCreated(bundle,rc=status)
    _VERIFY(STATUS) 
    if(isCreated) then
       call ESMF_FieldBundleGet(BUNDLE, FieldCount=FIELDCOUNT, RC=STATUS)
       _VERIFY(STATUS)

       do I = 1, FIELDCOUNT
          call ESMF_FieldBundleGet(BUNDLE, I, FIELD, RC=STATUS)
          _VERIFY(STATUS)
          call MAPL_FieldDestroy(FIELD, RC=status)
          _VERIFY(STATUS)
       end do
    end if

    _RETURN(ESMF_SUCCESS)

  end subroutine MAPL_FieldBundleDestroy
         
  subroutine MAPL_StateAddField(State, Field, RC)
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
    call ESMF_StateAdd(state, fields, RC=status)
    _VERIFY(STATUS)
!=================
!!!ALT Example to add one field at the time (not used anymore)
!!!      call ESMF_StateAdd(STATE, FIELD, proxyflag=.false., &
!!!           addflag=.true., replaceflag=.false., RC=STATUS )
!=================

! check for attribute

    call ESMF_AttributeGet(state, NAME=attrName, isPresent=haveAttr, RC=STATUS)
    _VERIFY(STATUS)
    if (haveAttr) then
       call ESMF_AttributeGet(state, NAME=attrName, itemcount=natt, RC=STATUS)
       _VERIFY(STATUS)
    else
       natt = 0
    end if
    allocate(currList(natt), stat=status)
    _VERIFY(STATUS)

    if (natt > 0) then
       ! get the current list
       call ESMF_AttributeGet(state, NAME=attrName, VALUELIST=currList, rc=status)
       _VERIFY(STATUS)
!ALT delete/destroy this attribute to prevent memory leaks
       call ESMF_AttributeRemove(state, NAME=attrName, rc=status)
       _VERIFY(STATUS)
    end if

    na = natt+1
    allocate(thisList(na), stat=status)
    _VERIFY(STATUS)

    thisList(1:natt) = currList

    call ESMF_FieldGet(field, name=name, rc=status)
    _VERIFY(STATUS)

    thisList(na) = name

    call ESMF_AttributeSet(state, NAME=attrName, itemcount=na, VALUELIST=thisList, rc=status)
    _VERIFY(STATUS)

    deallocate(thisList)
    deallocate(currList)

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_StateAddField

  subroutine MAPL_StateAddBundle(State, Bundle, RC)
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
    call ESMF_StateAdd(state, Bundles, RC=status)
    _VERIFY(STATUS)

! check for attribute

    call ESMF_AttributeGet(state, NAME=attrName, isPresent=haveAttr, RC=STATUS)
    _VERIFY(STATUS)
    if (haveAttr) then
       call ESMF_AttributeGet(state, NAME=attrName, itemcount=natt, RC=STATUS)
       _VERIFY(STATUS)
    else
       natt = 0
    end if
    allocate(currList(natt), stat=status)
    _VERIFY(STATUS)

    if (natt > 0) then
       ! get the current list
       call ESMF_AttributeGet(state, NAME=attrName, VALUELIST=currList, rc=status)
       _VERIFY(STATUS)
!ALT delete/destroy this attribute to prevent memory leaks
       call ESMF_AttributeRemove(state, NAME=attrName, rc=status)
       _VERIFY(STATUS)
    end if

    na = natt+1
    allocate(thisList(na), stat=status)
    _VERIFY(STATUS)

    thisList(1:natt) = currList

    call ESMF_FieldBundleGet(bundle, name=name, rc=status)
    _VERIFY(STATUS)

    thisList(na) = name

    call ESMF_AttributeSet(state, NAME=attrName, itemcount=na, VALUELIST=thisList, rc=status)
    _VERIFY(STATUS)

    deallocate(thisList)
    deallocate(currList)

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_StateAddBundle

  subroutine MAPL_FieldBundleAddField(Bundle, Field, multiflag, RC)
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
    call ESMF_FieldBundleAdd(Bundle, fields, multiflag=multiflag, RC=status)
    _VERIFY(STATUS)

! check for attribute

    call ESMF_AttributeGet(Bundle, NAME=attrName, isPresent=haveAttr, RC=STATUS)
    _VERIFY(STATUS)
    if (haveAttr) then
       call ESMF_AttributeGet(Bundle, NAME=attrName, itemcount=natt, RC=STATUS)
       _VERIFY(STATUS)
    else
       natt = 0
    end if
    allocate(currList(natt), stat=status)
    _VERIFY(STATUS)

    if (natt > 0) then
       ! get the current list
       call ESMF_AttributeGet(Bundle, NAME=attrName, VALUELIST=currList, rc=status)
       _VERIFY(STATUS)
!ALT delete/destroy this attribute to prevent memory leaks
       call ESMF_AttributeRemove(bundle, NAME=attrName, rc=status)
       _VERIFY(STATUS)
    end if

    na = natt+1
    allocate(thisList(na), stat=status)
    _VERIFY(STATUS)

    thisList(1:natt) = currList

    call ESMF_FieldGet(field, name=name, rc=status)
    _VERIFY(STATUS)

    thisList(na) = name

    call ESMF_AttributeSet(bundle, NAME=attrName, itemcount=na, VALUELIST=thisList, rc=status)
    _VERIFY(STATUS)

    deallocate(thisList)
    deallocate(currList)

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_FieldBundleAddField

  subroutine MAPL_FieldBundleGetByIndex(Bundle, fieldIndex, Field, RC)
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

    call ESMF_AttributeGet(Bundle, NAME=attrName, itemcount=natt, RC=STATUS)
    _VERIFY(STATUS)
    allocate(currList(natt), stat=status)
    _VERIFY(STATUS)

    ! get the current list
    call ESMF_AttributeGet(Bundle, NAME=attrName, VALUELIST=currList, rc=status)
    _VERIFY(STATUS)

    name = currList(fieldIndex)
    call ESMF_FieldBundleGet(Bundle, fieldName = name, field=field, rc=status)
    _VERIFY(STATUS)

    deallocate(currList)

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_FieldBundleGetByIndex

!BOPI
!  !IROUTINE: MAPL_GetHorzIJIndex -- Get indexes on destributed ESMF grid for an arbitary lat and lon

!  !INTERFACE:
  subroutine MAPL_GetHorzIJIndex(npts,II,JJ,lon,lat,lonR8,latR8,Grid,IMGlob,JMGlob, &
          & EdgeLons, EdgeLats, rc)
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
     integer,            optional, intent(in   ) :: IMGlob
     integer,            optional, intent(in   ) :: JMGlob
     real(ESMF_KIND_R8), optional, intent(inout) :: EdgeLons(:,:,:) ! array of longitudes in radians on _all_ 6 faces
     real(ESMF_KIND_R8), optional, intent(inout) :: EdgeLats(:,:,:) ! array of latitudes in radians on _all_ 6 faces
     integer,            optional, intent(out  ) :: rc  ! return code
  
     !DESCRIPTION
!    For a set of longitudes and latitudes in radians this routine will return the indexes for the domain
!    Depending on how it is invoked these will be the local domain or the global indices. 
!    If the Lat/Lon pair is not in the domain -1 is returned. 
!    The routine works for both the gmao cube and lat/lon grids.
!    Currently the lat/lon grid is asumed to go from -180 to 180
     !EOPI

     integer                    :: status

     integer                         :: IM_World, JM_World, dims(3)
     integer                         :: IM, JM, counts(3)
     real(ESMF_KIND_R8), pointer     :: lons(:,:) => null()
     real(ESMF_KIND_R8), pointer     :: lats(:,:) => null()
     real(ESMF_KIND_R8), allocatable :: lons_1d(:)
     real(ESMF_KIND_R8), allocatable :: lats_1d(:)
     real(ESMF_KIND_R8), allocatable :: elons(:)
     real(ESMF_KIND_R8), allocatable :: elats(:)
     real(ESMF_KIND_R8), allocatable :: ex(:)
     real(ESMF_KIND_R8), allocatable :: ey(:)
     real(ESMF_KIND_R8), allocatable :: EdgeX(:,:)
     real(ESMF_KIND_R8), allocatable :: EdgeY(:,:)
     real(ESMF_KIND_R8), allocatable, save :: EdgeLats_(:,:,:)
     real(ESMF_KIND_R8), allocatable, save :: EdgeLons_(:,:,:)
     real                    :: lonloc,latloc,x_loc,y_loc
     logical                 :: isCubed, switch
     integer                 :: face_pnt,face,imp1,jmp1,itmp
     integer                 :: im_1d, jm_1d, IIloc, JJloc, i, j
     real(kind=REAL64), parameter :: PI_R8     = 3.14159265358979323846_REAL64
     logical                 :: localSearch
     integer                 :: fStart,fEnd
     logical, save           :: computeEdges = .true.


     ! if the grid is present then we can just get the prestored edges and the dimensions of the grid
     ! this also means we are running on a distributed grid
     ! if grid not present then the we just be running outside of ESMF and the user must
     ! pass in the the dimensions of the grid and we must compute them 
     ! and assume search on the global domain
     if (present(Grid)) then
        call MAPL_GridGet(grid, localCellCountPerDim=counts,globalCellCountPerDim=dims,rc=status)
        _VERIFY(STATUS)
        IM_World = dims(1)
        JM_World = dims(2)
        IM = counts(1)
        JM = counts(2)
        localSearch = .true.
     else
        _ASSERT(present(IMGlob), 'IMGlob must be present if Grid is present')
        _ASSERT(present(JMGlob), 'JMGlob must be present if Grid is present')
        IM_World = IMGlob
        JM_World = JMGlob
        IM = IM_World
        JM = IM_World
        localSearch = .false.
     end if   

     if (JM_World == 6*IM_World) then
        isCubed = .true.
     else
        isCubed = .false.
     end if
     ii = -1
     jj = -1
     if (isCubed) then
        if (localSearch) then
           if (computeEdges) then
              allocate(EdgeLats_(IM+1,JM+1,1),stat=status)
              _VERIFY(STATUS)
              allocate(EdgeLons_(IM+1,JM+1,1),stat=status)
              _VERIFY(STATUS)
              call MAPL_GridGetCorners(Grid,EdgeLons_(:,:,1),EdgeLats_(:,:,1),rc=status)
              _VERIFY(STATUS)
              computeEdges = .false.
           end if
        else
           _ASSERT(present(EdgeLats) .and. present(EdgeLons), 'inconsistent use of optional arguments')
           EdgeLats_ = EdgeLats
           EdgeLons_ = EdgeLons
        end if
        allocate(EdgeX(IM+1,JM+1),stat=status)
        _VERIFY(STATUS)
        allocate(EdgeY(IM+1,JM+1),stat=status)
        _VERIFY(STATUS)

        if (localSearch) then
           fStart = 1
           fEnd   = 1
        else
           fStart = 1
           fEnd   = 6
        end if

        do j = fStart, fEnd
           if (localSearch) then
              call check_face(IM+1,JM+1,EdgeLons_(:,:,1),EdgeLats_(:,:,1),FACE)
              _ASSERT(FACE > 0 .and. FACE <= 6, 'illegal grid face (must be in [1..6])')
              call cube_xy(IM+1,JM+1,EdgeX,EdgeY,EdgeLons_(:,:,j),EdgeLats_(:,:,j),face)
           else
              call check_face(IM+1,JM+1,EdgeLons_(:,:,j),EdgeLats_(:,:,j),FACE)
              _ASSERT(FACE > 0 .and. FACE <= 6, 'illegal grid face (must be in [1..6])')
              call cube_xy(IM+1,JM+1,EdgeX,EdgeY,EdgeLons_(:,:,j),EdgeLats_(:,:,j),face)
           end if
           switch = .false.
           if (abs(EdgeX(1,1)-EdgeX(2,1)) < 0.0001) switch = .true.

           if (switch) then
              allocate(ex(jm+1),ey(im+1), stat = status)
              _VERIFY(STATUS)
              imp1=im+1
              jmp1=jm+1
              im_1d = jm+1
              jm_1d = im+1
           else
              allocate(ex(im+1),ey(jm+1), stat=status)
              _VERIFY(status)
              imp1=im+1
              jmp1=jm+1
              im_1d = im+1
              jm_1d = jm+1
           end if
           call flatten_xy(EdgeX,EdgeY,ex,ey,imp1,jmp1,im_1d,jm_1d,switch)
           do i=1,npts
              ! CS grid 0 to 360 shift if we must
              ! if this is a global search and we have not found point, search this face
              ! otherwise always do search
              if ( (.not.localSearch .and. II(i) <0 .and. JJ(i) < 0) .or. localSearch) then
                 if (present(lon) .and. present(lat)) then
                    lonloc = lon(i)
                    latloc = lat(i)
                 else if (present(lonR8) .and. present(latR8)) then
                    lonloc = lonR8(i)
                    latloc = latR8(i)
                 end if
                 if (lonloc < 0.) lonloc = lonloc + 2.0*MAPL_PI
                 call check_face_pnt(lonloc,latloc,face_pnt)
                 if (face_pnt == face) then
                    call cube_xy_point(x_loc,y_loc,latloc,lonloc,face_pnt)
                    IIloc = ijsearch(ex,im_1d,x_loc,.false.)
                    JJloc = ijsearch(ey,jm_1d,y_loc,.false.)
                    if (switch) then
                       itmp = IIloc
                       IIloc = JJloc
                       JJloc = itmp
                    endif
                    if (.not.localSearch) JJloc = IM_World*(j-1)+JJloc
                 else
                    IIloc = -1
                    JJloc = -1
                 end if
                 II(i) = IIloc
                 JJ(i) = JJloc
              end if
           end do
           deallocate(ex)
           deallocate(ey)
        end do


        deallocate(EdgeY)
        deallocate(EdgeX)
     else
        if (localSearch) then
           call ESMF_GridGetCoord(grid,coordDim=1, localDe=0, &
              staggerloc=ESMF_STAGGERLOC_CENTER, fArrayPtr = lons, rc=status)
           _VERIFY(STATUS)
           call ESMF_GridGetCoord(grid,coordDim=2, localDe=0, &
              staggerloc=ESMF_STAGGERLOC_CENTER, fArrayPtr = lats, rc=status)
           _VERIFY(STATUS)
        else
           _ASSERT(.false.,'if not isCubed, localSearch must be .true.')
        end if
        allocate(lons_1d(im),stat=status)
        _VERIFY(STATUS)
        allocate(lats_1d(jm),stat=status)
        _VERIFY(STATUS)
        allocate(elons(im+1),stat=status)
        _VERIFY(STATUS)
        allocate(elats(jm+1),stat=status)
        _VERIFY(STATUS)
        lons_1d = lons(:,1)
        lats_1d = lats(1,:)
        call calc_edges_1d(elons,lons_1d,IM)
        call calc_edges_1d(elats,lats_1d,JM)
        ! lat-lon grid goes from -180 to 180 shift if we must
        ! BMA this -180 to 180 might change at some point
        do i=1,npts
           if (present(lon) .and. present(lat)) then
              lonloc = lon(i)
              latloc = lat(i)
           else if (present(lonR8) .and. present(latR8)) then
              lonloc = lonR8(i)
              latloc = latR8(i)
           end if
           if (lonloc > MAPL_PI) lonloc = lonloc - 2.0*MAPL_PI
           IIloc = ijsearch(elons,im+1,lonloc,.false.)
           JJloc = ijsearch(elats,jm+1,latloc,.false.)
           II(i) = IIloc
           JJ(i) = JJloc
        end do
        deallocate(lons_1d,lats_1d,elons,elats)
     end if

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
   end function

   subroutine calc_edges_1d(ecoords,coords,idim)
      integer, intent(in) :: idim
      real(ESMF_KIND_R8), intent(in)  :: coords(idim)
      real(ESMF_KIND_R8), intent(out) :: ecoords(idim+1)
      ecoords(1)  = coords(1) - 0.5 * ( coords(2) - coords(1) )
      ecoords(2:idim) = 0.5 * ( coords(1:idim-1)+coords(2:idim) )
      ecoords(idim+1) = coords(idim) + 0.5 * (coords(idim) - coords(idim-1))
      return
   end subroutine calc_edges_1d

      subroutine cube_xy(IM,JM,x,y,LONS,LATS,face)
      integer, intent(in) :: IM,JM
      real(ESMF_KIND_R8), intent(inout) :: x(:,:),y(:,:)
      real(ESMF_KIND_R8), intent(in) :: LATS(:,:),LONS(:,:)
      integer, intent(in) :: face

      real(ESMF_KIND_R8) :: rsq3
      real(ESMF_KIND_R8)  :: LAT,LON
      integer :: i,j
      rsq3 = 1.0/sqrt(3.0d0)
      do i=1,IM
       do j=1,JM
        LAT=LATS(I,J)
        LON=LONS(I,J)+MAPL_PI_R8/18.0d0
        select case(face)
         case (1)
          x(I,J) =  rsq3*tan(LON)
          y(I,J) =  rsq3*tan(LAT)/cos(LON)
         case (2)
          x(I,J) =  rsq3*tan(LON)
          y(I,J) = -rsq3*tan(LAT)/cos(LON)
         case (3)
          x(I,J) = -rsq3*cos(LON)/sin(LON)
          y(I,J) =  rsq3*tan(LAT)/sin(LON)
         case (4)
          x(I,J) = -rsq3*cos(LON)/sin(LON)
          y(I,J) = -rsq3*tan(LAT)/sin(LON)
         case (5)
          x(I,J) =  rsq3*sin(LON)*cos(LAT)/sin(LAT)
          y(I,J) = -rsq3*cos(LON)*cos(LAT)/sin(LAT)
         case (6)
          x(I,J) = -rsq3*sin(LON)*cos(LAT)/sin(LAT)
          y(I,J) = -rsq3*cos(LON)*cos(LAT)/sin(LAT)
        end select
       enddo
      enddo

      end subroutine cube_xy

      subroutine cube_xy_point(x,y,LAT,LON,face)
      real, intent(inout) :: x,y
      real, intent(in) :: LAT,LON
      integer, intent(in) :: face

      real :: rsq3,llon,llat
      rsq3 = 1.0/sqrt(3.0)
      LLAT=LAT
      LLON=LON+MAPL_PI/18.0
      select case(face)
       case (1)
        x =  rsq3*tan(LLON)
        y =  rsq3*tan(LLAT)/cos(LLON)
       case (2)
        x =  rsq3*tan(LLON)
        y = -rsq3*tan(LLAT)/cos(LLON)
       case (3)
        x = -rsq3*cos(LLON)/sin(LLON)
        y =  rsq3*tan(LLAT)/sin(LLON)
       case (4)
        x = -rsq3*cos(LLON)/sin(LLON)
        y = -rsq3*tan(LLAT)/sin(LLON)
       case (5)
        x =  rsq3*sin(LLON)*cos(LLAT)/sin(LLAT)
        y = -rsq3*cos(LLON)*cos(LLAT)/sin(LLAT)
       case (6)
        x = -rsq3*sin(LLON)*cos(LLAT)/sin(LLAT)
        y = -rsq3*cos(LLON)*cos(LLAT)/sin(LLAT)
      end select

      end subroutine cube_xy_point

      subroutine check_face(IM,JM,LONS,LATS,face)
      integer, intent(in) :: im,jm
      real(ESMF_KIND_R8), intent(in) :: LONS(:,:),LATS(:,:)
      integer, intent(inout) :: face
      real :: lon,lat
      integer :: i,j,k
      real :: s(6),smin, xyz(3), rsq3
      integer :: fmin,ifmin(6),imax,imaxt

      ifmin = 0
      rsq3=1.0/sqrt(3.)
      do i=1,IM
       do j=1,JM
        smin = 30.0
        lon=LONS(i,j)+MAPL_PI/18.0
        lat=LATS(i,j)
        xyz(1)=cos(lon)*cos(lat)
        xyz(2)=sin(lon)*cos(lat)
        xyz(3)=sin(lat)
        if (xyz(1) /= 0.0) then
         s(1)=rsq3/xyz(1)
         s(2)=-rsq3/xyz(1)
        else
         s(1)=1000.0
         s(2)=1000.0
        endif
        if (xyz(2) /= 0.0) then
         s(3)=rsq3/xyz(2)
         s(4)=-rsq3/xyz(2)
        else
         s(3)=1000.0
         s(4)=1000.0
        endif
        if (xyz(3) /= 0.0) then
         s(5)=rsq3/xyz(3)
         s(6)=-rsq3/xyz(3)
        else
         s(5)=1000.0
         s(6)=1000.0
        endif
        do k=1,6
         if (s(k) > 0) then
          if (s(k) < smin) then
           smin = s(k)
           fmin = k
          endif
         endif
        enddo
        ifmin(fmin) = ifmin(fmin)+1
       enddo
      enddo
      imax = 0
      do k=1,6
       imaxt=ifmin(k)
       if (imaxt > imax) then
        imax = imaxt
        face = k
       endif
      enddo
      end subroutine check_face

      subroutine flatten_xy(x,y,x_1d,y_1d,im,jm,im_1d,jm_1d,switch)
      implicit none
      integer, intent(in) :: im,jm, im_1d, jm_1d
      real(ESMF_KIND_R8), intent(in) :: x(:,:), y(:,:)
      real(ESMF_KIND_R8), intent(inout) :: y_1d(:), x_1d(:)
      logical, intent(in) :: switch
      integer :: i
      if (.not.switch) then
       do i =1,im_1d
        x_1d(i)=x(i,1)
       enddo
       do i =1,jm_1d
        y_1d(i)=y(1,i)
       enddo
      else if (switch) then
       do i =1,im_1d
        x_1d(i)=x(1,i)
       enddo
       do i =1,jm_1d
        y_1d(i)=y(i,1)
       enddo
      endif
      return
      end subroutine flatten_xy

      subroutine check_face_pnt(LON,LAT,face)
      real, intent(in) :: LON,LAT
      integer, intent(inout) :: face
      real :: llon,llat
      integer :: k
      real :: s(6),smin, xyz(3), rsq3
      integer :: fmin
      character(len=ESMF_MAXSTR) :: Iam
      Iam = 'check_face_pnt'

      rsq3=1.0/sqrt(3.)
      smin = 30.0
      llon=lon+MAPL_PI/18.0
      llat=lat
      xyz(1)=cos(llon)*cos(llat)
      xyz(2)=sin(llon)*cos(llat)
      xyz(3)=sin(llat)
      fmin = 7
      if (xyz(1) /= 0.) then
       s(1)=rsq3/xyz(1)
       s(2)=-rsq3/xyz(1)
      else
       s(1)=1000.
       s(2)=1000.
      endif
      if (xyz(2) /= 0.) then
       s(3)=rsq3/xyz(2)
       s(4)=-rsq3/xyz(2)
      else
       s(3)=1000.
       s(4)=1000.
      endif
      if (xyz(3) /= 0.) then
       s(5)=rsq3/xyz(3)
       s(6)=-rsq3/xyz(3)
      else
       s(5)=1000.
       s(6)=1000.
      endif
      do k=1,6
       if (s(k) > 0) then
        if (s(k) < smin) then
         smin = s(k)
         fmin = k
        endif
       endif
      enddo
      if (fmin /= 7) then
       face = fmin
      endif  
      end subroutine check_face_pnt
         
  end subroutine MAPL_GetHorzIJIndex

  subroutine MAPL_GenGridName(im, jm, lon, lat, xyoffset, gridname, geos_style)
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

  function MAPL_GenXYOffset(lon, lat) result(xy)
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

  subroutine MAPL_GeosNameNew(name)
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
  function MAPL_BundleCreate(name,grid,fieldNames,is2D,isEdge,long_names,units,rc) result(B)
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

  allocate(localIs2D(size(fieldNames)),stat=status)
  _VERIFY(STATUS)
  if (present(is2D)) then
     _ASSERT(size(fieldNames) == size(is2D),'inconsistent size of is2D array')
     localIs2D = is2D
  else
     localIs2D = .false. 
  end if
  allocate(localIsEdge(size(fieldNames)),stat=status)
  _VERIFY(STATUS)
  if (present(isEdge)) then
     _ASSERT(size(fieldNames) == size(isEdge), 'inconsistent size of isEdge array')
     localIsEdge = isEdge
  else
     localIsEdge = .false. 
  end if
  if (present(long_names)) then
     _ASSERT(size(fieldNames) == size(long_names), 'inconsistent size of long_names array')
  end if
  if (present(units)) then
     _ASSERT(size(fieldNames) == size(units), 'inconsistent size of units array')
  end if

  B = ESMF_FieldBundleCreate ( name=name, rc=STATUS )
  _VERIFY(STATUS)
  call ESMF_FieldBundleSet ( B, grid=GRID, rc=STATUS )
  _VERIFY(STATUS)
  call MAPL_GridGet(GRID, globalCellCountPerDim=COUNTS, &
       localCellCountPerDim=DIMS, RC=STATUS)
  _VERIFY(STATUS)
  do i=1,size(fieldnames)
     if (localIs2D(i)) then

        allocate(PTR2(DIMS(1),DIMS(2)),stat=STATUS)
        _VERIFY(STATUS)
        PTR2  = 0.0
        call ESMF_GridGet(GRID, dimCount=gridRank, rc=status)
        _VERIFY(STATUS)
        allocate(gridToFieldMap(gridRank), stat=status)
        _VERIFY(STATUS)
        if(gridRank == 2) then
           gridToFieldMap(1) = 1
           gridToFieldMap(2) = 2
        else if (gridRank == 3) then
           gridToFieldMap(1) = 1
           gridToFieldMap(2) = 2
           gridToFieldMap(3) = 0
        else
           _RETURN(ESMF_FAILURE)
        end if
        FIELD = ESMF_FieldCreate(grid=GRID, &
                datacopyFlag = ESMF_DATACOPY_REFERENCE,   &
                farrayPtr=PTR2, gridToFieldMap=gridToFieldMap, &
                name=fieldNames(i), RC=STATUS)
        _VERIFY(STATUS)
        deallocate(gridToFieldMap)
        call ESMF_AttributeSet(FIELD, NAME='DIMS', VALUE=MAPL_DimsHorzOnly, RC=STATUS)
        _VERIFY(STATUS)
        call ESMF_AttributeSet(FIELD, NAME='VLOCATION', VALUE=MAPL_VLocationNone, RC=STATUS)
        _VERIFY(STATUS)

     else
        if (localIsEdge(i)) then
           allocate(PTR3(Dims(1),Dims(2),0:counts(3)),stat=status)
           _VERIFY(STATUS)
        else
           allocate(PTR3(Dims(1),Dims(2),counts(3)),stat=status)
           _VERIFY(STATUS)
        end if
        PTR3 = 0.0
        FIELD = ESMF_FieldCreate(grid=GRID, &
                datacopyFlag = ESMF_DATACOPY_REFERENCE,   &
                farrayPtr=PTR3, name=fieldNames(i), RC=STATUS)
        call ESMF_AttributeSet(FIELD, NAME='DIMS', VALUE=MAPL_DimsHorzVert, RC=STATUS)
        _VERIFY(STATUS)
        if (localIsEdge(i)) then
              call ESMF_AttributeSet(FIELD, NAME='VLOCATION', VALUE=MAPL_VLocationEdge, RC=STATUS)
              _VERIFY(STATUS)
           else
              call ESMF_AttributeSet(FIELD, NAME='VLOCATION', VALUE=MAPL_VLocationCenter, RC=STATUS)
              _VERIFY(STATUS)
        end if

     end if
     if (present(long_names)) then
        call ESMF_AttributeSet(FIELD, NAME='LONG_NAME', VALUE=long_names(i), RC=STATUS)
        _VERIFY(STATUS)
     else
        call ESMF_AttributeSet(FIELD, NAME='LONG_NAME', VALUE="UNKNOWN", RC=STATUS)
        _VERIFY(STATUS)
     end if
     if (present(units)) then
        call ESMF_AttributeSet(FIELD, NAME='LONG_NAME', VALUE=units(i), RC=STATUS)
        _VERIFY(STATUS)
     else
        call ESMF_AttributeSet(FIELD, NAME='LONG_NAME', VALUE="UNKNOWN", RC=STATUS)
        _VERIFY(STATUS)
     end if
     call MAPL_FieldBundleAdd(B, FIELD, RC=STATUS)
     _VERIFY(STATUS)
  enddo
 
  deallocate(localIs2D)
  deallocate(localIsEdge)
  _RETURN(ESMF_SUCCESS)

  end function MAPL_BundleCreate

  subroutine MAPL_DistGridGet(distGrid,minIndex,maxIndex,rc)

     type(ESMF_DistGrid), intent(inout) :: distGrid
     integer,             intent(inout) :: MinIndex(:,:)
     integer,             intent(inout) :: MaxIndex(:,:)
     integer, optional,   intent(out  ) :: rc

     integer :: status
     character(len=ESMF_MAXSTR) :: Iam

     integer :: i,tileSize,tileCount,tile,deCount
     logical :: ESMFCubeSphere
     integer, allocatable :: elementCountPTile(:)
     integer, allocatable :: deToTileMap(:)
     integer, allocatable :: oldMinIndex(:,:),oldMaxIndex(:,:)

     Iam = "MAPL_DistGridGet"

     ESMFCubeSphere = .false.

     call ESMF_DistGridGet(distGrid,tileCount=tileCount,rc=status)
     _VERIFY(STATUS)

     if (tileCount==6) ESMFCubeSphere = .true.

     if (ESMFCubeSphere) then
        allocate(elementCountPTile(tileCount),stat=status)
        _VERIFY(STATUS)
        call ESMF_DistGridGet(distGrid,elementCountPTile=elementCountPTile,rc=status)
        _VERIFY(STATUS)
        ! All tile should have same number of elements
        tileSize = elementCountPTile(1)
        tileSize = SQRT(real(tileSize))
        deallocate(elementCountPTile)

        deCount = size(minIndex,2)

        allocate(deToTileMap(deCount),stat=status)
        _VERIFY(STATUS)
        allocate(oldMinIndex(2,deCount),oldMaxIndex(2,deCount),stat=status)
        _VERIFY(STATUS)
        call ESMF_DistGridGet(distGrid,MaxIndexPDe=oldMaxIndex,MinIndexPDe=oldMinIndex, &
                              deToTileMap=deToTileMap,rc=status)
        _VERIFY(STATUS)
        do i=1,deCount
           tile = deToTileMap(i)
           select case (tile)
              case (1)
                 minIndex(:,i)=oldMinIndex(:,i)
                 maxIndex(:,i)=oldMaxIndex(:,i)
              case (2)
                 minIndex(1,i)=oldMinIndex(1,i) -  tileSize
                 minIndex(2,i)=oldMinIndex(2,i) +  tileSize
                 maxIndex(1,i)=oldMaxIndex(1,i) -  tileSize
                 maxIndex(2,i)=oldMaxIndex(2,i) +  tileSize
              case (3)
                 minIndex(1,i)=oldMinIndex(1,i) -  tileSize
                 minIndex(2,i)=oldMinIndex(2,i) +  tileSize
                 maxIndex(1,i)=oldMaxIndex(1,i) -  tileSize
                 maxIndex(2,i)=oldMaxIndex(2,i) +  tileSize
              case (4)
                 minIndex(1,i)=oldMinIndex(1,i) -2*tileSize
                 minIndex(2,i)=oldMinIndex(2,i) +2*tileSize
                 maxIndex(1,i)=oldMaxIndex(1,i) -2*tileSize
                 maxIndex(2,i)=oldMaxIndex(2,i) +2*tileSize
              case (5)
                 minIndex(1,i)=oldMinIndex(1,i) -2*tileSize
                 minIndex(2,i)=oldMinIndex(2,i) +2*tileSize
                 maxIndex(1,i)=oldMaxIndex(1,i) -2*tileSize
                 maxIndex(2,i)=oldMaxIndex(2,i) +2*tileSize
              case (6)
                 minIndex(1,i)=oldMinIndex(1,i) -3*tileSize
                 minIndex(2,i)=oldMinIndex(2,i) +3*tileSize
                 maxIndex(1,i)=oldMaxIndex(1,i) -3*tileSize
                 maxIndex(2,i)=oldMaxIndex(2,i) +3*tileSize
           end select
        enddo
        deallocate(deToTileMap)
        deallocate(oldMaxIndex,oldMinIndex)

     else

        call ESMF_DistGridGet(distGrid,minIndexPDe=minIndex,maxIndexPDe=maxIndex,rc=status)
        _VERIFY(STATUS)

     end if

 end subroutine MAPL_DistGridGet

 function MAPL_TrimString(istring,rc) result(ostring)
    character(len=*), intent(in) :: istring
    integer, optional, intent(out) :: rc

    character(len=:), allocatable :: ostring
    integer :: strlen
    integer :: status

    strlen = len_trim(istring)
    if (istring(strlen:strlen)==char(0)) then
       allocate(ostring,source=istring(1:strlen-1),stat=status)
       _VERIFY(status)
    else
       allocate(ostring,source=istring(1:strlen),stat=status)
       _VERIFY(status)
    end if
    _RETURN(_SUCCESS)
 end function MAPL_TrimString

 subroutine MAPL_FieldSplit(field, fields, rc)
   type(ESMF_Field),          intent(IN   ) :: field
   type(ESMF_Field), pointer, intent(  out) :: fields(:)
   integer, optional,         intent(  out) :: rc

   ! local vars
   integer :: status
   integer :: k, n
   integer :: gridRank

   logical                    :: has_ungrd
   integer                    :: ungrd_cnt
   integer, allocatable       :: gridToFieldMap(:)
   integer, allocatable       :: ungrd(:)
   real, pointer              :: ptr4d(:,:,:,:) => null()
   real, pointer              :: ptr3d(:,:,:) => null()
   type(ESMF_Field)           :: f, fld
   type(ESMF_Grid)            :: grid
   type(ESMF_TypeKind_Flag)   :: tk
   character(len=ESMF_MAXSTR) :: name
   character(len=ESMF_MAXSTR) :: splitName

   ! get ptr
   ! loop over 4-d dim
   ! create 3d field
   ! put in state/bundle
   ! end-of-loop
   call ESMF_FieldGet(field, name=name, grid=grid, typekind=tk, rc=status)
   _VERIFY(STATUS)

   call ESMF_GridGet(GRID, dimCount=gridRank, rc=status)
   _VERIFY(STATUS)
   allocate(gridToFieldMap(gridRank), stat=status)
   _VERIFY(STATUS)
   call ESMF_FieldGet(field, gridToFieldMap=gridToFieldMap, rc=status)
   _VERIFY(STATUS)

   if (tk == ESMF_TYPEKIND_R4) then
      !ALT: assumes 1 DE per PET
      call ESMF_FieldGet(Field,0,ptr4D,rc=status)
      _VERIFY(STATUS)
      n = size(ptr4d,4)
      allocate(fields(n), stat=status)
      _VERIFY(STATUS)
      n = 0
      do k=lbound(ptr4d,4),ubound(ptr4d,4)
         n = n+1
         ptr3d => ptr4d(:,:,:,k)
         ! create a new field
         write(splitName,'(A,I3.3)') trim(name) // '_', n
         f = MAPL_FieldCreateEmpty(name=splitName, grid=grid, rc=status)
         _VERIFY(STATUS)
         call ESMF_FieldEmptyComplete(F, farrayPtr=ptr3D,    &
              datacopyFlag = ESMF_DATACOPY_REFERENCE,             &
              gridToFieldMap=gridToFieldMap,                      &
              rc = status)
         _VERIFY(STATUS)
         ! copy attributes and adjust as necessary
         fld = field ! shallow copy to get around intent(in/out)
         call MAPL_FieldCopyAttributes(FIELD_IN=fld, FIELD_OUT=f, RC=status)
         _VERIFY(STATUS)

         ! adjust ungridded dims attribute (if any)
         call ESMF_AttributeGet(FIELD, NAME="UNGRIDDED_DIMS", isPresent=has_ungrd, RC=STATUS)
         _VERIFY(STATUS)
         if (has_ungrd) then
            call ESMF_AttributeGet(F, NAME='UNGRIDDED_DIMS', itemcount=UNGRD_CNT, RC=STATUS)
            _VERIFY(STATUS)
            allocate(ungrd(UNGRD_CNT), stat=status)
            _VERIFY(STATUS)
            call ESMF_AttributeGet(F, NAME='UNGRIDDED_DIMS', valueList=UNGRD, RC=STATUS)
            _VERIFY(STATUS)
            call ESMF_AttributeRemove(F, NAME='UNGRIDDED_DIMS', RC=STATUS)
            _VERIFY(STATUS)
            if (ungrd_cnt > 1) then
               ungrd_cnt = ungrd_cnt - 1
               call ESMF_AttributeGet(F, NAME='UNGRIDDED_DIMS', &
                    valueList=UNGRD(1:ungrd_cnt), RC=STATUS)
               _VERIFY(STATUS)
            else
               has_ungrd = .false.
            end if
            deallocate(ungrd)
         end if
            
         fields(n) = f
      end do
   else if (tk == ESMF_TYPEKIND_R8) then
      _ASSERT(.false., "R8 overload not implemented yet")
   end if

   deallocate(gridToFieldMap)
   ! fields SHOULD be deallocated by the caller!!!
   _RETURN(ESMF_SUCCESS)
 end subroutine MAPL_FieldSplit
end module MAPL_BaseMod

