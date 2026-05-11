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
     pinflag = ESMF_PIN_DE_TO_SSI_CONTIG

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
       call MAPL_FieldAttSetI4(field, NAME="MAPL_InitStatus", &
            VALUE=MAPL_InitialDefault, _RC)
    end if

    ! Clean up
    deallocate(haloWidth)
    deallocate(gridToFieldMap)


    _RETURN(ESMF_SUCCESS)
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

    call ESMF_FieldGet(field, status=fieldStatus, _RC)

    if (fieldStatus == ESMF_FIELDSTATUS_COMPLETE) then
       call ESMF_FieldGet(field, Array=array, _RC)

       call ESMF_ArrayGet(array, localDeCount=localDeCount, _RC)
       _ASSERT(localDeCount == 1, 'currently MAPL supports only 1 local array')
       call ESMF_ArrayGet(array, localarrayList=larrayList, _RC)
       larray => lArrayList(1) ! alias

       call ESMF_LocalArrayGet(larray, rank=rank, typekind=tk, &
            _RC)

       call ESMF_LocalArrayF90Deallocate(larray, typekind=tk, rank=rank, _RC)
    end if

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_FieldF90Deallocate

  pure subroutine MAPL_DecomposeDim ( dim_world,dim,NDEs, unusable, symmetric, min_DE_extent )
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

    pure logical function even(n)
      integer, intent(in) :: n
      even = mod(n,2).EQ.0
    end function even

    pure logical function odd(n)
      integer, intent(in) :: n
      odd = mod(n,2).EQ.1
    end function odd

  end subroutine MAPL_DecomposeDim

   ! MAPL_Interp_Fac and MAPL_ClimInterpFac moved to MAPL_TimeInterpolation (base3g)

   ! A year is a leap year if
  ! 1) it is divible by 4, and
  ! 2) it is not divisible by 100, unless
  ! 3) it is also divisible by 400.
  logical function MAPL_LEAP(NY)
    integer, intent(in) :: NY

    MAPL_LEAP = mod(NY,4)==0 .and. (mod(NY,100)/=0 .or. mod(NY,400)==0)

  end function MAPL_LEAP


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
    type(ESMF_Info)                       :: infoh
    type (ESMF_StateItem_Flag), pointer   :: ITEMTYPES(:)
    character(len=ESMF_MAXSTR ), pointer  :: ITEMNAMES(:)
    integer                               :: ITEMCOUNT
    integer                               :: I

    call ESMF_InfoGetFromHost(STATE,infoh,_RC)
    call ESMF_InfoSet(infoh,NAME,VALUE,_RC)

    call ESMF_StateGet(STATE,ITEMCOUNT=ITEMCOUNT,_RC)

    IF (ITEMCOUNT>0) then
       allocate(ITEMNAMES(ITEMCOUNT),_STAT)
       allocate(ITEMTYPES(ITEMCOUNT),_STAT)
       call ESMF_StateGet(STATE, ITEMNAMELIST=ITEMNAMES, &
            ITEMTYPELIST=ITEMTYPES, _RC)

       do I = 1, ITEMCOUNT
          if(itemtypes(I)==ESMF_StateItem_State) then
             call ESMF_StateGet(STATE, itemNames(I), nestedState, _RC)
             call MAPL_StateAttSetI4(nestedState, NAME, VALUE, _RC)
          else if(itemtypes(I)==ESMF_StateItem_FieldBundle) then
             call ESMF_StateGet(STATE, itemNames(I), BUNDLE, _RC)
             call MAPL_BundleAttSetI4(BUNDLE, NAME, VALUE, _RC)
          else if(itemtypes(I)==ESMF_StateItem_Field) then
             call ESMF_StateGet(STATE, itemNames(I), FIELD, _RC)
             call MAPL_FieldAttSetI4(FIELD, NAME, VALUE, _RC)
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
    type(ESMF_Info)                       :: infoh
    integer                               :: FIELDCOUNT
    integer                               :: I

    call ESMF_InfoGetFromHost(BUNDLE,infoh,_RC)
    call ESMF_InfoSet(infoh,NAME,VALUE,_RC)

    call ESMF_FieldBundleGet(BUNDLE, FieldCount=FIELDCOUNT, _RC)

    do I = 1, FIELDCOUNT
       call ESMF_FieldBundleGet(BUNDLE, I, FIELD, _RC)
       call ESMF_InfoGetFromHost(FIELD,infoh,_RC)
       call ESMF_InfoSet(infoh,NAME,VALUE,_RC)
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
    type(ESMF_Info)                         :: infoh


    call ESMF_InfoGetFromHost(FIELD,infoh,_RC)
    call ESMF_InfoSet(infoh,NAME,VALUE,_RC)

    call ESMF_FieldGet(field, status=fieldStatus, _RC)

    if(fieldStatus == ESMF_FIELDSTATUS_COMPLETE) then
       call ESMF_FieldGet(field, Array=array, _RC)
       call ESMF_InfoGetFromHost(array,infoh,_RC)
       call ESMF_InfoSet(infoh,NAME,VALUE,_RC)
    end if

     _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_FieldAttSetI4
  ! ========================================

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
