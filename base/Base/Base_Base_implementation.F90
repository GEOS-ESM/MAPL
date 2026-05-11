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

   ! MAPL_Interp_Fac and MAPL_ClimInterpFac moved to MAPL_TimeInterpolation (base3g)

   ! A year is a leap year if
  ! 1) it is divible by 4, and
  ! 2) it is not divisible by 100, unless
  ! 3) it is also divisible by 400.
  logical function MAPL_LEAP(NY)
    integer, intent(in) :: NY

    MAPL_LEAP = mod(NY,4)==0 .and. (mod(NY,100)/=0 .or. mod(NY,400)==0)

  end function MAPL_LEAP


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

  ! ========================================

end submodule Base_Implementation
