!------------------------------------------------------------------------------
!               Global Modeling and Assimilation Office (GMAO)                !
!                    Goddard Earth Observing System (GEOS)                    !
!                                 MAPL Component                              !
!------------------------------------------------------------------------------
!
#include "MAPL_ErrLog.h"
#include "unused_dummy.H"
!
!>
!### MODULE: `MAPL_Base`
!
! Author: GMAO SI-Team
!
! The module `MAPL_Base` provides a collection assorted
! utilities and constants used throughout the MAPL Library.
!
module MAPL_Base

  ! !USES:
  !
  use ESMF, only: ESMF_MAXSTR
  use MAPL_Constants, only: MAPL_UNDEF
  use MAPL_TimeInterpolation, only: MAPL_Interp_Fac, MAPL_ClimInterpFac
  use, intrinsic :: iso_fortran_env, only: REAL64
  implicit NONE
  private

  public :: MAPL_UNDEF

  ! !PUBLIC MEMBER FUNCTIONS:
  !
  !public MAPL_FieldF90Deallocate
  public MAPL_ClimInterpFac       ! re-exported from MAPL_TimeInterpolation (base3g)
  !public MAPL_ConnectCoupling
   public MAPL_GRID_INTERIOR
   public MAPL_Interp_Fac   ! re-exported from MAPL_TimeInterpolation (base3g)
  public MAPL_GetHorzIJIndex
  public MAPL_GetGlobalHorzIJIndex
  public MAPL_Reverse_Schmidt
  public MAPL_GenGridName
   public MAPL_GridGetCorners
   public MAPL_GridGetInterior


  !----------------------------------------------------------------------

  ! Note: The routine below came from ESMFL; it has been moved here to
  !       avoid circular dependencies (Arlindo).
  interface  MAPL_GridGetInterior
    module procedure MAPL_Grid_Interior
  end interface 


  interface
     module subroutine MAPL_FieldF90Deallocate(field, rc)
       use ESMF, only: ESMF_Field
       type(ESMF_Field),  intent(INOUT) :: field
       integer, optional, intent(  OUT) :: rc
     end subroutine MAPL_FieldF90Deallocate

     module subroutine MAPL_PICKEM(II,JJ,IM,JM,COUNT)
       integer, intent(IN ) :: IM, JM, COUNT
       integer, intent(OUT) :: II(COUNT), JJ(COUNT)
     end subroutine MAPL_PICKEM



     !............................................................................

     module subroutine MAPL_GRID_INTERIOR(GRID,I1,IN,J1,JN)
       use ESMF, only: ESMF_Grid
       type (ESMF_Grid), intent(IN) :: grid
       integer, intent(OUT)         :: I1, IN, J1, JN
     end subroutine MAPL_GRID_INTERIOR

     module subroutine MAPL_GridGetCorners(grid,gridCornerLons, gridCornerLats, RC)
       use ESMF, only: ESMF_Grid, ESMF_KIND_R8
       type (ESMF_Grid), intent(INOUT) :: GRID
       real(ESMF_KIND_R8), intent(INOUT) :: gridCornerLons(:,:)
       real(ESMF_KIND_R8), intent(INOUT) :: gridCornerLats(:,:)
       integer, optional, intent(  OUT) :: RC

     end subroutine MAPL_GridGetCorners

     !BOPI
     !  !IROUTINE: MAPL_GetHorzIJIndex -- Get indexes on destributed ESMF grid for an arbitary lat and lon

     !  !INTERFACE:
     module subroutine MAPL_GetHorzIJIndex(npts,II,JJ,lon,lat,lonR8,latR8,Grid, rc)
       use ESMF, only: ESMF_KIND_R8, ESMF_GRid
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
     end subroutine MAPL_GetHorzIJIndex

     module subroutine MAPL_GetGlobalHorzIJIndex(npts,II,JJ,lon,lat,lonR8,latR8,Grid, rc)
       use ESMF, only: ESMF_KIND_R8, ESMF_GRid
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
     end subroutine MAPL_GetGlobalHorzIJIndex

     module subroutine MAPL_Reverse_Schmidt(Grid, stretched, npts,lon,lat,lonR8,latR8, lonRe, latRe, rc)
       use ESMF, only: ESMF_KIND_R8, ESMF_GRid
       implicit none
       !ARGUMENTS:
       type(ESMF_Grid),              intent(inout) :: Grid        ! ESMF grid
       logical,                      intent(out  ) :: stretched
       integer,                      intent(in   ) :: npts        ! number of points in lat and lon arrays
       real, optional,               intent(in   ) :: lon(npts)   ! array of longitudes in radians
       real, optional,               intent(in   ) :: lat(npts)   ! array of latitudes in radians
       real(ESMF_KIND_R8), optional, intent(in   ) :: lonR8(npts) ! array of longitudes in radians
       real(ESMF_KIND_R8), optional, intent(in   ) :: latR8(npts) ! array of latitudes in radians
       real(ESMF_KIND_R8), optional, intent(out  ) :: lonRe(npts) ! array of longitudes in radians
       real(ESMF_KIND_R8), optional, intent(out  ) :: latRe(npts) ! array of latitudes in radians
       integer,            optional, intent(out  ) :: rc  ! return code
     end subroutine MAPL_Reverse_Schmidt

     module subroutine MAPL_GenGridName(im, jm, lon, lat, xyoffset, gridname, geos_style)
       integer :: im, jm
       character (len=*) :: gridname
       real, optional    :: lon(:), lat(:)
       integer, optional :: xyoffset
       logical,  optional :: geos_style
     end subroutine MAPL_GenGridName

       module function MAPL_StrUpCase(str) result(new)
         character(len=*), intent(IN) :: str
         character(len=len(str))      :: new
       end function MAPL_StrUpCase

       module function MAPL_StrDnCase(str) result(new)
         character(len=*), intent(IN) :: str
         character(len=len(str))      :: new
       end function MAPL_StrDnCase

   end interface

contains

end module MAPL_Base

module MAPL_BaseMod
  use MAPL_Base
  use MAPL_RangeMod, only:   MAPL_Range
  use mapl_MaplGrid, only: MAPL_GridGet, MAPL_DistGridGet, MAPL_GetImsJms, MAPL_GridHasDE
  use MAPL_Constants




end module MAPL_BaseMod


