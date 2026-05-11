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

  use MAPL_Constants, only: MAPL_UNDEF
  use MAPL_ExceptionHandling
  use, intrinsic :: iso_fortran_env, only: REAL64
  implicit NONE
  private

  public :: MAPL_UNDEF
  public :: MAPL_GRID_INTERIOR
  public :: MAPL_GridGetInterior
  public :: MAPL_GridGetCorners

  interface MAPL_GridGetInterior
    module procedure MAPL_GRID_INTERIOR
  end interface

contains

  subroutine MAPL_GRID_INTERIOR(grid, i1, in, j1, jn)
    use ESMF, only: ESMF_Grid
    use mapl3g_GridGet, only: GridGet
    type(ESMF_Grid), intent(in) :: grid
    integer, intent(out) :: i1, in, j1, jn
    integer, allocatable :: interior(:)
    call GridGet(grid, interior=interior)
    i1=interior(1); in=interior(2); j1=interior(3); jn=interior(4)
  end subroutine MAPL_GRID_INTERIOR

  subroutine MAPL_GridGetCorners(grid, gridCornerLons, gridCornerLats, rc)
    use ESMF, only: ESMF_Grid, ESMF_KIND_R8
    use mapl3g_GridGet, only: GridGet
    type(ESMF_Grid), intent(inout) :: grid
    real(ESMF_KIND_R8), intent(inout) :: gridCornerLons(:,:)
    real(ESMF_KIND_R8), intent(inout) :: gridCornerLats(:,:)
    integer, optional, intent(out) :: rc
    integer :: status
    real(ESMF_KIND_R8), allocatable :: corners(:,:,:)
    call GridGet(grid, corners=corners, rc=status)
    _VERIFY(status)
    gridCornerLons = corners(:,:,1)
    gridCornerLats = corners(:,:,2)
    _RETURN(_SUCCESS)
  end subroutine MAPL_GridGetCorners

end module MAPL_Base

module MAPL_BaseMod
  use MAPL_Base
  use MAPL_RangeMod, only: MAPL_Range
  use mapl_MaplGrid, only: MAPL_GridGet, MAPL_DistGridGet, MAPL_GetImsJms, MAPL_GridHasDE
  use MAPL_Constants

end module MAPL_BaseMod
