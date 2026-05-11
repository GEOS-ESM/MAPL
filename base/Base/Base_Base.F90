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


