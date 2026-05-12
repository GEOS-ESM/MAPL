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

end module MAPL_Base

module MAPL_BaseMod
  use MAPL_Base
  use MAPL_RangeMod, only: MAPL_Range
  use mapl_MaplGrid, only: MAPL_GridGet, MAPL_DistGridGet, MAPL_GetImsJms, MAPL_GridHasDE
  use MAPL_Constants

end module MAPL_BaseMod
