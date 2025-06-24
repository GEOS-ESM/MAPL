

#include "MAPL_ErrLog.h"

!BOP

! !MODULE: MAPL_Profiler -- A Module to instrument codes for profiling


! !INTERFACE:

  module MAPL_ProfMod

! !USES:

  use ESMF
  use MAPL_ExceptionHandling
  implicit none
  private

! !PUBLIC TYPES:

! !PUBLIC MEMBER FUNCTIONS:
  public MAPL_ProfDisable
  public MAPL_ProfEnable
  public MAPL_ProfIsDisabled

!EOP

  logical,       save :: DISABLED  = .false.


  contains

!********************************************************
    logical function MAPL_ProfIsDisabled()
      !$omp master
      MAPL_ProfIsDisabled = DISABLED
      !$omp end master

    end function MAPL_ProfIsDisabled

!********************************************************

!********************************************************

    subroutine MAPL_ProfDisable(RC)
      integer, optional, intent(OUT)   :: RC

      character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_ProfDisable"

      !$omp master
      DISABLED = .true.
      !$omp end master

      _return(ESMF_SUCCESS)
      
    end subroutine MAPL_ProfDisable

!********************************************************

    subroutine MAPL_ProfEnable(RC)
      integer, optional, intent(OUT)   :: RC

      character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_ProfEnable"

      !$omp master
      DISABLED = .false.
      !$omp end master

      _return(ESMF_SUCCESS)
      
    end subroutine MAPL_ProfEnable

  end module MAPL_ProfMod

