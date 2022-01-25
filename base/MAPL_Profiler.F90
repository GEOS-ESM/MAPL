

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
      MAPL_ProfIsDisabled = DISABLED

    end function MAPL_ProfIsDisabled

!********************************************************

!********************************************************

    subroutine MAPL_ProfDisable(RC)
      integer, optional, intent(OUT)   :: RC

      character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_ProfDisable"

      DISABLED = .true.

      _RETURN(ESMF_SUCCESS)
      
    end subroutine MAPL_ProfDisable

!********************************************************

    subroutine MAPL_ProfEnable(RC)
      integer, optional, intent(OUT)   :: RC

      character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_ProfEnable"

      DISABLED = .false.

      _RETURN(ESMF_SUCCESS)
      
    end subroutine MAPL_ProfEnable

  end module MAPL_ProfMod

