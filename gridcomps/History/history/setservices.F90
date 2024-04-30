!------------------------------------------------------------------------------
!               Global Modeling and Assimilation Office (GMAO)                !
!                    Goddard Earth Observing System (GEOS)                    !
!                                 MAPL Component                              !
!------------------------------------------------------------------------------
!
#include "MAPL_Generic.h"
#include "unused_dummy.H"
!
!>
!### MODULE: `MAPL_HistoryGridCompMod`
!
! Author: GMAO SI-Team
!
! `MAPL_HistoryGridCompMod` contains the `Initialize`, `Run` and `Finalize` methods for `History`.
! The three methods are called at the level of CAP.
!
!=====================================================================
SUBMODULE (MAPL_HistoryGridCompMod) setservices_smod

  CONTAINS
!>
! Sets Initialize, Run and Finalize services for the `MAPL_HistoryGridComp` component.
!
  MODULE subroutine SetServices ( gc, rc )
    type(ESMF_GridComp), intent(inout) :: gc     !! composite gridded component
    integer, intent(out), optional     :: rc     !! return code

    integer                         :: status
    type (HISTORY_wrap)             :: wrap
    type (HISTORY_STATE), pointer   :: internal_state

! Register services for this component
! ------------------------------------

    call MAPL_GridCompSetEntryPoint ( gc, ESMF_METHOD_INITIALIZE, Initialize, _RC)

    call MAPL_GridCompSetEntryPoint ( gc, ESMF_METHOD_RUN,   Run,       _RC)

    call MAPL_GridCompSetEntryPoint ( gc, ESMF_METHOD_FINALIZE, Finalize,  _RC)

    call MAPL_GridCompSetEntryPoint ( gc, ESMF_METHOD_WRITERESTART, RecordRestart, _RC)

! Allocate an instance of the private internal state...
!------------------------------------------------------

    allocate(internal_state, _STAT)

! and save its pointer in the GC
!-------------------------------

    wrap%ptr => internal_state
    call ESMF_GridCompSetInternalState(gc, wrap, status)

! Generic Set Services
! --------------------
    call MAPL_GenericSetServices ( gc,_RC )

    _RETURN(ESMF_SUCCESS)

  end subroutine SetServices

END SUBMODULE

