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
  SUBMODULE (MAPL_HistoryGridCompMod) record_restart_smod
!
! !USES:
!
  implicit none

contains


  MODULE SUBROUTINE RecordRestart( gc, import, export, clock, rc )

   intrinsic :: size
   intrinsic :: TRIM


! !ARGUMENTS:

    type(ESMF_GridComp), intent(inout)    :: gc     ! composite gridded component
    type(ESMF_State),       intent(inout) :: import ! import state
    type(ESMF_State),       intent(  out) :: export ! export state
    type(ESMF_Clock),       intent(inout) :: clock  ! the clock

    integer, intent(out), OPTIONAL        :: rc     ! Error code:
                                                     ! = 0 all is well
                                                     ! otherwise, error

    integer                         :: status


    character(len=14)                :: datestamp ! YYYYMMDD_HHMMz
    type(HistoryCollection), pointer :: list(:)
    type(HISTORY_wrap)               :: wrap
    type (HISTORY_STATE), pointer    :: IntState
    integer                          :: n, nlist
    logical                          :: doRecord
    character(len=ESMF_MAXSTR)       :: fname_saved, filename
    type (MAPL_MetaComp), pointer    :: meta

    _UNUSED_DUMMY(import)
    _UNUSED_DUMMY(export)
! Check if it is time to do anything
    doRecord = .false.

    call MAPL_InternalStateRetrieve(GC, meta, _RC)

    doRecord = MAPL_RecordAlarmIsRinging(meta, _RC)
    if (.not. doRecord) then
       _RETURN(ESMF_SUCCESS)
    end if

    call MAPL_DateStampGet(clock, datestamp, _RC)

! Retrieve the pointer to the state
    call ESMF_GridCompGetInternalState(gc, wrap, status)
    IntState => wrap%ptr
    list => IntState%list
    nlist = size(list)

    do n=1,nlist
       if(list(n)%monthly) then
          !ALT: To avoid waste, we should not write checkpoint files
          ! when History just wrote the collection,
          ! since the accumulators and the counters have been reset
          if (.not. ESMF_AlarmIsRinging ( list(n)%his_alarm )) then
             if (.not. list(n)%partial) then

                ! save the compname
                call ESMF_CplCompGet (INTSTATE%CCS(n), name=fname_saved, _RC)
                ! add timestamp to filename
                filename = trim(fname_saved) // datestamp
                call ESMF_CplCompSet (INTSTATE%CCS(n), name=filename, _RC)

                call ESMF_CplCompWriteRestart (INTSTATE%CCS(n), &
                     importState=INTSTATE%CIM(n), &
                     exportState=INTSTATE%GIM(n), &
                     clock=CLOCK,           &
                     userRC=STATUS)
                _VERIFY(STATUS)
                ! restore the compname
                call ESMF_CplCompSet (INTSTATE%CCS(n), name=fname_saved, _RC)
             end if
          end if
       end if
    enddo
    _RETURN(ESMF_SUCCESS)
  end subroutine RecordRestart

END SUBMODULE
