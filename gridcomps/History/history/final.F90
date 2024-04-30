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
  SUBMODULE (MAPL_HistoryGridCompMod) final_smod
!
! !USES:
!
  CONTAINS

!=====================================================================
!>
! Finalize the `MAPL_HistoryGridComp` component.
!
  MODULE SUBROUTINE Finalize ( gc, import, export, clock, rc )

   intrinsic :: size

    type(ESMF_GridComp), intent(inout)    :: gc     !! composite gridded component
    type(ESMF_State),       intent(inout) :: import !! import state
    type(ESMF_State),       intent(  out) :: export !! export state
    type(ESMF_Clock),       intent(inout) :: clock  !! the clock

    integer, intent(out), OPTIONAL        :: rc     ! Error code:
                                                     ! = 0 all is well
                                                     ! otherwise, error

    integer                         :: status
    type(HistoryCollection), pointer     :: list(:)
    type(HISTORY_wrap)              :: wrap
    type (HISTORY_STATE), pointer   :: IntState
    integer                         :: nlist, n
    type (MAPL_MetaComp), pointer :: GENSTATE


! Begin...

    call MAPL_GetObjectFromGC ( gc, GENSTATE, _RC)

! Retrieve the pointer to the state

    call ESMF_GridCompGetInternalState(gc, wrap, status)
    IntState => wrap%ptr
    list => IntState%list
    nlist = size(list)

! Close UNITs of GEOSgcm History Data
! -----------------------------------

   do n=1,nlist
      deallocate(list(n)%r4, list(n)%r8, list(n)%r8_to_r4)
      if (list(n)%disabled) cycle
      IF (list(n)%format == 'CFIO') then
         if( MAPL_CFIOIsCreated(list(n)%mcfio) ) then
            CALL MAPL_CFIOdestroy (list(n)%mcfio, _RC)
         end if
      ELSE
         if( list(n)%unit.ne.0 ) call FREE_FILE( list(n)%unit )
      END if
      if(list(n)%monthly) then
         !ALT need some logic if alarm if not ringing
         if (.not. ESMF_AlarmIsRinging ( list(n)%his_alarm )) then
            if (.not. list(n)%partial) then
               call ESMF_CplCompWriteRestart (INTSTATE%CCS(n), &
                    importState=INTSTATE%CIM(n), &
                    exportState=INTSTATE%GIM(n), &
                    clock=CLOCK,           &
                    userRC=STATUS)
               _VERIFY(STATUS)
            end if
         end if
      end if
   enddo

#if 0
   do n=1,nlist
      IF (IntState%average(n)) then
         call MAPL_StateDestroy(IntState%gim(n), _RC)
         call MAPL_StateDestroy(IntState%cim(n), _RC)
      end IF
   enddo
#endif


    call  MAPL_GenericFinalize ( GC, IMPORT, EXPORT, CLOCK, _RC )


    _RETURN(ESMF_SUCCESS)
  end subroutine Finalize

END SUBMODULE

