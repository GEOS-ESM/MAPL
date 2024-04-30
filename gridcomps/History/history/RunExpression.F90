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
  SUBMODULE (MAPL_HistoryGridCompMod) RunExpression
!
! !USES:
!
  implicit none

contains


  MODULE SUBROUTINE MAPL_RunExpression(state,fields,tmpfields,rewrite,nfield,rc)

  type (ESMF_State),  intent(in)    :: state
  character(len=*), intent(in):: fields(:,:),tmpfields(:)
  logical, intent(inout) :: rewrite(:)
  integer, intent(in):: nfield
  integer, optional, intent(out) :: rc

! Local variables:
  character(len=ESMF_MAXSTR)     :: fname,fexpr
  integer:: m,STATUS
  type(ESMF_Field) :: field

  do m=1,nfield
     if (rewrite(m)) then
        fname = trim(fields(3,m))
        call MAPL_StateGet(state,fname,field,force_field=.true.,_RC)
        fexpr = tmpfields(m)
        call MAPL_StateEval(state,fexpr,field,_RC)
     end if
  enddo

  _RETURN(ESMF_SUCCESS)

  end subroutine MAPL_RunExpression

END SUBMODULE
